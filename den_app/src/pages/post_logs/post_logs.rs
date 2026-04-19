use crate::AppRoute;
use crate::pages::nodes::{NodeConfig, read_and_parse};
use den_layout::{DenRouteState, DenRouter};
use eframe::egui;
use std::sync::{Arc, Mutex};

/// Pipeline fixa de geração de imagem. Ordem importa — cada node assume que
/// o anterior já rodou e deixou os artefatos em `output/` (que o próximo deve
/// ler de `input/` — convenção dos CLIs ndnm).
const PIPELINE_PATHS: &[&str] = &[
    "nodes/load-checkpoint/node.toml",
    "nodes/clip-text-encode/node.toml",
    "nodes/latent-image/node.toml",
    "nodes/ksampler/node.toml",
    "nodes/decode/node.toml",
];

/// Arquivo que o clip-text-encode consome como prompt. Carregamos dele no
/// boot pra que o input da tela reflita o estado atual em disco, e
/// sobrescrevemos antes de cada `generate()`.
const PROMPT_PATH: &str = "nodes/clip-text-encode/input/prompt.txt";

/// Wiring entre steps: cada entrada é disparada DEPOIS que o step de índice
/// `after_step` completa com sucesso. Copia `src` → `dst` pra popular o
/// `input/` do próximo node com o artefato recém-produzido.
///
/// Sem isso, cada CLI roda com o conteúdo velho do seu `input/` e a mudança
/// de prompt/seed nunca chega até o decode. Os pesos do checkpoint (que não
/// mudam entre runs) ficam de fora — só arquivos-ponte leves.
struct PipelineWire {
    after_step: usize,
    src: &'static str,
    dst: &'static str,
    label: &'static str,
}

const WIRES: &[PipelineWire] = &[
    PipelineWire {
        after_step: 1,
        src: "nodes/clip-text-encode/output/conditioning.safetensors",
        dst: "nodes/ksampler/input/clip-text-encode/conditioning.safetensors",
        label: "conditioning → ksampler",
    },
    PipelineWire {
        after_step: 2,
        src: "nodes/latent-image/output/latent.safetensors",
        dst: "nodes/ksampler/input/latent-image/latent.safetensors",
        label: "latent → ksampler",
    },
    PipelineWire {
        after_step: 3,
        src: "nodes/ksampler/output/latent_denoised.safetensors",
        dst: "nodes/decode/input/latent_denoised.safetensors",
        label: "latent_denoised → decode",
    },
];

/// Estado de um passo individual da pipeline. `status` controla o estilo no
/// template (`step-pending`, `step-running`, `step-ok`, `step-error`).
pub struct PipelineStep {
    pub config: NodeConfig,
    pub status: String,
    pub elapsed_ms: u64,
}

impl PipelineStep {
    fn pending(config: NodeConfig) -> Self {
        Self {
            config,
            status: "pending".to_string(),
            elapsed_ms: 0,
        }
    }
}

pub struct LogEntry {
    pub kind: String,
    pub time: String,
    pub title: String,
    pub detail: String,
}

/// Mensagem das threads worker pro main thread. Mantida intencionalmente
/// distinta da `InboxMsg` de `nodes.rs` — a pipeline trackeia status por
/// índice de step, o NodesPage trackeia barra de progresso de um único run.
enum InboxMsg {
    Log(LogEntry),
    StepStarted { idx: usize },
    StepFinished { idx: usize, status: String, elapsed_ms: u64 },
    Done,
}

pub struct PostLogsPage {
    pub initialized: bool,
    pub steps: Vec<PipelineStep>,
    pub running: bool,
    pub logs: Vec<LogEntry>,
    /// Prompt de texto pro clip-text-encode. Bindado ao input da UI. No
    /// boot vem do arquivo em disco; antes de cada `generate()` a página
    /// sobrescreve o arquivo com esse valor.
    pub prompt: String,
    /// Caminho do PNG da última geração bem-sucedida (lido das mensagens do
    /// save-image). Vazio se nunca rodou ou se a última run falhou.
    pub last_image: String,
    inbox: Arc<Mutex<Vec<InboxMsg>>>,
}

impl Default for PostLogsPage {
    fn default() -> Self {
        Self {
            initialized: false,
            steps: Vec::new(),
            running: false,
            logs: Vec::new(),
            prompt: String::new(),
            last_image: String::new(),
            inbox: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl PostLogsPage {
    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
        __den_route_state: &mut DenRouteState,
    ) {
        if !self.initialized {
            self.load_pipeline();
            self.initialized = true;
        }
        self.drain_inbox();
        den_macros::den_template!("pages/post_logs/post_logs", self);
    }

    /// Lê e parseia cada TOML da pipeline. Falhas viram log de erro mas não
    /// bloqueiam o resto — a UI mostra os steps que carregaram OK.
    fn load_pipeline(&mut self) {
        self.push_log("info", "Carregando pipeline", &format!("{} steps", PIPELINE_PATHS.len()));
        for path in PIPELINE_PATHS {
            match read_and_parse(path) {
                Ok(cfg) => {
                    let port = cfg.port_str();
                    self.push_log("info", "Step carregado", &format!("{} → {}", cfg.id, port));
                    self.steps.push(PipelineStep::pending(cfg));
                }
                Err(e) => self.push_log("error", &format!("Falha ao ler {path}"), &e),
            }
        }
        // Hidrata o input da UI com o prompt atual em disco, se existir.
        // Permite editar-pra-mudar em vez de digitar do zero todo boot.
        match std::fs::read_to_string(PROMPT_PATH) {
            Ok(text) => {
                self.prompt = text.trim_end_matches('\n').to_string();
                self.push_log("info", "Prompt carregado", PROMPT_PATH);
            }
            Err(_) => {
                self.push_log(
                    "info",
                    "Prompt vazio",
                    &format!("{PROMPT_PATH} não existe ainda"),
                );
            }
        }
    }

    fn drain_inbox(&mut self) {
        let Ok(mut inbox) = self.inbox.try_lock() else {
            return;
        };
        if inbox.is_empty() {
            return;
        }
        for msg in inbox.drain(..) {
            match msg {
                InboxMsg::Log(entry) => {
                    if entry.kind == "image" {
                        self.last_image = entry.detail.clone();
                    }
                    self.logs.insert(0, entry);
                }
                InboxMsg::StepStarted { idx } => {
                    if let Some(step) = self.steps.get_mut(idx) {
                        step.status = "running".to_string();
                    }
                }
                InboxMsg::StepFinished { idx, status, elapsed_ms } => {
                    if let Some(step) = self.steps.get_mut(idx) {
                        step.status = status;
                        step.elapsed_ms = elapsed_ms;
                    }
                }
                InboxMsg::Done => {
                    self.running = false;
                }
            }
        }
    }

    fn generate(&mut self) {
        if self.running || self.steps.is_empty() {
            return;
        }
        if self.prompt.trim().is_empty() {
            self.push_log(
                "error",
                "Prompt vazio",
                "Digite algo no campo \"Prompt\" antes de gerar",
            );
            return;
        }
        // Escreve o prompt antes de spawnar. Se falhar, aborta sem rodar —
        // prevenir pipeline rodando com prompt antigo é mais útil que
        // silenciosamente usar o que estava em disco.
        if let Err(e) = std::fs::write(PROMPT_PATH, format!("{}\n", self.prompt.trim())) {
            self.push_log("error", "Falha ao escrever prompt", &e.to_string());
            return;
        }
        self.push_log(
            "info",
            "Prompt salvo",
            &format!("{} → {}", PROMPT_PATH, self.prompt.trim()),
        );
        self.push_log("info", "Gerando imagem", "Iniciando pipeline");
        self.running = true;
        self.last_image.clear();
        for step in &mut self.steps {
            step.status = "pending".to_string();
            step.elapsed_ms = 0;
        }
        let configs: Vec<NodeConfig> = self.steps.iter().map(|s| s.config.clone()).collect();
        let inbox = Arc::clone(&self.inbox);
        std::thread::spawn(move || run_pipeline(configs, inbox));
    }

    fn reset_pipeline(&mut self) {
        if self.running {
            return;
        }
        self.steps.clear();
        self.last_image.clear();
        self.initialized = false;
        self.push_log("info", "Pipeline resetada", "Recarregando configs");
    }

    /// Abre `self.last_image` no viewer default do sistema via `xdg-open`
    /// (Linux). No Mac seria `open`, no Windows `start` — generalizar depois
    /// se precisar cross-platform.
    fn open_image(&mut self) {
        if self.last_image.is_empty() {
            self.push_log("error", "Sem imagem", "Gere uma imagem primeiro");
            return;
        }
        let path = self.last_image.clone();
        match std::process::Command::new("xdg-open").arg(&path).spawn() {
            Ok(_) => self.push_log("info", "Abrindo no viewer", &path),
            Err(e) => self.push_log("error", "xdg-open falhou", &e.to_string()),
        }
    }

    fn clear_logs(&mut self) {
        self.logs.clear();
    }

    fn push_log(&mut self, kind: &str, title: &str, detail: &str) {
        self.logs.insert(
            0,
            LogEntry {
                kind: kind.to_string(),
                time: now_hhmmss(),
                title: title.to_string(),
                detail: detail.to_string(),
            },
        );
    }
}

/// Loop sequencial: pra cada step, marca running, dispatch (HTTP server ou CLI),
/// marca ok/error, para no primeiro erro. Sempre emite `Done` no fim.
fn run_pipeline(steps: Vec<NodeConfig>, inbox: Arc<Mutex<Vec<InboxMsg>>>) {
    for (idx, cfg) in steps.iter().enumerate() {
        push(&inbox, InboxMsg::StepStarted { idx });

        let started = std::time::Instant::now();
        let outcome = if cfg.is_server() {
            run_http_step(idx, steps.len(), cfg, &inbox)
        } else {
            run_cli_step(idx, steps.len(), cfg, &inbox)
        };
        let elapsed_ms = started.elapsed().as_millis() as u64;

        match outcome {
            Ok(()) => {
                push(
                    &inbox,
                    InboxMsg::StepFinished {
                        idx,
                        status: "ok".to_string(),
                        elapsed_ms,
                    },
                );
                // Transferência de artefatos pro próximo node. Se falhar
                // (source ausente, permissão, etc.), aborta a pipeline —
                // step seguinte rodaria com input velho e produziria resultado
                // enganoso.
                if !run_wires(idx, &inbox) {
                    push(&inbox, InboxMsg::Done);
                    return;
                }
            }
            Err(()) => {
                push(
                    &inbox,
                    InboxMsg::StepFinished {
                        idx,
                        status: "error".to_string(),
                        elapsed_ms,
                    },
                );
                push(&inbox, InboxMsg::Done);
                return;
            }
        }
    }

    push(
        &inbox,
        InboxMsg::Log(LogEntry {
            kind: "info".to_string(),
            time: now_hhmmss(),
            title: "Pipeline concluída".to_string(),
            detail: format!("{} steps OK", steps.len()),
        }),
    );
    push(&inbox, InboxMsg::Done);
}

/// Copia os arquivos-ponte registrados em `WIRES` pra `after_step == idx`.
/// Retorna false se qualquer cópia falhar — caller aborta a pipeline.
fn run_wires(idx: usize, inbox: &Arc<Mutex<Vec<InboxMsg>>>) -> bool {
    for wire in WIRES.iter().filter(|w| w.after_step == idx) {
        match std::fs::copy(wire.src, wire.dst) {
            Ok(bytes) => push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "info".to_string(),
                    time: now_hhmmss(),
                    title: format!("Wire: {}", wire.label),
                    detail: format!("{} → {} ({} bytes)", wire.src, wire.dst, bytes),
                }),
            ),
            Err(e) => {
                push(
                    inbox,
                    InboxMsg::Log(LogEntry {
                        kind: "error".to_string(),
                        time: now_hhmmss(),
                        title: format!("Wire falhou: {}", wire.label),
                        detail: format!("{} → {}: {}", wire.src, wire.dst, e),
                    }),
                );
                return false;
            }
        }
    }
    true
}

fn run_http_step(idx: usize, total: usize, cfg: &NodeConfig, inbox: &Arc<Mutex<Vec<InboxMsg>>>) -> Result<(), ()> {
    let url = cfg.endpoint_run();
    push(
        inbox,
        InboxMsg::Log(LogEntry {
            kind: "request".to_string(),
            time: now_hhmmss(),
            title: format!("[{}/{}] {} (server)", idx + 1, total, cfg.name),
            detail: format!("GET {url}"),
        }),
    );

    let res = ureq::get(&url)
        .timeout(std::time::Duration::from_secs(600))
        .call();

    match res {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.into_string().unwrap_or_else(|e| format!("(read err: {e})"));
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "response".to_string(),
                    time: now_hhmmss(),
                    title: format!("[{}/{}] HTTP {status}", idx + 1, total),
                    detail: body.clone(),
                }),
            );
            check_for_image_in_text(&body, inbox);
            Ok(())
        }
        Err(ureq::Error::Status(code, resp)) => {
            let body = resp.into_string().unwrap_or_else(|e| format!("(read err: {e})"));
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: format!("[{}/{}] HTTP {code}", idx + 1, total),
                    detail: body,
                }),
            );
            Err(())
        }
        Err(err) => {
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: format!("[{}/{}] Falha de transporte", idx + 1, total),
                    detail: format!("{err}\n\nServidor em {url} está rodando?"),
                }),
            );
            Err(())
        }
    }
}

fn run_cli_step(idx: usize, total: usize, cfg: &NodeConfig, inbox: &Arc<Mutex<Vec<InboxMsg>>>) -> Result<(), ()> {
    push(
        inbox,
        InboxMsg::Log(LogEntry {
            kind: "request".to_string(),
            time: now_hhmmss(),
            title: format!("[{}/{}] {} (CLI)", idx + 1, total, cfg.name),
            detail: format!("$ {} (cwd: {})", cfg.command, cfg.dir),
        }),
    );

    let spawn_time = std::time::SystemTime::now();
    let output = match std::process::Command::new(&cfg.command)
        .current_dir(&cfg.dir)
        .output()
    {
        Ok(o) => o,
        Err(e) => {
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: format!("[{}/{}] Falha ao spawnar", idx + 1, total),
                    detail: format!("{e}\n\nBuildou? Tente: cd {} && cargo build --release", cfg.dir),
                }),
            );
            return Err(());
        }
    };

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();

    if !stdout.trim().is_empty() {
        push(
            inbox,
            InboxMsg::Log(LogEntry {
                kind: "info".to_string(),
                time: now_hhmmss(),
                title: format!("[{}/{}] stdout", idx + 1, total),
                detail: stdout.clone(),
            }),
        );
    }
    if !stderr.trim().is_empty() {
        push(
            inbox,
            InboxMsg::Log(LogEntry {
                kind: "info".to_string(),
                time: now_hhmmss(),
                title: format!("[{}/{}] stderr", idx + 1, total),
                detail: stderr.clone(),
            }),
        );
    }

    if !output.status.success() {
        let code = output.status.code().unwrap_or(-1);
        push(
            inbox,
            InboxMsg::Log(LogEntry {
                kind: "error".to_string(),
                time: now_hhmmss(),
                title: format!("[{}/{}] Exit {code}", idx + 1, total),
                detail: stderr,
            }),
        );
        return Err(());
    }

    push(
        inbox,
        InboxMsg::Log(LogEntry {
            kind: "response".to_string(),
            time: now_hhmmss(),
            title: format!("[{}/{}] Exit 0 OK", idx + 1, total),
            detail: format!("{} terminou", cfg.command),
        }),
    );
    // 2 fontes possíveis do path: stdout do CLI (se ele printar), ou varredura
    // da pasta `output/` do node procurando imagens novas desde o spawn.
    check_for_image_in_text(&stdout, inbox);
    check_for_image_in_output(&cfg.dir, spawn_time, inbox);
    Ok(())
}

fn check_for_image_in_text(text: &str, inbox: &Arc<Mutex<Vec<InboxMsg>>>) {
    if let Some(path) = extract_image_path(text) {
        emit_image(&path, inbox);
    }
}

/// Scaneia `<node_dir>/output/` por arquivos de imagem criados/modificados
/// depois de `since`. Emite 1 evento `image` por hit. O CLI não precisa
/// printar nada — a presença do arquivo é a fonte da verdade.
fn check_for_image_in_output(
    node_dir: &str,
    since: std::time::SystemTime,
    inbox: &Arc<Mutex<Vec<InboxMsg>>>,
) {
    let output_dir = std::path::Path::new(node_dir).join("output");
    let Ok(entries) = std::fs::read_dir(&output_dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(ext) = path.extension().and_then(|e| e.to_str()) else {
            continue;
        };
        if !matches!(ext.to_ascii_lowercase().as_str(), "png" | "jpg" | "jpeg" | "webp") {
            continue;
        }
        let Ok(meta) = entry.metadata() else { continue };
        let Ok(mtime) = meta.modified() else { continue };
        if mtime < since {
            continue;
        }
        // Canonicalize pra mostrar path absoluto (melhor pra xdg-open).
        let absolute = std::fs::canonicalize(&path)
            .unwrap_or(path)
            .to_string_lossy()
            .into_owned();
        emit_image(&absolute, inbox);
    }
}

fn emit_image(path: &str, inbox: &Arc<Mutex<Vec<InboxMsg>>>) {
    push(
        inbox,
        InboxMsg::Log(LogEntry {
            kind: "image".to_string(),
            time: now_hhmmss(),
            title: "Imagem gerada".to_string(),
            detail: path.to_string(),
        }),
    );
}

fn push(inbox: &Arc<Mutex<Vec<InboxMsg>>>, msg: InboxMsg) {
    if let Ok(mut buf) = inbox.lock() {
        buf.push(msg);
    }
}

/// Extrai um caminho de imagem do JSON da resposta. Procura por chaves comuns
/// (`image`, `output`, `path`) sem depender de schema fixo — qualquer node
/// downstream que devolva `"image": "..."` ou `"path": "...png"` é detectado.
fn extract_image_path(body: &str) -> Option<String> {
    for key in &["\"image\"", "\"output\"", "\"path\"", "\"file\""] {
        if let Some(idx) = body.find(key) {
            let after_key = &body[idx + key.len()..];
            let colon = after_key.find(':')?;
            let after_colon = after_key[colon + 1..].trim_start();
            let after_quote = after_colon.strip_prefix('"')?;
            let end = after_quote.find('"')?;
            let val = &after_quote[..end];
            if val.ends_with(".png") || val.ends_with(".jpg") || val.ends_with(".webp") {
                return Some(val.to_string());
            }
        }
    }
    None
}

fn now_hhmmss() -> String {
    match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
        Ok(d) => {
            let secs = d.as_secs();
            let h = (secs / 3600) % 24;
            let m = (secs / 60) % 60;
            let s = secs % 60;
            format!("{h:02}:{m:02}:{s:02}")
        }
        Err(_) => "??:??:??".to_string(),
    }
}
