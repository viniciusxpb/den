use crate::AppRoute;
use den_layout::{DenRouteState, DenRouter};
use eframe::egui;
use std::io::BufRead;
use std::sync::{Arc, Mutex};

/// Diretório raiz onde a UI procura nodes (subpastas com `node.toml`).
/// Mudar aqui afeta a descoberta automática feita em `discover_nodes`.
const NODES_ROOT: &str = "nodes";

/// Subset do `node.toml` que a UI consome. Hand-parse pra evitar dep `toml`
/// só pra ler 5 strings + 1 int — mesmo padrão usado em `home.rs` pro JSON.
///
/// `port = None` indica node CLI (roda como subprocess). `port = Some(n)`
/// indica node servidor HTTP (orquestrador faz GET /run). O modelo é decidido
/// pela presença/ausência de `[runtime] port = N` no toml.
#[derive(Default, Clone)]
pub struct NodeConfig {
    pub id: String,
    pub name: String,
    pub version: String,
    pub category: String,
    pub description: String,
    pub command: String,
    pub port: Option<u16>,
    /// Diretório do node (onde fica `node.toml`). Usado pra resolver
    /// `command` e `input/output/` relativos ao spawn de subprocess.
    pub dir: String,
}

impl NodeConfig {
    pub fn is_server(&self) -> bool {
        self.port.is_some()
    }
    pub fn is_cli(&self) -> bool {
        self.port.is_none()
    }
    pub fn port_str(&self) -> String {
        match self.port {
            Some(p) => format!(":{p}"),
            None => "CLI".to_string(),
        }
    }
    pub fn endpoint_run(&self) -> String {
        match self.port {
            Some(p) => format!("http://127.0.0.1:{p}/run"),
            None => format!("subprocess {}", self.command),
        }
    }
    pub fn endpoint_health(&self) -> String {
        match self.port {
            Some(p) => format!("http://127.0.0.1:{p}/health"),
            None => "n/a (CLI)".to_string(),
        }
    }
}

/// Card de um node descoberto. Mostrado na lista superior; reflete o estado
/// do filesystem no momento do scan (não acompanha mudanças sem `refresh`).
pub struct NodeCard {
    pub path: String,
    pub config: NodeConfig,
}

/// Linha de log: tipo (info/request/response/error), timestamp, título, detalhe.
/// O `kind` vira classe CSS (`kind-info`, `kind-badge-info`, etc.) no template.
pub struct LogEntry {
    pub kind: String,
    pub time: String,
    pub title: String,
    pub detail: String,
}

/// Mensagem que uma thread worker empurra pro main thread via `inbox`.
/// `Log` vira log entry; `Progress` atualiza a barra; `Done` libera o
/// estado `running` sem entrar nos logs.
enum InboxMsg {
    Log(LogEntry),
    Progress { pct: i32, label: String },
    Done,
}

pub struct NodesPage {
    pub nodes: Vec<NodeCard>,
    pub active_path: String,
    pub active: NodeConfig,
    pub running: bool,
    pub initialized: bool,
    pub logs: Vec<LogEntry>,
    /// 0..=100 — % do arquivo atual. Só faz sentido quando `running`.
    pub progress_pct: i32,
    /// `progress_pct` arredondado pra múltiplo de 5 como String — usado pra
    /// montar a classe CSS `pb-0`…`pb-100` no template (Den não suporta
    /// `style="width: Npx"` dinâmico, então discretizamos em classes).
    pub progress_bucket: String,
    /// Texto auxiliar ao lado da barra, ex.: "writing clip_g".
    pub progress_label: String,
    /// Buffer compartilhado com as threads worker. Eventos chegam aqui em
    /// ordem cronológica e são drenados a cada frame antes do render.
    inbox: Arc<Mutex<Vec<InboxMsg>>>,
}

impl Default for NodesPage {
    fn default() -> Self {
        Self {
            nodes: Vec::new(),
            active_path: String::new(),
            active: NodeConfig::default(),
            running: false,
            initialized: false,
            logs: Vec::new(),
            progress_pct: 0,
            progress_bucket: "0".to_string(),
            progress_label: String::new(),
            inbox: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

impl NodesPage {
    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
        __den_route_state: &mut DenRouteState,
    ) {
        if !self.initialized {
            self.refresh_nodes();
            self.initialized = true;
        }
        self.drain_inbox();
        den_macros::den_template!("pages/nodes/nodes", self);
    }

    /// Move eventos das threads worker para o estado local. `Log` empurra
    /// entries; `Progress` atualiza os campos de barra; `Done` libera
    /// `running` e reseta a barra. Responses/errors também liberam `running`
    /// pra casos não-streaming (health check).
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
                    if matches!(entry.kind.as_str(), "response" | "error") {
                        self.running = false;
                        self.progress_pct = 0;
                        self.progress_bucket = "0".to_string();
                        self.progress_label.clear();
                    }
                    self.logs.insert(0, entry);
                }
                InboxMsg::Progress { pct, label } => {
                    let clamped = pct.clamp(0, 100);
                    self.progress_pct = clamped;
                    self.progress_bucket = ((clamped / 5) * 5).to_string();
                    self.progress_label = label;
                }
                InboxMsg::Done => {
                    self.running = false;
                    self.progress_pct = 0;
                    self.progress_bucket = "0".to_string();
                    self.progress_label.clear();
                }
            }
        }
    }

    /// Re-scan do diretório `NODES_ROOT`. Chamado uma vez no primeiro render
    /// e via botão "Atualizar". Pré-seleciona o primeiro node descoberto.
    fn refresh_nodes(&mut self) {
        self.push_log("info", "Escaneando nodes", NODES_ROOT);
        self.nodes.clear();
        match discover_nodes(NODES_ROOT) {
            Ok(found) => {
                if found.is_empty() {
                    self.push_log("info", "Nenhum node encontrado", NODES_ROOT);
                } else {
                    self.push_log(
                        "info",
                        &format!("{} node(s) descoberto(s)", found.len()),
                        &found
                            .iter()
                            .map(|n| n.config.id.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                    );
                    if self.active_path.is_empty() {
                        self.active_path = found[0].path.clone();
                        self.active = found[0].config.clone();
                    }
                }
                self.nodes = found;
            }
            Err(e) => self.push_log("error", "Falha ao escanear", &e),
        }
    }

    fn load_active(&mut self) {
        let path = self.active_path.clone();
        if path.is_empty() {
            self.push_log("error", "Caminho vazio", "Preencha o input de path");
            return;
        }
        self.push_log("info", "Carregando", &path);
        match read_and_parse(&path) {
            Ok(cfg) => {
                self.push_log(
                    "info",
                    "Config OK",
                    &format!("{} v{} → {}", cfg.id, cfg.version, cfg.port_str()),
                );
                self.active = cfg;
            }
            Err(e) => self.push_log("error", "Falha", &e),
        }
    }

    fn check_health(&mut self) {
        if self.active.id.is_empty() {
            self.push_log("error", "Sem node ativo", "Carregue um node primeiro");
            return;
        }
        if self.active.is_cli() {
            self.push_log(
                "info",
                "Health check n/a",
                "Node CLI não tem endpoint /health (só servers)",
            );
            return;
        }
        let url = self.active.endpoint_health();
        self.push_log("request", "GET /health", &url);
        let inbox = Arc::clone(&self.inbox);
        std::thread::spawn(move || {
            let entry = http_get(&url);
            if let Ok(mut buf) = inbox.lock() {
                buf.push(InboxMsg::Log(entry));
            }
        });
    }

    fn run_active(&mut self) {
        if self.active.id.is_empty() {
            self.push_log("error", "Sem node ativo", "Carregue um node primeiro");
            return;
        }
        if self.running {
            return;
        }
        let cfg = self.active.clone();
        self.running = true;
        self.progress_pct = 0;
        self.progress_bucket = "0".to_string();
        self.progress_label.clear();
        let inbox = Arc::clone(&self.inbox);

        if cfg.is_server() {
            let url = cfg.endpoint_run();
            self.push_log("request", "GET /run (SSE)", &url);
            self.push_log("info", "Servidor esperado em", &cfg.command);
            std::thread::spawn(move || {
                stream_sse_run(&url, inbox);
            });
        } else {
            self.push_log(
                "request",
                "Spawn CLI",
                &format!("{} (cwd: {})", cfg.command, cfg.dir),
            );
            std::thread::spawn(move || {
                spawn_cli(&cfg, inbox);
            });
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

/// Itera `root/*/node.toml` e devolve cards parseados. Erros de parse de um
/// node individual viram `info` log mas não interrompem a descoberta.
fn discover_nodes(root: &str) -> Result<Vec<NodeCard>, String> {
    let read_dir = std::fs::read_dir(root).map_err(|e| format!("read_dir({root}): {e}"))?;
    let mut out = Vec::new();
    for entry in read_dir.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let toml_path = path.join("node.toml");
        if !toml_path.exists() {
            continue;
        }
        let toml_str = toml_path.to_string_lossy().to_string();
        if let Ok(cfg) = read_and_parse(&toml_str) {
            out.push(NodeCard {
                path: toml_str,
                config: cfg,
            });
        }
    }
    out.sort_by(|a, b| a.config.id.cmp(&b.config.id));
    Ok(out)
}

pub fn read_and_parse(path: &str) -> Result<NodeConfig, String> {
    let text = std::fs::read_to_string(path).map_err(|e| format!("ler {path}: {e}"))?;
    let mut cfg = parse_node_toml(&text)?;
    cfg.dir = std::path::Path::new(path)
        .parent()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_default();
    Ok(cfg)
}

/// Spawna o binário CLI do node como subprocess no `cfg.dir`. Captura stdout
/// e stderr e empurra como log entries (1 entry por). Done sai pra UI sair
/// do estado `running`.
fn spawn_cli(cfg: &NodeConfig, inbox: Arc<Mutex<Vec<InboxMsg>>>) {
    let started = std::time::Instant::now();
    let mut child = match std::process::Command::new(&cfg.command)
        .current_dir(&cfg.dir)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => {
            push(
                &inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: "Falha ao spawnar".to_string(),
                    detail: format!("{} (cwd: {}): {}", cfg.command, cfg.dir, e),
                }),
            );
            push(&inbox, InboxMsg::Done);
            return;
        }
    };

    let status = child.wait();
    let elapsed_ms = started.elapsed().as_millis();
    let stdout = child
        .stdout
        .take()
        .map(|mut s| {
            let mut buf = String::new();
            use std::io::Read;
            let _ = s.read_to_string(&mut buf);
            buf
        })
        .unwrap_or_default();
    let stderr = child
        .stderr
        .take()
        .map(|mut s| {
            let mut buf = String::new();
            use std::io::Read;
            let _ = s.read_to_string(&mut buf);
            buf
        })
        .unwrap_or_default();

    let exit_code = status.as_ref().ok().and_then(|s| s.code()).unwrap_or(-1);
    let kind = if status.as_ref().map(|s| s.success()).unwrap_or(false) {
        "response"
    } else {
        "error"
    };

    if !stdout.trim().is_empty() {
        push(
            &inbox,
            InboxMsg::Log(LogEntry {
                kind: "info".to_string(),
                time: now_hhmmss(),
                title: "stdout".to_string(),
                detail: stdout,
            }),
        );
    }
    if !stderr.trim().is_empty() {
        push(
            &inbox,
            InboxMsg::Log(LogEntry {
                kind: "info".to_string(),
                time: now_hhmmss(),
                title: "stderr".to_string(),
                detail: stderr,
            }),
        );
    }
    push(
        &inbox,
        InboxMsg::Log(LogEntry {
            kind: kind.to_string(),
            time: now_hhmmss(),
            title: format!("Exit {exit_code} ({elapsed_ms} ms)"),
            detail: format!("{} terminou", cfg.command),
        }),
    );
    push(&inbox, InboxMsg::Done);
}

/// Chamada HTTP blocking simples (usada pro /health). Retorna um único
/// LogEntry com o corpo inteiro.
fn http_get(url: &str) -> LogEntry {
    let started = std::time::Instant::now();
    let res = ureq::get(url)
        .timeout(std::time::Duration::from_secs(10))
        .call();
    let elapsed = started.elapsed().as_millis();
    let time = now_hhmmss();
    match res {
        Ok(resp) => {
            let status = resp.status();
            let body = resp.into_string().unwrap_or_else(|e| format!("(read err: {e})"));
            LogEntry {
                kind: "response".to_string(),
                time,
                title: format!("HTTP {status} ({elapsed} ms)"),
                detail: body,
            }
        }
        Err(ureq::Error::Status(code, resp)) => {
            let body = resp.into_string().unwrap_or_else(|e| format!("(read err: {e})"));
            LogEntry {
                kind: "error".to_string(),
                time,
                title: format!("HTTP {code} ({elapsed} ms)"),
                detail: body,
            }
        }
        Err(err) => LogEntry {
            kind: "error".to_string(),
            time,
            title: format!("Falha de transporte ({elapsed} ms)"),
            detail: err.to_string(),
        },
    }
}

/// Consome o endpoint SSE do `/run` e empurra InboxMsg em tempo real.
/// Parser minimalista de SSE: junta linhas `event:` / `data:` e dispara
/// ao encontrar a linha em branco que delimita o frame. Garante Done
/// no fim (seja sucesso, erro de transporte, ou HTTP != 2xx) pra UI
/// sempre sair do estado `running`.
fn stream_sse_run(url: &str, inbox: Arc<Mutex<Vec<InboxMsg>>>) {
    let started = std::time::Instant::now();
    let resp = match ureq::get(url).set("Accept", "text/event-stream").call() {
        Ok(r) => r,
        Err(ureq::Error::Status(code, r)) => {
            let body = r.into_string().unwrap_or_else(|e| format!("(read err: {e})"));
            push(
                &inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: format!("HTTP {code}"),
                    detail: body,
                }),
            );
            push(&inbox, InboxMsg::Done);
            return;
        }
        Err(err) => {
            push(
                &inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: "Falha ao abrir stream".to_string(),
                    detail: err.to_string(),
                }),
            );
            push(&inbox, InboxMsg::Done);
            return;
        }
    };

    let reader = resp.into_reader();
    let mut buf = std::io::BufReader::new(reader);
    let mut line = String::new();
    let mut cur_event: Option<String> = None;
    let mut cur_data: Option<String> = None;

    loop {
        line.clear();
        match buf.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => {}
            Err(e) => {
                push(
                    &inbox,
                    InboxMsg::Log(LogEntry {
                        kind: "error".to_string(),
                        time: now_hhmmss(),
                        title: "Erro lendo stream".to_string(),
                        detail: e.to_string(),
                    }),
                );
                break;
            }
        }
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.is_empty() {
            if let Some(data) = cur_data.take() {
                dispatch_sse(cur_event.take().as_deref().unwrap_or(""), &data, &inbox);
            } else {
                cur_event = None;
            }
        } else if trimmed.starts_with(':') {
            // comentário SSE (keep-alive), ignora
        } else if let Some(rest) = trimmed.strip_prefix("event:") {
            cur_event = Some(rest.trim().to_string());
        } else if let Some(rest) = trimmed.strip_prefix("data:") {
            let rest = rest.strip_prefix(' ').unwrap_or(rest);
            cur_data = Some(rest.to_string());
        }
    }

    push(
        &inbox,
        InboxMsg::Log(LogEntry {
            kind: "info".to_string(),
            time: now_hhmmss(),
            title: "Stream encerrado".to_string(),
            detail: format!("{} ms", started.elapsed().as_millis()),
        }),
    );
    push(&inbox, InboxMsg::Done);
}

fn push(inbox: &Arc<Mutex<Vec<InboxMsg>>>, msg: InboxMsg) {
    if let Ok(mut buf) = inbox.lock() {
        buf.push(msg);
    }
}

/// Traduz um frame SSE num InboxMsg apropriado.
/// - Envelope (SSE `event:` setado): `started`, `finished`, `error` do server.
/// - Sem envelope (só `data:`): o JSON traz um campo `"event"` vindo do child
///   binary (`progress`, `detected`, `written`, `done`, ...).
fn dispatch_sse(sse_event: &str, data: &str, inbox: &Arc<Mutex<Vec<InboxMsg>>>) {
    let parsed: serde_json::Value = match serde_json::from_str(data) {
        Ok(v) => v,
        Err(_) => {
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "info".to_string(),
                    time: now_hhmmss(),
                    title: "Frame não-JSON".to_string(),
                    detail: data.to_string(),
                }),
            );
            return;
        }
    };
    // Se o server marcou event:, trata-se de envelope
    match sse_event {
        "started" => {
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "info".to_string(),
                    time: now_hhmmss(),
                    title: "Run iniciado".to_string(),
                    detail: data.to_string(),
                }),
            );
            return;
        }
        "finished" => {
            let status = parsed.get("status").and_then(|v| v.as_str()).unwrap_or("?");
            let kind = if status == "ok" { "response" } else { "error" };
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: kind.to_string(),
                    time: now_hhmmss(),
                    title: format!("Run finalizado ({status})"),
                    detail: data.to_string(),
                }),
            );
            return;
        }
        "error" => {
            let msg = parsed
                .get("message")
                .and_then(|v| v.as_str())
                .unwrap_or(data)
                .to_string();
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: "Erro".to_string(),
                    detail: msg,
                }),
            );
            return;
        }
        _ => {}
    }

    // Evento do child binary — chaveado pelo campo "event" dentro do JSON.
    let ev_name = parsed.get("event").and_then(|v| v.as_str()).unwrap_or("");
    match ev_name {
        "progress" => {
            let pct = parsed.get("pct").and_then(|v| v.as_i64()).unwrap_or(0) as i32;
            let component = parsed
                .get("component")
                .and_then(|v| v.as_str())
                .unwrap_or("");
            let file = parsed.get("file").and_then(|v| v.as_str()).unwrap_or("");
            let label = if file.is_empty() {
                component.to_string()
            } else {
                format!("{file} · {component}")
            };
            push(inbox, InboxMsg::Progress { pct, label });
        }
        "done" => {
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "info".to_string(),
                    time: now_hhmmss(),
                    title: "Child done".to_string(),
                    detail: data.to_string(),
                }),
            );
        }
        "error" => {
            let msg = parsed
                .get("message")
                .and_then(|v| v.as_str())
                .unwrap_or(data)
                .to_string();
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "error".to_string(),
                    time: now_hhmmss(),
                    title: "Erro no child".to_string(),
                    detail: msg,
                }),
            );
        }
        other => {
            push(
                inbox,
                InboxMsg::Log(LogEntry {
                    kind: "info".to_string(),
                    time: now_hhmmss(),
                    title: if other.is_empty() {
                        "event".to_string()
                    } else {
                        other.to_string()
                    },
                    detail: data.to_string(),
                }),
            );
        }
    }
}

/// Hand-parser pro subset de TOML que precisamos: chaves top-level e a seção
/// `[runtime]`. Aceita strings entre aspas e inteiros sem aspas; ignora
/// comentários, linhas vazias e qualquer outra seção.
fn parse_node_toml(text: &str) -> Result<NodeConfig, String> {
    let mut cfg = NodeConfig::default();
    let mut section: &str = "";
    for raw in text.lines() {
        let line = match raw.find('#') {
            Some(i) => raw[..i].trim(),
            None => raw.trim(),
        };
        if line.is_empty() {
            continue;
        }
        if let Some(rest) = line.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
            section = match rest.trim() {
                "runtime" => "runtime",
                _ => "",
            };
            continue;
        }
        let Some((key, value)) = line.split_once('=') else {
            continue;
        };
        let key = key.trim();
        let value = value.trim();
        let str_val = value
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .map(|s| s.to_string());

        match (section, key) {
            ("", "id") => cfg.id = str_val.unwrap_or_default(),
            ("", "name") => cfg.name = str_val.unwrap_or_default(),
            ("", "version") => cfg.version = str_val.unwrap_or_default(),
            ("", "category") => cfg.category = str_val.unwrap_or_default(),
            ("", "description") => cfg.description = str_val.unwrap_or_default(),
            ("runtime", "command") => cfg.command = str_val.unwrap_or_default(),
            ("runtime", "port") => {
                let port: u16 = value
                    .parse()
                    .map_err(|e| format!("port inválido `{value}`: {e}"))?;
                cfg.port = Some(port);
            }
            _ => {}
        }
    }
    if cfg.id.is_empty() {
        return Err("toml não contém `id`".to_string());
    }
    if cfg.command.is_empty() {
        return Err("toml não contém `[runtime] command`".to_string());
    }
    Ok(cfg)
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
