use crate::AppRoute;
use crate::models::CatFact;
use den_layout::{DenGhostService, DenRouteState, DenRouter};
use eframe::egui;

pub struct HomePage {
    pub name: String,
    pub message: String,
    pub active: bool,
    pub count: u32,
    pub tags: Vec<String>,
    /// Demo do `GhostService`: começa em loading com mock (`#[ghost(...)]`),
    /// vira o fato real depois que o request HTTP a `catfact.ninja` completar.
    pub cat: DenGhostService<CatFact>,
}

impl Default for HomePage {
    fn default() -> Self {
        Self {
            name: "Vini".to_string(),
            message: String::new(),
            active: true,
            count: 0,
            tags: vec![
                "Rust".to_string(),
                "egui".to_string(),
                "Den".to_string(),
                "SCSS".to_string(),
                "Proc Macros".to_string(),
            ],
            cat: DenGhostService::new(),
        }
    }
}

impl HomePage {
    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
        __den_route_state: &mut DenRouteState,
    ) {
        // Polling do ghost: se o fetch completou, o template já vai ver o valor real
        // neste mesmo frame. Sem `tick()` o `loading` ficaria true pra sempre.
        self.cat.tick();
        den_macros::den_template!("pages/home/home", self);
    }

    fn toggle_status(&mut self) {
        self.active = !self.active;
    }

    fn increment_count(&mut self) {
        self.count += 1;
    }

    fn reset(&mut self) {
        self.active = true;
        self.count = 0;
        self.message.clear();
    }

    /// Dispara o request HTTP a `catfact.ninja` em background.
    /// O `DenGhostService::fetch` roda em thread separada — UI nunca bloqueia.
    /// Quando vier resposta, o próximo `tick()` (chamado em `render`) substitui
    /// o ghost pelo fato real.
    fn fetch_cat(&mut self) {
        self.cat.fetch(|| match http_fetch_cat() {
            Ok(c) => c,
            Err(err) => CatFact {
                fact: format!("(falha no fetch: {err})"),
                length: 0,
            },
        });
    }
}

/// Faz GET em `https://catfact.ninja/fact` e parseia o JSON `{"fact":"...","length":N}`.
///
/// Parsing manual via `find()`/`split()` pra evitar dep de `serde_json` só
/// pra um campo. Se a API mudar de schema, o fallback retorna texto bruto.
fn http_fetch_cat() -> Result<CatFact, String> {
    let body = ureq::get("https://catfact.ninja/fact")
        .timeout(std::time::Duration::from_secs(10))
        .call()
        .map_err(|e| e.to_string())?
        .into_string()
        .map_err(|e| e.to_string())?;

    let fact = extract_string_field(&body, "fact").unwrap_or_else(|| body.clone());
    let length = extract_number_field(&body, "length").unwrap_or(0) as u32;

    Ok(CatFact { fact, length })
}

/// Extrai `"key":"value"` do JSON. Tolerante a espaços; não decoda escapes.
/// Se a string contiver `\"` ou `\n`, eles passam literais — aceitável pro demo.
fn extract_string_field(json: &str, key: &str) -> Option<String> {
    let needle = format!("\"{key}\"");
    let idx = json.find(&needle)?;
    let after_key = &json[idx + needle.len()..];
    let colon = after_key.find(':')?;
    let after_colon = &after_key[colon + 1..].trim_start();
    let after_quote = after_colon.strip_prefix('"')?;
    let end = after_quote.find('"')?;
    Some(after_quote[..end].to_string())
}

/// Extrai `"key": N` do JSON (apenas inteiros simples, sem float ou exponencial).
fn extract_number_field(json: &str, key: &str) -> Option<u64> {
    let needle = format!("\"{key}\"");
    let idx = json.find(&needle)?;
    let after_key = &json[idx + needle.len()..];
    let colon = after_key.find(':')?;
    let after_colon = after_key[colon + 1..].trim_start();
    let digits: String = after_colon.chars().take_while(|c| c.is_ascii_digit()).collect();
    digits.parse().ok()
}
