//! Den Style Editor
//!
//! Janela separada que mostra todas as classes SCSS com controles visuais
//! (sliders, color pickers, dropdowns). Quando o dev muda um valor, faz
//! rewrite cirúrgico no .scss. O cargo-watch detecta e recompila o app.
//! Delay total: ~300ms debounce + ~1-2s recompilação.

use eframe::egui;
use std::time::Instant;

/// Path do pacote den_app em compile time — baked no binário.
const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");

mod model {
    use std::path::PathBuf;
    use std::time::Duration;

    pub const WRITE_DELAY: Duration = Duration::from_millis(300);
    pub const SCAN_INTERVAL: Duration = Duration::from_secs(1);

    pub struct ScssFile {
        pub path: PathBuf,
        pub raw_content: String,
        pub classes: Vec<EditableClass>,
        pub dirty: bool,
    }

    pub struct EditableClass {
        pub name: String,
        #[allow(dead_code)] // reservado pra hover-aware styling futuro
        pub is_hover: bool,
        pub properties: Vec<EditableProperty>,
    }

    pub struct EditableProperty {
        pub name: String,
        pub value: PropertyValue,
        /// Byte offset do início do valor no raw_content.
        pub raw_offset: usize,
        /// Comprimento em bytes do valor original (incluindo espaços antes do `;`).
        pub raw_len: usize,
    }

    #[derive(Clone)]
    pub enum PropertyValue {
        Color { r: u8, g: u8, b: u8 },
        Size { value: f32, min: f32, max: f32, suffix: String },
        Enum { current: String, options: Vec<String> },
        Border { width: f32, color: (u8, u8, u8) },
        Raw(String),
    }

    impl PropertyValue {
        pub fn to_scss_string(&self) -> String {
            match self {
                Self::Color { r, g, b } => format!("#{r:02x}{g:02x}{b:02x}"),
                Self::Size { value, suffix, .. } => {
                    let n = if *value == value.floor() {
                        format!("{}", *value as i32)
                    } else {
                        format!("{value:.1}")
                    };
                    if suffix.is_empty() || suffix == "px" { n } else { format!("{n}{suffix}") }
                }
                Self::Enum { current, .. } => current.clone(),
                Self::Border { width, color } => {
                    let w = if *width == width.floor() {
                        format!("{}", *width as i32)
                    } else {
                        format!("{width:.1}")
                    };
                    format!("{w}px solid #{:02x}{:02x}{:02x}", color.0, color.1, color.2)
                }
                Self::Raw(text) => text.clone(),
            }
        }
    }
}

mod ui {
    use eframe::egui;
    use super::model::*;

    // ============================================================================
    // UI — controles por tipo de propriedade
    // ============================================================================

    pub(super) fn render_property(ui: &mut egui::Ui, prop: &mut EditableProperty) -> bool {
        let mut changed = false;

        ui.horizontal(|ui| {
            ui.add_sized(
                [96.0, 16.0],
                egui::Label::new(
                    egui::RichText::new(&prop.name)
                        .size(11.0)
                        .monospace()
                        .color(egui::Color32::from_rgb(150, 150, 150)),
                ),
            );

            match &mut prop.value {
                PropertyValue::Color { r, g, b } => {
                    let mut color = egui::Color32::from_rgb(*r, *g, *b);
                    if egui::color_picker::color_edit_button_srgba(
                        ui,
                        &mut color,
                        egui::color_picker::Alpha::Opaque,
                    )
                    .changed()
                    {
                        *r = color.r();
                        *g = color.g();
                        *b = color.b();
                        changed = true;
                    }
                    ui.label(
                        egui::RichText::new(format!("#{:02x}{:02x}{:02x}", r, g, b))
                            .size(11.0)
                            .monospace()
                            .color(egui::Color32::GRAY),
                    );
                }

                PropertyValue::Size { value, min, max, suffix } => {
                    let suffix_str = suffix.clone();
                    if ui
                        .add(
                            egui::Slider::new(value, *min..=*max)
                                .suffix(suffix_str.as_str())
                                .clamping(egui::SliderClamping::Always),
                        )
                        .changed()
                    {
                        changed = true;
                    }
                }

                PropertyValue::Enum { current, options } => {
                    let opts = options.clone();
                    egui::ComboBox::from_id_salt(&prop.name)
                        .selected_text(current.as_str())
                        .show_ui(ui, |ui| {
                            for opt in &opts {
                                if ui.selectable_label(current == opt, opt).clicked() {
                                    *current = opt.clone();
                                    changed = true;
                                }
                            }
                        });
                }

                PropertyValue::Border { width, color } => {
                    if ui
                        .add(egui::Slider::new(width, 0.0..=10.0).suffix("px"))
                        .changed()
                    {
                        changed = true;
                    }
                    let mut c = egui::Color32::from_rgb(color.0, color.1, color.2);
                    if egui::color_picker::color_edit_button_srgba(
                        ui,
                        &mut c,
                        egui::color_picker::Alpha::Opaque,
                    )
                    .changed()
                    {
                        *color = (c.r(), c.g(), c.b());
                        changed = true;
                    }
                }

                PropertyValue::Raw(text) => {
                    if ui.text_edit_singleline(text).changed() {
                        changed = true;
                    }
                }
            }
        });

        changed
    }
}

mod io {
    use std::collections::HashMap;
    use std::path::PathBuf;
    use super::model::*;

    // ============================================================================
    // File I/O
    // ============================================================================

    pub fn find_scss_files(dir: &std::path::Path) -> Vec<PathBuf> {
        let mut files = Vec::new();
        let Ok(entries) = std::fs::read_dir(dir) else { return files };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(find_scss_files(&path));
            } else if path.extension().map_or(false, |e| e == "scss") {
                files.push(path);
            }
        }
        files
    }

    /// Rewrite cirúrgico: aplica todos os edits do maior offset pro menor,
    /// preservando formatação, comentários e ordem das outras propriedades.
    pub fn write_back(file: &mut ScssFile) {
        let mut edits: Vec<(usize, usize, String)> = file
            .classes
            .iter()
            .flat_map(|c| c.properties.iter())
            .map(|p| (p.raw_offset, p.raw_len, p.value.to_scss_string()))
            .collect();

        // Decrescente por offset — garante que edits anteriores não invalidam os seguintes
        edits.sort_by(|a, b| b.0.cmp(&a.0));

        let mut content = file.raw_content.clone();
        for (offset, len, new_val) in edits {
            if offset + len > content.len() { continue; }
            if content[offset..offset + len].trim() != new_val {
                content.replace_range(offset..offset + len, &new_val);
            }
        }

        if content == file.raw_content {
            file.dirty = false;
            return;
        }

        match std::fs::write(&file.path, &content) {
            Ok(()) => {
                // Re-parseia pra atualizar os byte offsets com base no conteúdo novo.
                // Sem isso, uma segunda edição usa offsets do conteúdo anterior → panic.
                file.classes = parse_scss_for_editing(&content);
                file.raw_content = content;
                file.dirty = false;
            }
            Err(e) => eprintln!("Den Style Editor: falha ao escrever {}: {e}", file.path.display()),
        }
    }

    // ============================================================================
    // SCSS parser com byte offsets
    // ============================================================================

    pub fn parse_scss_for_editing(content: &str) -> Vec<EditableClass> {
        let vars = collect_scss_vars(content);
        let mut classes = Vec::new();
        let bytes = content.as_bytes();
        let mut pos = 0;

        while pos < bytes.len() {
            skip_ws(bytes, &mut pos);
            if pos >= bytes.len() { break; }

            // Pula variáveis ($var: value;)
            if bytes[pos] == b'$' {
                while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'\n' { pos += 1; }
                if pos < bytes.len() { pos += 1; }
                continue;
            }

            // Pula comentários // e /* */
            if bytes[pos] == b'/' {
                if pos + 1 < bytes.len() && bytes[pos + 1] == b'/' {
                    while pos < bytes.len() && bytes[pos] != b'\n' { pos += 1; }
                    continue;
                }
                if pos + 1 < bytes.len() && bytes[pos + 1] == b'*' {
                    pos += 2;
                    while pos + 1 < bytes.len() && !(bytes[pos] == b'*' && bytes[pos + 1] == b'/') {
                        pos += 1;
                    }
                    pos = (pos + 2).min(bytes.len());
                    continue;
                }
            }

            if bytes[pos] != b'.' { pos += 1; continue; }
            pos += 1;

            let class_name = read_ident(bytes, &mut pos);
            if class_name.is_empty() { continue; }

            let is_hover = if pos < bytes.len() && bytes[pos] == b':' {
                pos += 1;
                read_ident(bytes, &mut pos) == "hover"
            } else {
                false
            };

            skip_ws(bytes, &mut pos);
            if pos >= bytes.len() || bytes[pos] != b'{' { continue; }
            pos += 1;

            let mut properties = Vec::new();

            loop {
                skip_ws(bytes, &mut pos);
                if pos >= bytes.len() || bytes[pos] == b'}' {
                    if pos < bytes.len() { pos += 1; }
                    break;
                }

                // Comentários inline
                if bytes[pos] == b'/' && pos + 1 < bytes.len() && bytes[pos + 1] == b'/' {
                    while pos < bytes.len() && bytes[pos] != b'\n' { pos += 1; }
                    continue;
                }

                let prop_name = read_css_ident(bytes, &mut pos);
                if prop_name.is_empty() { pos += 1; continue; }

                skip_ws(bytes, &mut pos);
                if pos >= bytes.len() || bytes[pos] != b':' { continue; }
                pos += 1;
                skip_ws(bytes, &mut pos);

                let value_start = pos;
                while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'}' { pos += 1; }
                let value_end = pos;
                if pos < bytes.len() && bytes[pos] == b';' { pos += 1; }

                let raw_value = content[value_start..value_end].trim();

                // Resolve variáveis pra classificação (exibe valor real no controle)
                let resolved = resolve_vars(raw_value, &vars);
                let pv = classify_value(&prop_name, &resolved);

                properties.push(EditableProperty {
                    name: prop_name,
                    value: pv,
                    raw_offset: value_start,
                    raw_len: value_end - value_start,
                });
            }

            if !properties.is_empty() {
                classes.push(EditableClass {
                    name: if is_hover { format!("{class_name}:hover") } else { class_name },
                    is_hover,
                    properties,
                });
            }
        }
        classes
    }

    fn classify_value(prop: &str, raw: &str) -> PropertyValue {
        match prop {
            "color" | "background" => parse_hex(raw)
                .map(|(r, g, b)| PropertyValue::Color { r, g, b })
                .unwrap_or_else(|| PropertyValue::Raw(raw.to_string())),

            "font-size" => PropertyValue::Size {
                value: parse_num(raw).unwrap_or(16.0),
                min: 6.0, max: 72.0, suffix: String::new(),
            },
            "padding" | "margin" => PropertyValue::Size {
                value: parse_num(raw).unwrap_or(0.0),
                min: 0.0, max: 64.0, suffix: String::new(),
            },
            "border-radius" => PropertyValue::Size {
                value: parse_num(raw).unwrap_or(0.0),
                min: 0.0, max: 32.0, suffix: String::new(),
            },
            "width" if raw.ends_with('%') => PropertyValue::Size {
                value: raw.trim_end_matches('%').parse().unwrap_or(100.0),
                min: 0.0, max: 100.0, suffix: "%".to_string(),
            },
            "width" => PropertyValue::Size {
                value: parse_num(raw).unwrap_or(100.0),
                min: 0.0, max: 800.0, suffix: String::new(),
            },
            "display" => PropertyValue::Enum {
                current: raw.to_string(),
                options: vec!["block".to_string(), "flex".to_string()],
            },
            "cursor" => PropertyValue::Enum {
                current: raw.to_string(),
                options: vec!["default".to_string(), "pointer".to_string()],
            },
            "border" => {
                let parts: Vec<&str> = raw.split_whitespace().collect();
                if parts.len() >= 3 {
                    let width = parse_num(parts[0]).unwrap_or(1.0);
                    let color = parse_hex(parts[2]).unwrap_or((0, 0, 0));
                    PropertyValue::Border { width, color }
                } else {
                    PropertyValue::Raw(raw.to_string())
                }
            }
            _ => PropertyValue::Raw(raw.to_string()),
        }
    }

    // DUPLICAÇÃO: lógica idêntica a parse/scss.rs. Extrair pra den_core quando criado. Ver PENDING.md.
    fn collect_scss_vars(content: &str) -> HashMap<String, String> {
        let mut vars = HashMap::new();
        for line in content.lines() {
            let t = line.trim();
            if let Some(rest) = t.strip_prefix('$') {
                if let Some(colon) = rest.find(':') {
                    let name = rest[..colon].trim().to_string();
                    let val = rest[colon + 1..].trim().trim_end_matches(';').trim().to_string();
                    if !name.is_empty() && !val.is_empty() {
                        vars.insert(name, val);
                    }
                }
            }
        }
        vars
    }

    fn resolve_vars(value: &str, vars: &HashMap<String, String>) -> String {
        if !value.contains('$') { return value.to_string(); }
        let mut result = value.to_string();
        for (name, val) in vars {
            result = result.replace(&format!("${name}"), val);
        }
        result
    }

    fn parse_num(s: &str) -> Option<f32> {
        s.trim_end_matches("px").parse::<f32>().ok()
    }

    fn parse_hex(s: &str) -> Option<(u8, u8, u8)> {
        let hex = s.trim_start_matches('#');
        let exp = if hex.len() == 3 {
            hex.chars().flat_map(|c| [c, c]).collect::<String>()
        } else {
            hex.to_string()
        };
        if exp.len() < 6 { return None; }
        let r = u8::from_str_radix(&exp[0..2], 16).ok()?;
        let g = u8::from_str_radix(&exp[2..4], 16).ok()?;
        let b = u8::from_str_radix(&exp[4..6], 16).ok()?;
        Some((r, g, b))
    }

    fn skip_ws(bytes: &[u8], pos: &mut usize) {
        while *pos < bytes.len() && bytes[*pos].is_ascii_whitespace() { *pos += 1; }
    }

    fn read_ident(bytes: &[u8], pos: &mut usize) -> String {
        let start = *pos;
        while *pos < bytes.len()
            && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'_' || bytes[*pos] == b'-')
        {
            *pos += 1;
        }
        std::str::from_utf8(&bytes[start..*pos]).unwrap_or("").to_string()
    }

    fn read_css_ident(bytes: &[u8], pos: &mut usize) -> String {
        let start = *pos;
        while *pos < bytes.len()
            && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'-' || bytes[*pos] == b'_')
        {
            *pos += 1;
        }
        std::str::from_utf8(&bytes[start..*pos]).unwrap_or("").to_string()
    }
}

use model::*;
use io::*;

// ============================================================================
// Data model
// ============================================================================

struct StyleEditor {
    files: Vec<ScssFile>,
    last_change: Option<Instant>,
    last_scan: Instant,
}

// ============================================================================
// Lifecycle
// ============================================================================

impl StyleEditor {
    fn new() -> Self {
        let pages_dir = std::path::Path::new(MANIFEST_DIR).join("src/pages");
        let files = find_scss_files(&pages_dir)
            .into_iter()
            .filter_map(|path| {
                let raw = std::fs::read_to_string(&path).ok()?;
                let classes = io::parse_scss_for_editing(&raw);
                Some(ScssFile { path, raw_content: raw, classes, dirty: false })
            })
            .collect();

        Self { files, last_change: None, last_scan: Instant::now() }
    }
}

impl eframe::App for StyleEditor {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Ctrl+S: save imediato
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::S)) {
            for file in &mut self.files {
                if file.dirty { write_back(file); }
            }
            self.last_change = None;
        }

        // Ctrl+Z: descarta mudanças pendentes (reload do disco)
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Z)) {
            for file in &mut self.files {
                if let Ok(content) = std::fs::read_to_string(&file.path) {
                    file.classes = io::parse_scss_for_editing(&content);
                    file.raw_content = content;
                    file.dirty = false;
                }
            }
            self.last_change = None;
        }

        // Debounce: escreve 300ms após última mudança
        if let Some(last) = self.last_change {
            if last.elapsed() >= WRITE_DELAY {
                for file in &mut self.files {
                    if file.dirty { write_back(file); }
                }
                self.last_change = None;
            } else {
                ctx.request_repaint_after(WRITE_DELAY);
            }
        }

        // File watch: re-parseia se .scss mudou externamente
        if self.last_scan.elapsed() >= SCAN_INTERVAL {
            for file in &mut self.files {
                if !file.dirty {
                    if let Ok(content) = std::fs::read_to_string(&file.path) {
                        if content != file.raw_content {
                            file.classes = io::parse_scss_for_editing(&content);
                            file.raw_content = content;
                        }
                    }
                }
            }
            self.last_scan = Instant::now();
        }

        // ── UI ───────────────────────────────────────────────────────────
        egui::CentralPanel::default().show(ctx, |ui| {
            let has_dirty = self.files.iter().any(|f| f.dirty);

            ui.horizontal(|ui| {
                ui.heading("Den Style Editor");
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if has_dirty {
                        ui.label(
                            egui::RichText::new("● unsaved")
                                .color(egui::Color32::from_rgb(230, 100, 80))
                                .size(12.0),
                        );
                    } else {
                        ui.label(
                            egui::RichText::new("✓ saved")
                                .color(egui::Color32::from_rgb(100, 200, 100))
                                .size(12.0),
                        );
                    }
                });
            });
            ui.label(
                egui::RichText::new("Ctrl+S: salvar  |  Ctrl+Z: descartar mudanças")
                    .size(10.0)
                    .color(egui::Color32::GRAY),
            );
            ui.separator();

            let mut any_changed = false;

            egui::ScrollArea::vertical().show(ui, |ui| {
                for file_idx in 0..self.files.len() {
                    let file_name = self.files[file_idx]
                        .path
                        .file_name()
                        .map(|n| n.to_string_lossy().to_string())
                        .unwrap_or_default();

                    ui.label(
                        egui::RichText::new(format!("📄 {file_name}"))
                            .size(11.0)
                            .color(egui::Color32::GRAY),
                    );
                    ui.add_space(4.0);

                    for class_idx in 0..self.files[file_idx].classes.len() {
                        let class_name =
                            self.files[file_idx].classes[class_idx].name.clone();
                        let n_props =
                            self.files[file_idx].classes[class_idx].properties.len();

                        let resp = egui::CollapsingHeader::new(
                            egui::RichText::new(&class_name).monospace().size(13.0),
                        )
                        .default_open(n_props <= 3)
                        .show(ui, |ui| {
                            let mut class_changed = false;
                            for prop_idx in
                                0..self.files[file_idx].classes[class_idx].properties.len()
                            {
                                if ui::render_property(
                                    ui,
                                    &mut self.files[file_idx].classes[class_idx]
                                        .properties[prop_idx],
                                ) {
                                    class_changed = true;
                                }
                            }
                            class_changed
                        });

                        if resp.body_returned.unwrap_or(false) {
                            self.files[file_idx].dirty = true;
                            any_changed = true;
                        }
                    }
                    ui.add_space(12.0);
                }
            });

            if any_changed {
                self.last_change = Some(Instant::now());
            }
        });
    }
}

fn main() -> eframe::Result {
    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_title("Den Style Editor")
            .with_inner_size([440.0, 720.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Den Style Editor",
        options,
        Box::new(|_cc| Ok(Box::new(StyleEditor::new()))),
    )
}
