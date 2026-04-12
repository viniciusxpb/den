//! Den Preview Generator
//!
//! Escaneia todos os templates HTML+SCSS em src/pages/, encontra elementos com
//! o atributo `dev`, e gera preview/index.html com CSS real e valores fictícios.
//!
//! Uso: cargo run --bin preview
//! Ou via: make dev (integrado ao watch)

use std::fs;
use std::path::{Path, PathBuf};

/// Largura da janela egui em pixels (deve coincidir com `app_config::WINDOW_WIDTH` = 1200).
///
/// Usada pra que `width: 100%` nos componentes do preview resolva relativo ao
/// mesmo espaço disponível que o app nativo enxerga. Atualizar aqui se o
/// tamanho padrão da janela mudar.
#[allow(dead_code)]
const EGUI_WINDOW_WIDTH: u32 = 1200;

fn main() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR")
        .unwrap_or_else(|_| ".".to_string());
    let pages_dir = Path::new(&manifest).join("src/pages");
    let preview_dir = Path::new(&manifest).join("../preview");

    fs::create_dir_all(&preview_dir).ok();

    let pairs = find_template_pairs(&pages_dir);
    if pairs.is_empty() {
        eprintln!("preview: nenhum template encontrado em {}", pages_dir.display());
        return;
    }

    let mut all_css = String::new();
    let mut all_components: Vec<(String, String)> = Vec::new(); // (label, html)

    for (html_path, scss_path) in &pairs {
        let html = fs::read_to_string(html_path).unwrap_or_default();
        let scss = fs::read_to_string(scss_path).unwrap_or_default();

        all_css.push_str(&scss_to_css(&scss));
        all_css.push('\n');

        let label = html_path
            .parent()
            .and_then(|p| p.file_name())
            .map(|n| n.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        for component in extract_dev_components(&html) {
            all_components.push((label.clone(), component));
        }
    }

    if all_components.is_empty() {
        eprintln!("preview: nenhum elemento com atributo `dev` encontrado.");
        eprintln!("         Adicione `dev` a um elemento no HTML, ex: <div dev class=\"foo\">");
        return;
    }

    let output = generate_preview_html(&all_css, &all_components);
    let out_path = preview_dir.join("index.html");
    let already_exists = out_path.exists();
    fs::write(&out_path, &output).expect("Failed to write preview/index.html");
    println!("preview: {} componente(s) → {}", all_components.len(), out_path.display());

    if !already_exists {
        std::process::Command::new("xdg-open")
            .arg(&out_path)
            .spawn()
            .ok();
    }
}

// ============================================================================
// Template discovery
// ============================================================================

fn find_template_pairs(dir: &Path) -> Vec<(PathBuf, PathBuf)> {
    let mut pairs = Vec::new();
    let Ok(entries) = fs::read_dir(dir) else { return pairs };

    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            pairs.extend(find_template_pairs(&path));
        } else if path.extension().map_or(false, |e| e == "html") {
            let scss = path.with_extension("scss");
            if scss.exists() {
                pairs.push((path, scss));
            }
        }
    }
    pairs
}

// ============================================================================
// SCSS → CSS
// ============================================================================

/// Converte SCSS do Den pra CSS válido.
/// - Resolve variáveis `$nome: valor` e substitui referências
/// - Adiciona `px` a valores numéricos sem unidade
/// - Remove as linhas de declaração de variáveis do output
fn scss_to_css(scss: &str) -> String {
    let vars = collect_scss_vars(scss);
    let mut out = String::new();
    for line in scss.lines() {
        let trimmed = line.trim();
        // Pula declarações de variáveis — não são CSS válido
        if trimmed.starts_with('$') {
            continue;
        }
        let resolved = resolve_scss_vars(line, &vars);
        let converted = add_px_to_unitless(&resolved);
        out.push_str(&converted);
        out.push('\n');

        // Compatibilidade egui: `ui.horizontal()` não faz stretch nos filhos e
        // usa item_spacing.x (~8px) entre eles. O CSS padrão de `display: flex`
        // faz align-items:stretch e gap:0, o que não bate com o comportamento egui.
        if converted.trim().trim_end_matches(';').trim() == "display: flex" {
            let indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();
            out.push_str(&format!(
                "{indent}align-items: flex-start; /* egui: filhos têm altura do conteúdo */\n"
            ));
            out.push_str(&format!(
                "{indent}gap: 8px; /* egui item_spacing.x padrão */\n"
            ));
        }
    }
    out
}

// DUPLICAÇÃO: lógica idêntica a parse/scss.rs. Extrair pra den_core quando criado. Ver PENDING.md.
fn collect_scss_vars(scss: &str) -> std::collections::HashMap<String, String> {
    let mut vars = std::collections::HashMap::new();
    for line in scss.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix('$') {
            if let Some(colon) = rest.find(':') {
                let name = rest[..colon].trim().to_string();
                let value = rest[colon + 1..].trim().trim_end_matches(';').trim().to_string();
                if !name.is_empty() && !value.is_empty() {
                    vars.insert(name, value);
                }
            }
        }
    }
    vars
}

fn resolve_scss_vars(line: &str, vars: &std::collections::HashMap<String, String>) -> String {
    if !line.contains('$') {
        return line.to_string();
    }
    let mut result = line.to_string();
    for (name, val) in vars {
        result = result.replace(&format!("${name}"), val);
    }
    result
}

/// Propriedades CSS que precisam de unidade px quando o valor for número puro.
const PX_PROPS: &[&str] = &[
    "font-size", "padding", "border-radius", "margin",
    "width", "height", "top", "left", "right", "bottom",
    "border-width", "gap",
];

fn add_px_to_unitless(line: &str) -> String {
    let trimmed = line.trim();
    for prop in PX_PROPS {
        if trimmed.starts_with(prop) {
            if let Some(colon) = trimmed.find(':') {
                let value = trimmed[colon + 1..].trim().trim_end_matches(';').trim();
                // Só adiciona px se o valor for um número puro (sem %, px, etc.)
                if value.parse::<f32>().is_ok() {
                    let indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();
                    return format!("{indent}{prop}: {value}px;");
                }
            }
        }
    }
    line.to_string()
}

// ============================================================================
// HTML extraction
// ============================================================================

/// Encontra todos os elementos com atributo `dev` e retorna cada um como HTML string.
fn extract_dev_components(html: &str) -> Vec<String> {
    let chars: Vec<char> = html.chars().collect();
    let mut pos = 0;
    let mut components = Vec::new();

    while pos < chars.len() {
        if chars[pos] == '<' && pos + 1 < chars.len() && chars[pos + 1] != '/' {
            if let Some((html_str, end)) = try_extract_dev_element(&chars, pos) {
                components.push(html_str);
                pos = end;
                continue;
            }
        }
        pos += 1;
    }
    components
}

/// Tenta parsear um elemento a partir de `pos`. Retorna (html, nova_pos) se tiver atributo `dev`.
fn try_extract_dev_element(chars: &[char], start: usize) -> Option<(String, usize)> {
    let mut pos = start + 1; // skip '<'

    // Lê tag name
    skip_ws(chars, &mut pos);
    let tag = read_ident(chars, &mut pos);
    if tag.is_empty() || tag == "for" || tag == "if" || tag == "else" {
        return None;
    }

    // Lê atributos
    let mut has_dev = false;
    let mut classes = String::new();

    skip_ws(chars, &mut pos);
    while pos < chars.len() && chars[pos] != '>' && chars[pos] != '/' {
        if pos < chars.len() && chars[pos] == '(' {
            // Event binding: (click)="funcao()" — consome inteiro e ignora
            pos += 1; // skip '('
            read_ident(chars, &mut pos); // "click"
            if pos < chars.len() && chars[pos] == ')' { pos += 1; }
            skip_ws(chars, &mut pos);
            if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                skip_ws(chars, &mut pos);
                read_quoted(chars, &mut pos);
            }
        } else {
            let attr = read_ident(chars, &mut pos);
            skip_ws(chars, &mut pos);
            if attr == "dev" {
                has_dev = true;
            } else if attr == "class" && pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                classes = read_quoted(chars, &mut pos);
            } else if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                read_quoted(chars, &mut pos);
            }
        }
        skip_ws(chars, &mut pos);
    }

    if !has_dev {
        return None;
    }

    // Self-closing?
    if pos < chars.len() && chars[pos] == '/' {
        pos += 2; // skip '/>'
        return Some((format!("<{tag} class=\"{classes}\"></{tag}>"), pos));
    }
    if pos < chars.len() && chars[pos] == '>' {
        pos += 1;
    }

    // Lê conteúdo até closing tag
    let (inner, end) = read_inner_html(chars, pos, &tag);
    let html = format!("<{tag} class=\"{classes}\">{inner}</{tag}>");
    Some((html, end))
}

/// Lê o conteúdo interno de um elemento até encontrar </tag>, convertendo Den → HTML.
fn read_inner_html(chars: &[char], start: usize, parent_tag: &str) -> (String, usize) {
    let mut pos = start;
    let mut out = String::new();

    while pos < chars.len() {
        if chars[pos] == '<' {
            // Closing tag?
            if pos + 1 < chars.len() && chars[pos + 1] == '/' {
                // Skip past closing tag
                while pos < chars.len() && chars[pos] != '>' {
                    pos += 1;
                }
                pos += 1; // skip '>'
                break;
            }

            // Control flow tags — render children, skip the wrapper
            let tag = peek_tag(chars, pos);
            match tag.as_str() {
                "for" => {
                    let (inner, end) = extract_for_children(chars, pos);
                    out.push_str(&inner);
                    pos = end;
                }
                "if" => {
                    let (inner, end) = extract_if_children(chars, pos);
                    out.push_str(&inner);
                    pos = end;
                }
                "else" => {
                    // Pula o <else> por completo
                    pos = skip_tag(chars, pos, "else");
                }
                _ => {
                    // Elemento normal — converte recursivamente
                    let (el_html, end) = convert_element(chars, pos);
                    out.push_str(&el_html);
                    pos = end;
                }
            }
        } else if chars[pos] == '{' && pos + 1 < chars.len() && chars[pos + 1] == '{' {
            // {{ expr }} �� placeholder
            let (placeholder, end) = extract_interpolation(chars, pos);
            out.push_str(&placeholder);
            pos = end;
        } else {
            out.push(chars[pos]);
            pos += 1;
        }
    }
    let _ = parent_tag; // usado só pra contexto
    (out, pos)
}

fn convert_element(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 1; // skip '<'
    skip_ws(chars, &mut pos);
    let tag = read_ident(chars, &mut pos);
    if tag.is_empty() {
        return (String::new(), pos);
    }

    let mut classes = String::new();
    skip_ws(chars, &mut pos);
    while pos < chars.len() && chars[pos] != '>' && chars[pos] != '/' {
        if pos < chars.len() && chars[pos] == '(' {
            // Event binding: (click)="funcao()" — consome inteiro e ignora
            pos += 1;
            read_ident(chars, &mut pos);
            if pos < chars.len() && chars[pos] == ')' { pos += 1; }
            skip_ws(chars, &mut pos);
            if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                skip_ws(chars, &mut pos);
                read_quoted(chars, &mut pos);
            }
        } else {
            let attr = read_ident(chars, &mut pos);
            skip_ws(chars, &mut pos);
            if attr == "class" && pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                classes = read_quoted(chars, &mut pos);
            } else if attr == "dev" {
                // ignora
            } else if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                read_quoted(chars, &mut pos);
            }
        }
        skip_ws(chars, &mut pos);
    }

    if pos < chars.len() && chars[pos] == '/' {
        pos += 2;
        return (format!("<{tag} class=\"{classes}\"></{tag}>"), pos);
    }
    if pos < chars.len() && chars[pos] == '>' {
        pos += 1;
    }

    let html_tag = den_tag_to_html(&tag);
    let (inner, end) = read_inner_html(chars, pos, &tag);
    (format!("<{html_tag} class=\"{classes}\">{inner}</{html_tag}>"), end)
}

fn den_tag_to_html(tag: &str) -> &str {
    match tag {
        "heading" => "h2",
        t => t,
    }
}

fn extract_for_children(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 1;
    skip_ws(chars, &mut pos);
    read_ident(chars, &mut pos); // "for"

    // Lê atributo each pra usar como placeholder
    let mut each_var = "item".to_string();
    skip_ws(chars, &mut pos);
    while pos < chars.len() && chars[pos] != '>' {
        let attr = read_ident(chars, &mut pos);
        skip_ws(chars, &mut pos);
        if pos < chars.len() && chars[pos] == '=' {
            pos += 1;
            let val = read_quoted(chars, &mut pos);
            if attr == "each" { each_var = val; }
        }
        skip_ws(chars, &mut pos);
    }
    if pos < chars.len() { pos += 1; } // skip '>'

    // Lê filhos uma vez, substituindo a variável por placeholder
    let (inner, end) = read_inner_html(chars, pos, "for");
    // Substitui {{ each_var }} por um placeholder legível
    let placeholder = format!("[{each_var}]");
    let inner = inner.replace(&format!("{each_var}"), &placeholder);
    (inner, end)
}

fn extract_if_children(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 1;
    skip_ws(chars, &mut pos);
    read_ident(chars, &mut pos); // "if"
    while pos < chars.len() && chars[pos] != '>' { pos += 1; }
    if pos < chars.len() { pos += 1; }

    // Só renderiza o bloco then
    let (inner, end) = read_inner_html(chars, pos, "if");
    (inner, end)
}

fn skip_tag(chars: &[char], start: usize, _tag: &str) -> usize {
    let mut pos = start;
    // Skip opening tag
    while pos < chars.len() && chars[pos] != '>' { pos += 1; }
    if pos < chars.len() { pos += 1; }
    // Skip children until closing tag
    while pos < chars.len() {
        if chars[pos] == '<' && pos + 1 < chars.len() && chars[pos + 1] == '/' {
            while pos < chars.len() && chars[pos] != '>' { pos += 1; }
            if pos < chars.len() { pos += 1; }
            return pos;
        }
        pos += 1;
    }
    pos
}

fn extract_interpolation(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 2; // skip '{{'
    let expr_start = pos;
    while pos + 1 < chars.len() && !(chars[pos] == '}' && chars[pos + 1] == '}') {
        pos += 1;
    }
    let expr: String = chars[expr_start..pos].iter().collect();
    let expr = expr.trim()
        .trim_start_matches("self.")
        .trim_start_matches("this.");
    let placeholder = format!("<span class=\"den-placeholder\">[{expr}]</span>");
    if pos + 1 < chars.len() { pos += 2; } // skip '}}'
    (placeholder, pos)
}

// ============================================================================
// HTML generation
// ============================================================================

fn generate_preview_html(css: &str, components: &[(String, String)]) -> String {
    let components_html: String = components
        .iter()
        .map(|(label, html)| {
            format!(
                r#"<div class="den-preview-item">
  <div class="den-preview-label">{label}</div>
  <div class="den-preview-component">{html}</div>
</div>"#
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="refresh" content="3">
  <title>Den Preview</title>
  <style>
/* ── Den component styles ────────────────────────────── */
{css}

/* ── Preview chrome ──────────────────────────────────── */
* {{ box-sizing: border-box; }}
body {{
  background: #1a1a2e;
  color: #ccc;
  font-family: sans-serif;
  margin: 0;
  padding: 24px;
}}
.den-preview-item {{
  margin-bottom: 32px;
}}
.den-preview-label {{
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 1px;
  color: #555;
  margin-bottom: 8px;
}}
.den-preview-component {{
  /* Simula a janela egui: mesma largura, background e padding do CentralPanel. */
  /* width: 100% nos filhos resolve relativo a este container — igual ao app nativo. */
  width: {EGUI_WINDOW_WIDTH}px;
  min-height: 40px;
  background: #f4f4f4;  /* egui Visuals::light() approximate */
  padding: 8px;          /* CentralPanel default inner margin */
  border: 2px solid #555;
  border-radius: 4px;
  overflow: hidden;
}}
.den-placeholder {{
  background: #2a2a3e;
  color: #888;
  border-radius: 3px;
  padding: 0 4px;
  font-style: italic;
  font-size: 0.9em;
}}
  </style>
</head>
<body>
{components_html}
</body>
</html>"#
    )
}

// ============================================================================
// Parser helpers (independente do den_macros)
// ============================================================================

fn peek_tag(chars: &[char], pos: usize) -> String {
    let mut p = pos;
    if p < chars.len() && chars[p] == '<' { p += 1; }
    while p < chars.len() && chars[p].is_ascii_whitespace() { p += 1; }
    let start = p;
    while p < chars.len() && (chars[p].is_ascii_alphanumeric() || chars[p] == '_' || chars[p] == '-') {
        p += 1;
    }
    chars[start..p].iter().collect()
}

fn skip_ws(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() { *pos += 1; }
}

fn read_ident(chars: &[char], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < chars.len() && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-') {
        *pos += 1;
    }
    chars[start..*pos].iter().collect()
}

fn read_quoted(chars: &[char], pos: &mut usize) -> String {
    if *pos >= chars.len() { return String::new(); }
    let q = chars[*pos];
    if q != '"' && q != '\'' { return read_ident(chars, pos); }
    *pos += 1;
    let start = *pos;
    while *pos < chars.len() && chars[*pos] != q { *pos += 1; }
    let val: String = chars[start..*pos].iter().collect();
    if *pos < chars.len() { *pos += 1; }
    val
}
