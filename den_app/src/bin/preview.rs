//! Den Preview Generator
//!
//! Renderiza cada página (`.html` + `.scss` pair em `src/pages/`) como um HTML
//! estático IDÊNTICO ao que o egui desenharia — mesma largura de viewport,
//! mesmo box-sizing, mesmo layout engine (CSS flex match do runtime Den).
//!
//! Saída: um arquivo por página em `preview/<pagename>.html`.
//!
//! Uso: `cargo run --bin preview`.

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Largura da janela egui em pixels (deve coincidir com `app_config::WINDOW_WIDTH = 1200`).
/// O container do preview usa exatamente esta largura pra que `width: 100%` resolva
/// contra o mesmo espaço disponível que o app nativo enxerga.
const EGUI_WINDOW_WIDTH: u32 = 1200;

/// Quantas iterações simular em `<for>` quando não temos dados reais.
const FOR_LOOP_ITERATIONS: usize = 3;

fn main() {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let pages_dir = Path::new(&manifest).join("src/pages");
    let preview_dir = Path::new(&manifest).join("../preview");

    fs::create_dir_all(&preview_dir).ok();

    // Escreve as fontes default do egui no preview dinamicamente.
    // Os bytes vêm direto do crate `epaint_default_fonts` (mesmos bytes que o
    // egui usa em runtime), então o browser renderiza com métricas idênticas.
    let fonts_dir = preview_dir.join("fonts");
    fs::create_dir_all(&fonts_dir).ok();
    let _ = fs::write(
        fonts_dir.join("Ubuntu-Light.ttf"),
        epaint_default_fonts::UBUNTU_LIGHT,
    );
    let _ = fs::write(
        fonts_dir.join("Hack-Regular.ttf"),
        epaint_default_fonts::HACK_REGULAR,
    );

    let pairs = find_template_pairs(&pages_dir);
    if pairs.is_empty() {
        eprintln!("preview: nenhum template em {}", pages_dir.display());
        return;
    }

    let mut index_links: Vec<(String, String)> = Vec::new();

    for (html_path, scss_path) in &pairs {
        let Some(page_name) = html_path
            .parent()
            .and_then(|p| p.file_name())
            .map(|n| n.to_string_lossy().to_string())
        else {
            continue;
        };

        let html = match fs::read_to_string(html_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("preview: falha ler {}: {e}", html_path.display());
                continue;
            }
        };
        let scss = match fs::read_to_string(scss_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("preview: falha ler {}: {e}", scss_path.display());
                continue;
            }
        };

        let css = scss_to_css(&scss);
        let body_html = convert_page_body(&html);
        let page_html = render_page(&page_name, &css, &body_html);

        let out_file = preview_dir.join(format!("{page_name}.html"));
        fs::write(&out_file, &page_html).expect("write preview page");
        println!("preview: {} → {}", page_name, out_file.display());

        index_links.push((page_name.clone(), format!("{page_name}.html")));
    }

    // Index com links pra cada página.
    let index_html = render_index(&index_links);
    let index_path = preview_dir.join("index.html");
    fs::write(&index_path, index_html).expect("write index.html");

    // Sempre abre o index — não tem watch mode aqui, então não gera spam.
    // Browsers costumam focar na tab existente se a URL já está aberta.
    std::process::Command::new("xdg-open")
        .arg(&index_path)
        .spawn()
        .ok();
}

// ============================================================================
// Template discovery
// ============================================================================

fn find_template_pairs(dir: &Path) -> Vec<(PathBuf, PathBuf)> {
    let mut pairs = Vec::new();
    let Ok(entries) = fs::read_dir(dir) else {
        return pairs;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            pairs.extend(find_template_pairs(&path));
        } else if path.extension().is_some_and(|e| e == "html") {
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

/// Converte SCSS do Den pra CSS puro.
/// Resolve variáveis `$nome`, adiciona `px` onde o valor é numérico sem unidade.
/// Não injeta hacks — o CSS resultante é direto e nossa layout engine casa com
/// `box-sizing: border-box` + flex padrão.
fn scss_to_css(scss: &str) -> String {
    let vars = collect_scss_vars(scss);
    let mut out = String::new();
    for line in scss.lines() {
        let trimmed = line.trim();
        // Pula declarações de variável (não são CSS válido).
        if trimmed.starts_with('$') {
            continue;
        }
        // Comentários SCSS `// ...` não são CSS válido — browsers tratam como
        // erro de sintaxe e podem ignorar regras subsequentes. Ou pula a linha
        // inteira (se começa com //) ou trunca a parte do `//` em diante.
        let line_no_comment = strip_line_comment(line);
        if line_no_comment.trim().is_empty() {
            continue;
        }
        let resolved = resolve_scss_vars(&line_no_comment, &vars);
        let converted = add_px_to_unitless(&resolved);
        out.push_str(&converted);
        out.push('\n');
    }
    out
}

/// Remove comentário `// ...` do fim da linha. Respeita `//` dentro de strings
/// (entre aspas) pra não quebrar URLs tipo `url("http://...")`.
fn strip_line_comment(line: &str) -> String {
    let bytes = line.as_bytes();
    let mut in_str: Option<u8> = None;
    let mut i = 0;
    while i + 1 < bytes.len() {
        let b = bytes[i];
        match in_str {
            Some(q) if b == q => in_str = None,
            Some(_) => {}
            None => {
                if b == b'"' || b == b'\'' {
                    in_str = Some(b);
                } else if b == b'/' && bytes[i + 1] == b'/' {
                    return line[..i].to_string();
                }
            }
        }
        i += 1;
    }
    line.to_string()
}

// DUPLICAÇÃO: mesma lógica em den_macros/parse/scss.rs e bin/style_editor.rs.
// Extrair pra den_core quando criar. Ver PENDING.md.
fn collect_scss_vars(scss: &str) -> HashMap<String, String> {
    let mut vars = HashMap::new();
    for line in scss.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix('$')
            && let Some(colon) = rest.find(':')
        {
            let name = rest[..colon].trim().to_string();
            let value = rest[colon + 1..]
                .trim()
                .trim_end_matches(';')
                .trim()
                .to_string();
            if !name.is_empty() && !value.is_empty() {
                vars.insert(name, value);
            }
        }
    }
    vars
}

fn resolve_scss_vars(line: &str, vars: &HashMap<String, String>) -> String {
    if !line.contains('$') {
        return line.to_string();
    }
    let mut result = line.to_string();
    for (name, val) in vars {
        result = result.replace(&format!("${name}"), val);
    }
    result
}

const PX_PROPS: &[&str] = &[
    "font-size",
    "padding",
    "border-radius",
    "margin",
    "width",
    "height",
    "top",
    "left",
    "right",
    "bottom",
    "border-width",
    "gap",
];

fn add_px_to_unitless(line: &str) -> String {
    let trimmed = line.trim();
    for prop in PX_PROPS {
        if trimmed.starts_with(prop)
            && let Some(colon) = trimmed.find(':')
        {
            let value = trimmed[colon + 1..].trim().trim_end_matches(';').trim();
            if value.parse::<f32>().is_ok() {
                let indent: String = line.chars().take_while(|c| c.is_whitespace()).collect();
                return format!("{indent}{prop}: {value}px;");
            }
        }
    }
    line.to_string()
}

// ============================================================================
// Den HTML → HTML puro (página inteira, não só elementos `dev`)
// ============================================================================

/// Converte o body inteiro de uma página Den em HTML padrão.
/// Processa `<for>`, `<if>/<else>`, `{{ expr }}`, `<input bind=...>`, `goto=`,
/// e pula comentários HTML `<!-- ... -->`.
fn convert_page_body(html: &str) -> String {
    let chars: Vec<char> = html.chars().collect();
    let mut out = String::new();
    let mut pos = 0;
    while pos < chars.len() {
        if is_html_comment_start(&chars, pos) {
            pos = skip_html_comment(&chars, pos);
            continue;
        }
        if chars[pos] == '<' {
            if pos + 1 < chars.len() && chars[pos + 1] == '/' {
                pos = skip_until_gt(&chars, pos);
                continue;
            }
            let tag = peek_tag(&chars, pos);
            match tag.as_str() {
                "for" => {
                    let (inner, end) = convert_for(&chars, pos);
                    out.push_str(&inner);
                    pos = end;
                }
                "if" => {
                    let (inner, end) = convert_if(&chars, pos);
                    out.push_str(&inner);
                    pos = end;
                }
                "else" => {
                    pos = skip_tag(&chars, pos);
                }
                _ => {
                    let (el, end) = convert_element(&chars, pos);
                    out.push_str(&el);
                    pos = end;
                }
            }
        } else if chars[pos] == '{' && pos + 1 < chars.len() && chars[pos + 1] == '{' {
            let (ph, end) = convert_interpolation(&chars, pos);
            out.push_str(&ph);
            pos = end;
        } else {
            out.push(chars[pos]);
            pos += 1;
        }
    }
    out
}

/// `true` se em `pos` começa `<!--`.
fn is_html_comment_start(chars: &[char], pos: usize) -> bool {
    pos + 3 < chars.len()
        && chars[pos] == '<'
        && chars[pos + 1] == '!'
        && chars[pos + 2] == '-'
        && chars[pos + 3] == '-'
}

/// Avança `pos` pra DEPOIS do próximo `-->`. Se não achar, vai até o fim.
fn skip_html_comment(chars: &[char], start: usize) -> usize {
    let mut pos = start + 4; // pula `<!--`
    while pos + 2 < chars.len() {
        if chars[pos] == '-' && chars[pos + 1] == '-' && chars[pos + 2] == '>' {
            return pos + 3;
        }
        pos += 1;
    }
    chars.len()
}

/// Converte um elemento Den em HTML, preservando tag → tag (ex.: `heading` → `h2`).
fn convert_element(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 1;
    skip_ws(chars, &mut pos);
    let tag = read_ident(chars, &mut pos);
    if tag.is_empty() {
        return (String::new(), pos);
    }

    let mut classes = String::new();
    let mut bind_expr: Option<String> = None;
    let mut placeholder: Option<String> = None;
    let mut goto_page: Option<String> = None;

    skip_ws(chars, &mut pos);
    while pos < chars.len() && chars[pos] != '>' && chars[pos] != '/' {
        if chars[pos] == '(' {
            // (click)="..." — consome e ignora (preview estático).
            pos += 1;
            read_ident(chars, &mut pos);
            if pos < chars.len() && chars[pos] == ')' {
                pos += 1;
            }
            skip_ws(chars, &mut pos);
            if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                skip_ws(chars, &mut pos);
                read_quoted(chars, &mut pos);
            }
        } else {
            let attr = read_ident(chars, &mut pos);
            skip_ws(chars, &mut pos);
            if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                skip_ws(chars, &mut pos);
                let val = read_quoted(chars, &mut pos);
                match attr.as_str() {
                    "class" => classes = val,
                    "bind" => bind_expr = Some(val),
                    "placeholder" => placeholder = Some(val),
                    "goto" => goto_page = Some(val),
                    _ => {}
                }
            } else if attr == "dev" {
                // marker interno — ignora
            }
        }
        skip_ws(chars, &mut pos);
    }

    // Self-closing?
    let self_closing = pos < chars.len() && chars[pos] == '/';
    if self_closing {
        pos += 2;
    } else if pos < chars.len() && chars[pos] == '>' {
        pos += 1;
    }

    let html_tag = den_tag_to_html(&tag);

    // Input: vira <input> real com placeholder (bind vira [self.field] read-only).
    if tag == "input" {
        let ph = placeholder.unwrap_or_default();
        let bind_label = bind_expr
            .as_deref()
            .map(|b| b.trim_start_matches("self.").to_string())
            .unwrap_or_default();
        let value_attr = if bind_label.is_empty() {
            String::new()
        } else {
            format!(r#" value="[{bind_label}]""#)
        };
        return (
            format!(
                r#"<input type="text" class="{classes}" placeholder="{ph}"{value_attr} readonly>"#
            ),
            pos,
        );
    }

    if self_closing {
        return (
            format!("<{html_tag} class=\"{classes}\"></{html_tag}>"),
            pos,
        );
    }

    let (inner, end) = read_inner(chars, pos);
    let goto_note = goto_page
        .map(|g| format!(r#" data-goto="{g}""#))
        .unwrap_or_default();
    (
        format!("<{html_tag} class=\"{classes}\"{goto_note}>{inner}</{html_tag}>"),
        end,
    )
}

/// Lê conteúdo interno até `</>`. Processa control flow e interpolação aninhados,
/// e pula comentários HTML `<!-- ... -->` (não leakam como texto).
fn read_inner(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start;
    let mut out = String::new();
    while pos < chars.len() {
        if is_html_comment_start(chars, pos) {
            pos = skip_html_comment(chars, pos);
            continue;
        }
        if chars[pos] == '<' {
            if pos + 1 < chars.len() && chars[pos + 1] == '/' {
                pos = skip_until_gt(chars, pos);
                return (out, pos);
            }
            let tag = peek_tag(chars, pos);
            match tag.as_str() {
                "for" => {
                    let (inner, end) = convert_for(chars, pos);
                    out.push_str(&inner);
                    pos = end;
                }
                "if" => {
                    let (inner, end) = convert_if(chars, pos);
                    out.push_str(&inner);
                    pos = end;
                }
                "else" => {
                    pos = skip_tag(chars, pos);
                }
                _ => {
                    let (el, end) = convert_element(chars, pos);
                    out.push_str(&el);
                    pos = end;
                }
            }
        } else if chars[pos] == '{' && pos + 1 < chars.len() && chars[pos + 1] == '{' {
            let (ph, end) = convert_interpolation(chars, pos);
            out.push_str(&ph);
            pos = end;
        } else {
            out.push(chars[pos]);
            pos += 1;
        }
    }
    (out, pos)
}

/// `<for each="var" in="expr">...</for>` — renderiza N iterações simuladas.
/// A cada iteração, substitui as ocorrências de `[each_var]` (saídas do
/// convert_interpolation aplicado a `{{ each_var }}`) por `[each_var #N]`.
fn convert_for(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 1;
    skip_ws(chars, &mut pos);
    read_ident(chars, &mut pos); // "for"

    let mut each_var = "item".to_string();
    skip_ws(chars, &mut pos);
    while pos < chars.len() && chars[pos] != '>' {
        let attr = read_ident(chars, &mut pos);
        skip_ws(chars, &mut pos);
        if pos < chars.len() && chars[pos] == '=' {
            pos += 1;
            skip_ws(chars, &mut pos);
            let val = read_quoted(chars, &mut pos);
            if attr == "each" {
                each_var = val;
            }
        }
        skip_ws(chars, &mut pos);
    }
    if pos < chars.len() {
        pos += 1;
    } // skip '>'

    let body_start = pos;
    let (body_template, end) = read_inner(chars, body_start);

    // `convert_interpolation` já transformou `{{ each_var }}` em `[each_var]` dentro
    // de um span. Aqui trocamos essa string pela versão numerada por iteração.
    let needle = format!("[{each_var}]");
    let mut out = String::new();
    for i in 0..FOR_LOOP_ITERATIONS {
        let replacement = format!("[{each_var} #{}]", i + 1);
        out.push_str(&body_template.replace(&needle, &replacement));
    }
    (out, end)
}

/// `<if cond="...">...</if>` — sempre renderiza o branch `then`; `<else>` é pulado.
fn convert_if(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 1;
    skip_ws(chars, &mut pos);
    read_ident(chars, &mut pos); // "if"
    while pos < chars.len() && chars[pos] != '>' {
        pos += 1;
    }
    if pos < chars.len() {
        pos += 1;
    }
    read_inner(chars, pos)
}

/// `{{ expr }}` → `<span class="den-placeholder">[expr_sem_self]</span>`.
fn convert_interpolation(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 2;
    let expr_start = pos;
    while pos + 1 < chars.len() && !(chars[pos] == '}' && chars[pos + 1] == '}') {
        pos += 1;
    }
    let expr: String = chars[expr_start..pos].iter().collect();
    let label = expr
        .trim()
        .trim_start_matches("self.")
        .trim_start_matches("this.");
    if pos + 1 < chars.len() {
        pos += 2;
    }
    (
        format!(r#"<span class="den-placeholder">[{label}]</span>"#),
        pos,
    )
}

fn den_tag_to_html(tag: &str) -> &str {
    match tag {
        "heading" => "h2",
        "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => tag,
        t => t,
    }
}

// ============================================================================
// Output HTML
// ============================================================================

/// Renderiza a página completa com CSS injetado. Sem labels, sem chrome — o
/// container externo tem o tamanho exato da janela egui (`EGUI_WINDOW_WIDTH`).
fn render_page(page_name: &str, css: &str, body_html: &str) -> String {
    // Reset mínimo + box-sizing (igual ao layout engine Den, border-box).
    // Font-face usa os mesmos bytes TTF que o egui embarca (Ubuntu-Light p/ proporcional,
    // Hack p/ mono). Os arquivos são escritos pelo `main` em `preview/fonts/`.
    // NÃO define background/color do body — vem 100% do SCSS via seletor `body`.
    format!(
        r#"<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="refresh" content="3">
  <title>Den Preview — {page_name}</title>
  <style>
@font-face {{
    font-family: "DenProportional";
    src: url("fonts/Ubuntu-Light.ttf") format("truetype");
    font-display: block;
}}
@font-face {{
    font-family: "DenMonospace";
    src: url("fonts/Hack-Regular.ttf") format("truetype");
    font-display: block;
}}
*, *::before, *::after {{ box-sizing: border-box; }}
html, body {{ margin: 0; padding: 0; }}
body {{
    min-height: 100vh;
    font-family: "DenProportional", sans-serif;
}}
code, pre {{ font-family: "DenMonospace", monospace; }}
.den-viewport {{
    width: {EGUI_WINDOW_WIDTH}px;
    min-height: 100vh;
    margin: 0 auto;
}}
.den-placeholder {{
    /* Marcação visual pra `{{{{ self.campo }}}}` quando o preview não tem data real. */
    opacity: 0.7;
    font-style: italic;
}}
input {{
    font-family: inherit;
    color: inherit;
    background: transparent;
    border: none;
    outline: none;
}}

/* ── Page stylesheet (CSS do SCSS da página, incluindo seletor `body`) ── */
{css}
  </style>
</head>
<body>
  <div class="den-viewport">
{body_html}
  </div>
</body>
</html>"#
    )
}

fn render_index(links: &[(String, String)]) -> String {
    // Index é uma lista simples de páginas disponíveis. Sem cores hardcoded.
    let items: String = links
        .iter()
        .map(|(name, href)| format!(r#"<li><a href="{href}">{name}</a></li>"#))
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        r#"<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <title>Den Preview — Index</title>
  <style>
body {{ font-family: -apple-system, sans-serif; padding: 40px; }}
h1 {{ font-weight: 300; }}
ul {{ list-style: none; padding: 0; }}
li {{ margin: 8px 0; font-size: 18px; }}
  </style>
</head>
<body>
  <h1>Den — Preview</h1>
  <ul>
{items}
  </ul>
</body>
</html>"#
    )
}

// ============================================================================
// Parser helpers
// ============================================================================

fn peek_tag(chars: &[char], pos: usize) -> String {
    let mut p = pos;
    if p < chars.len() && chars[p] == '<' {
        p += 1;
    }
    while p < chars.len() && chars[p].is_ascii_whitespace() {
        p += 1;
    }
    let start = p;
    while p < chars.len()
        && (chars[p].is_ascii_alphanumeric() || chars[p] == '_' || chars[p] == '-')
    {
        p += 1;
    }
    chars[start..p].iter().collect()
}

fn skip_ws(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn read_ident(chars: &[char], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < chars.len()
        && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-')
    {
        *pos += 1;
    }
    chars[start..*pos].iter().collect()
}

fn read_quoted(chars: &[char], pos: &mut usize) -> String {
    if *pos >= chars.len() {
        return String::new();
    }
    let q = chars[*pos];
    if q != '"' && q != '\'' {
        return read_ident(chars, pos);
    }
    *pos += 1;
    let start = *pos;
    while *pos < chars.len() && chars[*pos] != q {
        *pos += 1;
    }
    let val: String = chars[start..*pos].iter().collect();
    if *pos < chars.len() {
        *pos += 1;
    }
    val
}

fn skip_until_gt(chars: &[char], start: usize) -> usize {
    let mut pos = start;
    while pos < chars.len() && chars[pos] != '>' {
        pos += 1;
    }
    if pos < chars.len() {
        pos += 1;
    }
    pos
}

fn skip_tag(chars: &[char], start: usize) -> usize {
    let mut pos = start;
    pos = skip_until_gt(chars, pos);
    while pos < chars.len() {
        if chars[pos] == '<' && pos + 1 < chars.len() && chars[pos + 1] == '/' {
            return skip_until_gt(chars, pos);
        }
        pos += 1;
    }
    pos
}
