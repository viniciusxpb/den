//! Renderiza o HTML final do preview agregando todas as páginas.
//!
//! Faz nav lateral, escopa o CSS de cada página numa wrapper class pra evitar
//! colisão de seletores, e injeta os fonts via `@font-face` apontando pros
//! arquivos copiados em `preview/fonts/`.

use super::PagePreview;
use super::preview_config::AUTO_REFRESH_SECONDS;
use den_app::app_config::WINDOW_WIDTH;

/// Renderiza o arquivo completo de preview com todas as páginas empilhadas.
pub(super) fn render_preview(pages: &[PagePreview]) -> String {
    let nav_items = pages
        .iter()
        .map(|page| {
            let slug = page_slug(&page.name);
            let name = escape_html(&page.name);
            format!(r##"<a href="#page-{slug}">{name}</a>"##)
        })
        .collect::<Vec<_>>()
        .join("\n    ");

    let page_styles = pages
        .iter()
        .map(|page| {
            let page_class = format!("den-page-{}", page_slug(&page.name));
            scope_page_css(&page_class, &page.css)
        })
        .collect::<Vec<_>>()
        .join("\n");

    let sections = pages
        .iter()
        .map(|page| {
            let slug = page_slug(&page.name);
            let page_class = format!("den-page-{slug}");
            let name = escape_html(&page.name);
            format!(
                r#"<section id="page-{slug}" class="den-page-frame">
  <h2>{name}</h2>
  <div class="den-viewport {page_class}">
{body_html}
  </div>
</section>"#,
                body_html = page.body_html
            )
        })
        .collect::<Vec<_>>()
        .join("\n\n");

    // Reset mínimo + box-sizing (igual ao layout engine Den, border-box).
    // Font-face usa os mesmos bytes TTF que o egui embarca (Ubuntu-Light p/ proporcional,
    // Hack p/ mono). Os arquivos são escritos pelo `main` em `preview/fonts/`.
    // O CSS de cada página é escopado no wrapper dela pra evitar colisão de classes.
    format!(
        r#"<!DOCTYPE html>
<html lang="pt-BR">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="refresh" content="{AUTO_REFRESH_SECONDS}">
  <title>Den Preview</title>
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
    background: #f2f4f8;
    color: #18202f;
}}
code, pre {{ font-family: "DenMonospace", monospace; }}
.den-preview-nav {{
    position: sticky;
    top: 0;
    z-index: 10;
    display: flex;
    gap: 8px;
    padding: 12px;
    background: rgba(242, 244, 248, 0.94);
    border-bottom: 1px solid #d8deea;
}}
.den-preview-nav a {{
    color: #18202f;
    text-decoration: none;
    font-size: 14px;
    padding: 6px 10px;
    border: 1px solid #c8d0de;
    border-radius: 6px;
    background: #ffffff;
}}
.den-preview-pages {{
    display: flex;
    flex-direction: column;
    gap: 24px;
    padding: 24px 0 48px;
}}
.den-page-frame {{
    scroll-margin-top: 64px;
}}
.den-page-frame h2 {{
    width: {WINDOW_WIDTH}px;
    margin: 0 auto 8px;
    font-size: 18px;
    font-weight: 600;
}}
.den-viewport {{
    width: {WINDOW_WIDTH}px;
    min-height: 480px;
    margin: 0 auto;
    overflow: hidden;
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

/* ── Page stylesheets escopados por página ── */
{page_styles}
  </style>
</head>
<body>
  <nav class="den-preview-nav">
    {nav_items}
  </nav>
  <main class="den-preview-pages">
{sections}
  </main>
</body>
</html>"#
    )
}

/// Escopa um CSS de página para o wrapper da página agregada no preview.
fn scope_page_css(page_class: &str, css: &str) -> String {
    let mut out = String::new();
    for line in css.lines() {
        let trimmed = line.trim();
        if trimmed.ends_with('{') && !trimmed.starts_with('@') {
            let selector = trimmed.trim_end_matches('{').trim();
            let scoped = selector
                .split(',')
                .map(|part| scope_selector(page_class, part.trim()))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&scoped);
            out.push_str(" {\n");
        } else {
            out.push_str(line);
            out.push('\n');
        }
    }
    out
}

/// Prefixa um seletor CSS simples com a classe da página.
fn scope_selector(page_class: &str, selector: &str) -> String {
    if selector == "body" || selector == "html" {
        return format!(".{page_class}");
    }
    if let Some(rest) = selector.strip_prefix("body") {
        return format!(".{page_class}{rest}");
    }
    if let Some(rest) = selector.strip_prefix("html") {
        return format!(".{page_class}{rest}");
    }
    if selector.starts_with(':') {
        return format!(".{page_class}{selector}");
    }
    format!(".{page_class} {selector}")
}

/// Converte nome de página para id/classe CSS previsível.
pub(super) fn page_slug(name: &str) -> String {
    let mut slug = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            slug.push(ch.to_ascii_lowercase());
        } else if !slug.ends_with('-') {
            slug.push('-');
        }
    }
    let slug = slug.trim_matches('-');
    if slug.is_empty() {
        "page".to_string()
    } else {
        slug.to_string()
    }
}

/// Escapa texto usado em labels do HTML gerado.
fn escape_html(input: &str) -> String {
    input
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}
