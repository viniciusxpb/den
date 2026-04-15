//! Den Preview Generator
//!
//! Renderiza todas as páginas (`.html` + `.scss` pair em `src/pages/`) num
//! único HTML estático — mesma largura de viewport, mesmo box-sizing, mesmo
//! layout engine (CSS flex match do runtime Den).
//!
//! Saída: `preview/preview.html`.
//!
//! Uso: `cargo run --bin preview`.
//!
//! Estrutura:
//! - este `main.rs` — entry, descoberta de fontes, limpeza de arquivos legados.
//! - [`mod@discovery`] — walk dos pares html+scss em `src/pages/`.
//! - [`mod@scss_css`] — SCSS → CSS (resolve vars, copia fonts, adiciona px).
//! - [`mod@html_convert`] — Den HTML (`@if`/`@for`/etc) → HTML padrão.
//! - [`mod@render`] — concatena tudo num único arquivo, escopa CSS por página.

mod discovery;
mod html_convert;
mod preview_config;
mod render;
mod scss_css;

use discovery::find_template_pairs;
use html_convert::convert_page_body;
use preview_config::{LEGACY_INDEX_FILE_NAME, PREVIEW_FILE_NAME};
use render::{page_slug, render_preview};
use scss_css::{rewrite_font_urls, scss_to_css};
use std::fs;
use std::io;
use std::path::Path;

/// Página Den convertida para HTML estático, pronta para entrar no preview.
pub(crate) struct PagePreview {
    pub(crate) name: String,
    pub(crate) css: String,
    pub(crate) body_html: String,
}

fn main() -> io::Result<()> {
    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let pages_dir = Path::new(&manifest).join("src/pages");
    let preview_dir = Path::new(&manifest).join("../preview");

    fs::create_dir_all(&preview_dir)?;
    write_preview_fonts(&preview_dir)?;

    let mut pairs = find_template_pairs(&pages_dir);
    pairs.sort_by(|a, b| a.0.cmp(&b.0));
    if pairs.is_empty() {
        eprintln!("preview: nenhum template em {}", pages_dir.display());
        return Ok(());
    }

    let mut pages = Vec::new();

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
        let css = rewrite_font_urls(
            &css,
            scss_path.parent().unwrap_or(&pages_dir),
            &preview_dir.join("fonts"),
            &page_slug(&page_name),
        );
        let body_html = convert_page_body(&html);
        pages.push(PagePreview {
            name: page_name,
            css,
            body_html,
        });
    }

    if pages.is_empty() {
        eprintln!(
            "preview: nenhum template pôde ser lido em {}",
            pages_dir.display()
        );
        return Ok(());
    }

    remove_legacy_preview_files(&preview_dir, &pages);

    let preview_html = render_preview(&pages);
    let preview_path = preview_dir.join(PREVIEW_FILE_NAME);
    fs::write(&preview_path, preview_html)?;
    println!(
        "preview: {} páginas → {}",
        pages.len(),
        preview_path.display()
    );

    // Sempre abre o arquivo único — não tem watch mode aqui, então não gera spam.
    // Browsers costumam focar na tab existente se a URL já está aberta.
    open_preview_file(&preview_path);

    Ok(())
}

/// Escreve as fontes default do egui no diretório do preview.
fn write_preview_fonts(preview_dir: &Path) -> io::Result<()> {
    // Os bytes vêm direto do crate `epaint_default_fonts` (mesmos bytes que o
    // egui usa em runtime), então o browser renderiza com métricas idênticas.
    let fonts_dir = preview_dir.join("fonts");
    fs::create_dir_all(&fonts_dir)?;
    fs::write(
        fonts_dir.join("Ubuntu-Light.ttf"),
        epaint_default_fonts::UBUNTU_LIGHT,
    )?;
    fs::write(
        fonts_dir.join("Hack-Regular.ttf"),
        epaint_default_fonts::HACK_REGULAR,
    )?;
    Ok(())
}

/// Remove HTMLs gerados pela versão antiga de preview por página.
fn remove_legacy_preview_files(preview_dir: &Path, pages: &[PagePreview]) {
    remove_legacy_preview_file(&preview_dir.join(LEGACY_INDEX_FILE_NAME));
    for page in pages {
        remove_legacy_preview_file(&preview_dir.join(format!("{}.html", page.name)));
    }
}

/// Remove um arquivo legado se existir, avisando no stderr quando falhar.
fn remove_legacy_preview_file(path: &Path) {
    match fs::remove_file(path) {
        Ok(()) => {}
        Err(err) if err.kind() == io::ErrorKind::NotFound => {}
        Err(err) => eprintln!("preview: falha ao remover {}: {err}", path.display()),
    }
}

/// Abre o preview no browser padrão quando o sistema possui `xdg-open`.
fn open_preview_file(path: &Path) {
    if let Err(err) = std::process::Command::new("xdg-open").arg(path).spawn() {
        eprintln!(
            "preview: gerado, mas falhou abrir {}: {err}",
            path.display()
        );
    }
}
