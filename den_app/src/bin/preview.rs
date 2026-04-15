//! Den Preview Generator
//!
//! Renderiza todas as páginas (`.html` + `.scss` pair em `src/pages/`) num
//! único HTML estático — mesma largura de viewport, mesmo box-sizing, mesmo
//! layout engine (CSS flex match do runtime Den).
//!
//! Saída: `preview/preview.html`.
//!
//! Uso: `cargo run --bin preview`.

mod preview_config;

use preview_config::{
    AUTO_REFRESH_SECONDS, EGUI_WINDOW_WIDTH, FOR_LOOP_ITERATIONS, LEGACY_INDEX_FILE_NAME,
    PREVIEW_FILE_NAME, PX_PROPS,
};
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Página Den convertida para HTML estático, pronta para entrar no preview.
struct PagePreview {
    name: String,
    css: String,
    body_html: String,
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

/// Copia fontes relativas declaradas em `url(...)` para `preview/fonts/`.
/// URLs absolutas/data/http ficam intactas; elas já são resolvidas pelo browser.
fn rewrite_font_urls(css: &str, scss_dir: &Path, fonts_dir: &Path, page_slug: &str) -> String {
    let mut out = String::new();
    let mut font_face_depth: Option<i32> = None;

    for line in css.lines() {
        // Assumimos que blocos @font-face não contêm chaves literais dentro de
        // strings. Isso mantém o scanner simples e suficiente para `src: url(...)`.
        if font_face_depth.is_none()
            && line
                .trim_start()
                .to_ascii_lowercase()
                .starts_with("@font-face")
        {
            font_face_depth = Some(0);
        }

        if font_face_depth.is_some() {
            out.push_str(&rewrite_font_urls_in_line(
                line, scss_dir, fonts_dir, page_slug,
            ));
        } else {
            out.push_str(line);
        }
        out.push('\n');

        if let Some(depth) = &mut font_face_depth {
            *depth += line.matches('{').count() as i32;
            *depth -= line.matches('}').count() as i32;
            if *depth <= 0 && line.contains('}') {
                font_face_depth = None;
            }
        }
    }
    out
}

/// Reescreve todos os `url(...)` de uma linha pertencente a um `@font-face`.
fn rewrite_font_urls_in_line(
    line: &str,
    scss_dir: &Path,
    fonts_dir: &Path,
    page_slug: &str,
) -> String {
    let mut out = String::new();
    let mut pos = 0;

    while let Some(rel_start) = find_url_call(&line[pos..]) {
        let call_start = pos + rel_start;
        let arg_start = call_start + "url(".len();
        let Some(close) = find_url_close(line, arg_start) else {
            break;
        };

        out.push_str(&line[pos..arg_start]);
        let raw_arg = &line[arg_start..close];
        out.push_str(&rewrite_font_url_arg(
            raw_arg, scss_dir, fonts_dir, page_slug,
        ));
        pos = close;
    }

    out.push_str(&line[pos..]);
    out
}

/// Encontra a próxima chamada `url(`, ignorando caixa alta/baixa.
fn find_url_call(text: &str) -> Option<usize> {
    text.to_ascii_lowercase().find("url(")
}

/// Encontra o `)` final de `url(...)`, respeitando aspas simples e duplas.
fn find_url_close(text: &str, start: usize) -> Option<usize> {
    let mut quote: Option<char> = None;
    for (offset, ch) in text[start..].char_indices() {
        match quote {
            Some(q) if ch == q => quote = None,
            Some(_) => {}
            None if ch == '"' || ch == '\'' => quote = Some(ch),
            None if ch == ')' => return Some(start + offset),
            None => {}
        }
    }
    None
}

/// Copia o asset de fonte relativo e devolve o argumento de `url(...)` reescrito.
fn rewrite_font_url_arg(
    raw_arg: &str,
    scss_dir: &Path,
    fonts_dir: &Path,
    page_slug: &str,
) -> String {
    let trimmed = raw_arg.trim();
    let (quote, url) = unquote_css_url(trimmed);
    if should_keep_css_url(url) {
        return raw_arg.to_string();
    }

    let source_path_part = css_url_path_part(url);
    let source = scss_dir.join(source_path_part);
    if !source.exists() {
        eprintln!(
            "preview: fonte declarada não encontrada em {}",
            source.display()
        );
        return raw_arg.to_string();
    }

    if let Err(err) = fs::create_dir_all(fonts_dir) {
        eprintln!("preview: falha criando {}: {err}", fonts_dir.display());
        return raw_arg.to_string();
    }

    let Some(file_name) = source.file_name().and_then(|name| name.to_str()) else {
        return raw_arg.to_string();
    };
    let target_name = format!("{page_slug}-{}", sanitize_font_asset_name(file_name));
    let target = fonts_dir.join(&target_name);
    if let Err(err) = fs::copy(&source, &target) {
        eprintln!(
            "preview: falha copiando fonte {} para {}: {err}",
            source.display(),
            target.display()
        );
        return raw_arg.to_string();
    }

    let rewritten = format!("fonts/{target_name}");
    match quote {
        Some(q) => format!("{q}{rewritten}{q}"),
        None => rewritten,
    }
}

/// Remove aspas externas de uma URL CSS, devolvendo a quote original.
fn unquote_css_url(url: &str) -> (Option<char>, &str) {
    let mut chars = url.chars();
    let Some(first) = chars.next() else {
        return (None, url);
    };
    if (first == '"' || first == '\'') && url.ends_with(first) {
        let start = first.len_utf8();
        let end = url.len() - first.len_utf8();
        (Some(first), &url[start..end])
    } else {
        (None, url)
    }
}

/// Retorna se uma URL CSS deve ser preservada sem cópia local.
fn should_keep_css_url(url: &str) -> bool {
    let lower = url.to_ascii_lowercase();
    url.is_empty()
        || url.starts_with('/')
        || url.starts_with('#')
        || lower.starts_with("http:")
        || lower.starts_with("https:")
        || lower.starts_with("data:")
}

/// Extrai só o caminho de uma URL CSS, removendo query/hash para lookup local.
fn css_url_path_part(url: &str) -> &str {
    let query = url.find('?');
    let hash = url.find('#');
    let cutoff = match (query, hash) {
        (Some(q), Some(h)) => q.min(h),
        (Some(q), None) => q,
        (None, Some(h)) => h,
        (None, None) => return url,
    };
    &url[..cutoff]
}

/// Normaliza o nome de arquivo copiado para `preview/fonts`.
fn sanitize_font_asset_name(name: &str) -> String {
    name.chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '.' || ch == '_' || ch == '-' {
                ch
            } else {
                '-'
            }
        })
        .collect()
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
    for (name, val) in vars_by_longest_name(vars) {
        result = result.replace(&format!("${name}"), val);
    }
    result
}

/// Ordena variáveis SCSS por nome descrescente para `$text-dim` vencer `$text`.
fn vars_by_longest_name(vars: &HashMap<String, String>) -> Vec<(&String, &String)> {
    let mut ordered: Vec<_> = vars.iter().collect();
    ordered.sort_by(|(a, _), (b, _)| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
    ordered
}

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
/// Processa `@if`/`!`, `@for`/`@empty`, `@object`, `{{ expr | pipe }}`, atributos
/// `@bind`/`@goto`/`@click`/`@with` e pula comentários HTML `<!-- ... -->`.
fn convert_page_body(html: &str) -> String {
    let chars: Vec<char> = html.chars().collect();
    let mut pos = 0;
    convert_until(&chars, &mut pos, None)
}

/// Parseia sequência de nós até `stop` (`}` dos blocos `@`) ou fim.
fn convert_until(chars: &[char], pos: &mut usize, stop: Option<char>) -> String {
    let mut out = String::new();
    while *pos < chars.len() {
        if let Some(s) = stop
            && chars[*pos] == s
        {
            break;
        }
        if is_html_comment_start(chars, *pos) {
            *pos = skip_html_comment(chars, *pos);
            continue;
        }
        if chars[*pos] == '@' {
            out.push_str(&convert_at_block(chars, pos));
            continue;
        }
        if chars[*pos] == '<' {
            if *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
                *pos = skip_until_gt(chars, *pos);
                continue;
            }
            let (el, end) = convert_element(chars, *pos);
            out.push_str(&el);
            *pos = end;
            continue;
        }
        if chars[*pos] == '{' && *pos + 1 < chars.len() && chars[*pos + 1] == '{' {
            let (ph, end) = convert_interpolation(chars, *pos);
            out.push_str(&ph);
            *pos = end;
            continue;
        }
        out.push(chars[*pos]);
        *pos += 1;
    }
    out
}

/// Despacha `@if`/`@for`/`@object` (e `!` órfão).
fn convert_at_block(chars: &[char], pos: &mut usize) -> String {
    *pos += 1; // skip '@'
    let name = {
        let mut p = *pos;
        read_ident_from(chars, &mut p)
    };
    // Avança p/ leitura do nome
    read_ident_from(chars, pos);
    match name.as_str() {
        "if" => convert_at_if(chars, pos),
        "for" => convert_at_for(chars, pos),
        "object" => convert_at_object(chars, pos),
        _ => String::new(),
    }
}

fn convert_at_if(chars: &[char], pos: &mut usize) -> String {
    // Consome `(cond)` e descarta — preview sempre renderiza o primeiro branch.
    skip_ws_at(chars, pos);
    skip_parens(chars, pos);
    skip_ws_at(chars, pos);
    let then_body = read_and_convert_block(chars, pos);
    // Pula branches `!cond { ... }` / `! { ... }` — preview só mostra o `@if`.
    loop {
        let save = *pos;
        skip_ws_at(chars, pos);
        if *pos >= chars.len() || chars[*pos] != '!' {
            *pos = save;
            break;
        }
        *pos += 1; // skip '!'
        // pula condição (até '{')
        while *pos < chars.len() && chars[*pos] != '{' {
            *pos += 1;
        }
        if *pos < chars.len() && chars[*pos] == '{' {
            *pos += 1;
            // pula conteúdo sem converter (descarta)
            let _ = convert_until(chars, pos, Some('}'));
            if *pos < chars.len() && chars[*pos] == '}' {
                *pos += 1;
            }
        }
    }
    then_body
}

fn convert_at_for(chars: &[char], pos: &mut usize) -> String {
    skip_ws_at(chars, pos);
    // `(var in expr)`
    let header = read_parens_content(chars, pos);
    let each_var = header
        .split(" in ")
        .next()
        .map(str::trim)
        .unwrap_or("item")
        .to_string();
    skip_ws_at(chars, pos);
    let body_template = read_and_convert_block(chars, pos);

    // Opcional `@empty { ... }` — preview com iterações > 0 ignora.
    let save = *pos;
    skip_ws_at(chars, pos);
    if starts_with_word(chars, *pos, "@empty") {
        *pos += 6;
        skip_ws_at(chars, pos);
        if *pos < chars.len() && chars[*pos] == '{' {
            *pos += 1;
            let _ = convert_until(chars, pos, Some('}'));
            if *pos < chars.len() && chars[*pos] == '}' {
                *pos += 1;
            }
        }
    } else {
        *pos = save;
    }

    let needle = format!("[{each_var}]");
    let mut out = String::new();
    for i in 0..FOR_LOOP_ITERATIONS {
        let replacement = format!("[{each_var} #{}]", i + 1);
        out.push_str(&body_template.replace(&needle, &replacement));
    }
    out
}

fn convert_at_object(chars: &[char], pos: &mut usize) -> String {
    skip_ws_at(chars, pos);
    let _scope = read_parens_content(chars, pos);
    skip_ws_at(chars, pos);
    read_and_convert_block(chars, pos)
}

fn read_and_convert_block(chars: &[char], pos: &mut usize) -> String {
    if *pos >= chars.len() || chars[*pos] != '{' {
        return String::new();
    }
    *pos += 1;
    let inner = convert_until(chars, pos, Some('}'));
    if *pos < chars.len() && chars[*pos] == '}' {
        *pos += 1;
    }
    inner
}

fn skip_parens(chars: &[char], pos: &mut usize) {
    if *pos >= chars.len() || chars[*pos] != '(' {
        return;
    }
    *pos += 1;
    let mut depth: i32 = 1;
    while *pos < chars.len() && depth > 0 {
        match chars[*pos] {
            '(' => depth += 1,
            ')' => depth -= 1,
            _ => {}
        }
        *pos += 1;
    }
}

fn read_parens_content(chars: &[char], pos: &mut usize) -> String {
    if *pos >= chars.len() || chars[*pos] != '(' {
        return String::new();
    }
    *pos += 1;
    let start = *pos;
    let mut depth: i32 = 1;
    while *pos < chars.len() && depth > 0 {
        match chars[*pos] {
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        *pos += 1;
    }
    let s: String = chars[start..*pos].iter().collect();
    if *pos < chars.len() {
        *pos += 1;
    }
    s
}

fn skip_ws_at(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn read_ident_from(chars: &[char], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < chars.len()
        && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-')
    {
        *pos += 1;
    }
    chars[start..*pos].iter().collect()
}

fn starts_with_word(chars: &[char], pos: usize, word: &str) -> bool {
    if pos + word.len() > chars.len() {
        return false;
    }
    chars[pos..pos + word.len()].iter().collect::<String>() == word
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
        if chars[pos] == '@' {
            // Atributos Den: @click, @bind, @goto, @with
            pos += 1;
            let attr = read_ident(chars, &mut pos);
            skip_ws(chars, &mut pos);
            if pos < chars.len() && chars[pos] == '=' {
                pos += 1;
                skip_ws(chars, &mut pos);
                let val = read_quoted(chars, &mut pos);
                match attr.as_str() {
                    "bind" => bind_expr = Some(val),
                    "goto" => goto_page = Some(val),
                    // click/with: ignorados no preview estático
                    _ => {}
                }
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
                    "placeholder" => placeholder = Some(val),
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

/// Lê conteúdo interno até `</>`. Processa `@` blocks e interpolação aninhados,
/// e pula comentários HTML `<!-- ... -->` (não leakam como texto).
fn read_inner(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start;
    let mut out = String::new();
    while pos < chars.len() {
        if is_html_comment_start(chars, pos) {
            pos = skip_html_comment(chars, pos);
            continue;
        }
        if chars[pos] == '@' {
            out.push_str(&convert_at_block(chars, &mut pos));
            continue;
        }
        if chars[pos] == '<' {
            if pos + 1 < chars.len() && chars[pos + 1] == '/' {
                pos = skip_until_gt(chars, pos);
                return (out, pos);
            }
            let (el, end) = convert_element(chars, pos);
            out.push_str(&el);
            pos = end;
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

/// `{{ expr | pipe }}` → `<span class="den-placeholder">[expr_sem_self | pipes]</span>`.
fn convert_interpolation(chars: &[char], start: usize) -> (String, usize) {
    let mut pos = start + 2;
    let expr_start = pos;
    while pos + 1 < chars.len() && !(chars[pos] == '}' && chars[pos + 1] == '}') {
        pos += 1;
    }
    let expr: String = chars[expr_start..pos].iter().collect();
    // Para exibição: pega só a parte antes do primeiro `|` (o resto é label meramente informativo).
    let before_pipe = expr.split('|').next().unwrap_or("").trim();
    let label = before_pipe
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

/// Renderiza o arquivo completo de preview com todas as páginas empilhadas.
fn render_preview(pages: &[PagePreview]) -> String {
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
    width: {EGUI_WINDOW_WIDTH}px;
    margin: 0 auto 8px;
    font-size: 18px;
    font-weight: 600;
}}
.den-viewport {{
    width: {EGUI_WINDOW_WIDTH}px;
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
fn page_slug(name: &str) -> String {
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

// ============================================================================
// Parser helpers
// ============================================================================

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

