//! Conversão SCSS → CSS pro preview HTML.
//!
//! Resolve variáveis `$nome`, adiciona `px` em valores numéricos sem unidade,
//! reescreve `url(...)` de `@font-face` copiando os arquivos pra `preview/fonts/`.
//! Não roda layout: o browser cuida disso, e nossa engine real (egui) é
//! independente desse pipeline.
//!
//! DUPLICAÇÃO: várias funções aqui (`collect_scss_vars`, `resolve_scss_vars`,
//! `vars_by_longest_name`) espelham `den_macros/parse/scss/variables.rs`.
//! Extrair pra `den_core` quando criar — ver PENDING.md.

use super::preview_config::PX_PROPS;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Converte SCSS do Den pra CSS puro.
/// Resolve variáveis `$nome`, adiciona `px` onde o valor é numérico sem unidade.
/// Não injeta hacks — o CSS resultante é direto e nossa layout engine casa com
/// `box-sizing: border-box` + flex padrão.
pub(super) fn scss_to_css(scss: &str) -> String {
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
pub(super) fn rewrite_font_urls(
    css: &str,
    scss_dir: &Path,
    fonts_dir: &Path,
    page_slug: &str,
) -> String {
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
