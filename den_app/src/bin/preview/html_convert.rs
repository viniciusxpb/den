//! Conversão Den HTML (sintaxe `@`) → HTML padrão pro preview estático.
//!
//! Reimplementa parte do parser do `den_macros` mas otimizada pra renderização:
//! `@if` sempre pega o primeiro branch, `@for` itera N vezes simuladas, `@object`
//! some (transparente). Atributos `@click`/`@with` são ignorados (sem runtime).
//!
//! DUPLICAÇÃO: pipeline similar ao `den_macros::parse::html`. Extrair pra
//! `den_core` quando criar — ver PENDING.md.

use super::preview_config::FOR_LOOP_ITERATIONS;

/// Converte o body inteiro de uma página Den em HTML padrão.
/// Processa `@if`/`!`, `@for`/`@empty`, `@object`, `{{ expr | pipe }}`, atributos
/// `@bind`/`@goto`/`@click`/`@with` e pula comentários HTML `<!-- ... -->`.
pub(super) fn convert_page_body(html: &str) -> String {
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

// -- Parser helpers (legacy: Vec<char> com posição mutável) -------------------

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
