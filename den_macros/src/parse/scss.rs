//! Parser SCSS mínimo usado em compile time pelo pipeline Den.

use super::color::parse_hex_color;
use crate::types::{BorderStyle, DisplayMode, StyleMap, StyleRule, WidthValue};
use std::collections::HashMap;

// SCSS identifiers são ASCII-only, então parsing byte-level é seguro aqui.

/// Converte SCSS de uma página em mapa de estilos resolvidos por classe.
pub fn parse_scss(input: &str) -> StyleMap {
    let vars = collect_variables(input);
    let mut styles = StyleMap::new();
    let input = input.trim();
    let bytes = input.as_bytes();
    let mut pos = 0;

    while pos < bytes.len() {
        skip_whitespace(bytes, &mut pos);
        if pos >= bytes.len() {
            break;
        }
        if skip_comment(bytes, &mut pos) {
            continue;
        }

        // Pula declarações de variáveis ($var: value;)
        if bytes[pos] == b'$' {
            while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'\n' {
                pos += 1;
            }
            if pos < bytes.len() {
                pos += 1;
            }
            continue;
        }

        // Aceita seletor `.className` (classe) ou `body` (tag raiz).
        // Armazena ambos no mesmo mapa; a chave é o nome sem ponto.
        // Como `body` é reservado pra tag do root, não colide com classe `.body`
        // em templates Den (convenção).
        let selector = if bytes[pos] == b'.' {
            pos += 1;
            read_identifier(bytes, &mut pos)
        } else if bytes[pos..].starts_with(b"body")
            && bytes.get(pos + 4).is_some_and(|c| !is_ident_char(*c))
        {
            pos += 4;
            "body".to_string()
        } else {
            pos += 1;
            continue;
        };

        if selector.is_empty() {
            continue;
        }
        let class_name = selector;

        let pseudo = if pos < bytes.len() && bytes[pos] == b':' {
            pos += 1;
            let p = read_identifier(bytes, &mut pos);
            if p.is_empty() { None } else { Some(p) }
        } else {
            None
        };

        skip_whitespace(bytes, &mut pos);

        if pos >= bytes.len() || bytes[pos] != b'{' {
            continue;
        }
        pos += 1; // skip '{'

        let mut rule = StyleRule::default();

        loop {
            skip_whitespace(bytes, &mut pos);
            if skip_comment(bytes, &mut pos) {
                continue;
            }
            if pos >= bytes.len() || bytes[pos] == b'}' {
                if pos < bytes.len() {
                    pos += 1; // skip '}'
                }
                break;
            }

            let prop_name = read_css_identifier(bytes, &mut pos);
            skip_whitespace(bytes, &mut pos);

            if prop_name.is_empty() {
                pos += 1;
                continue;
            }

            if pos >= bytes.len() || bytes[pos] != b':' {
                skip_invalid_declaration(bytes, &mut pos);
                continue;
            }
            pos += 1; // skip ':'
            skip_whitespace(bytes, &mut pos);

            let start = pos;
            while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'}' {
                pos += 1;
            }
            let raw = std::str::from_utf8(&bytes[start..pos])
                .unwrap_or("")
                .trim()
                .to_string();
            let value = resolve_vars(&raw, &vars);

            if pos < bytes.len() && bytes[pos] == b';' {
                pos += 1;
            }

            match prop_name.as_str() {
                "color" => rule.color = parse_hex_color(&value),
                "font-size" => rule.font_size = parse_size_value(&value),
                "background" => rule.background = parse_hex_color(&value),
                "padding" => rule.padding = parse_size_value(&value),
                "margin" => rule.margin = parse_size_value(&value),
                "display" if value == "flex" => rule.display = DisplayMode::Flex,
                "display" if value == "grid" => rule.display = DisplayMode::Grid,
                "border" => rule.border = parse_border_value(&value),
                "border-radius" => rule.border_radius = parse_size_value(&value),
                "width" => rule.width = parse_width_value(&value),
                "height" => rule.height = parse_width_value(&value),
                "min-width" => rule.min_width = Some(parse_width_value(&value)),
                "max-width" => rule.max_width = Some(parse_width_value(&value)),
                "min-height" => rule.min_height = Some(parse_width_value(&value)),
                "max-height" => rule.max_height = Some(parse_width_value(&value)),
                "gap" => rule.gap = parse_size_value(&value),
                "cursor" if value == "pointer" => rule.cursor_pointer = true,
                "flex" if value == "1" => rule.flex_grow = true,
                "flex-grow" if value == "1" => rule.flex_grow = true,
                _ => {}
            }
        }

        match pseudo.as_deref() {
            Some("hover") => {
                let entry = styles.entry(class_name).or_default();
                entry.hover = Some(Box::new(rule));
            }
            Some(p) => {
                eprintln!("Den: unsupported pseudo-selector ':{p}', ignoring");
            }
            None => {
                styles.entry(class_name).or_default().merge_from(&rule);
            }
        }
    }

    styles
}

/// Coleta todas as declarações `$nome: valor;` do SCSS.
// DUPLICAÇÃO: lógica similar em preview.rs e style_editor.rs. Extrair pra den_core. Ver PENDING.md.
fn collect_variables(input: &str) -> HashMap<String, String> {
    let mut vars = HashMap::new();
    let bytes = input.as_bytes();
    let mut pos = 0;

    while pos < bytes.len() {
        skip_whitespace(bytes, &mut pos);
        if pos >= bytes.len() {
            break;
        }
        if skip_comment(bytes, &mut pos) {
            continue;
        }

        if bytes[pos] == b'$' {
            pos += 1; // skip '$'
            let name = read_identifier(bytes, &mut pos);
            skip_whitespace(bytes, &mut pos);
            if pos < bytes.len() && bytes[pos] == b':' {
                pos += 1; // skip ':'
                skip_whitespace(bytes, &mut pos);
                let start = pos;
                while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'\n' {
                    pos += 1;
                }
                let value = std::str::from_utf8(&bytes[start..pos])
                    .unwrap_or("")
                    .trim()
                    .to_string();
                if !name.is_empty() && !value.is_empty() {
                    vars.insert(name, value);
                }
            }
        }
        pos += 1;
    }
    vars
}

/// Substitui referências `$nome` pelos valores resolvidos.
fn resolve_vars(value: &str, vars: &HashMap<String, String>) -> String {
    if !value.contains('$') {
        return value.to_string();
    }
    let mut result = value.to_string();
    for (name, val) in vars {
        result = result.replace(&format!("${name}"), val);
    }
    result
}

fn parse_size_value(value: &str) -> Option<f32> {
    value.trim_end_matches("px").parse::<f32>().ok()
}

fn parse_width_value(value: &str) -> WidthValue {
    if value == "auto" {
        return WidthValue::Auto;
    }
    if let Some(pct) = value.strip_suffix('%')
        && let Ok(v) = pct.trim().parse::<f32>()
    {
        return WidthValue::Percent(v / 100.0);
    }
    if let Some(v) = parse_size_value(value) {
        return WidthValue::Px(v);
    }
    eprintln!("Den: unsupported width value '{value}', falling back to auto");
    WidthValue::Auto
}

fn parse_border_value(value: &str) -> Option<BorderStyle> {
    let parts: Vec<&str> = value.split_whitespace().collect();
    if parts.len() < 3 {
        return None;
    }
    let width = parse_size_value(parts[0])?;
    let style = parts[1];
    if style != "solid" {
        eprintln!("Den: border style '{style}' is not supported, rendering as solid");
    }
    let color = parse_hex_color(parts[2])?;
    Some(BorderStyle { width, color })
}

fn skip_whitespace(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

/// Pula comentários `// ...` e `/* ... */`, retornando se consumiu algo.
fn skip_comment(bytes: &[u8], pos: &mut usize) -> bool {
    if *pos + 1 >= bytes.len() || bytes[*pos] != b'/' {
        return false;
    }

    if bytes[*pos + 1] == b'/' {
        *pos += 2;
        while *pos < bytes.len() && bytes[*pos] != b'\n' {
            *pos += 1;
        }
        return true;
    }

    if bytes[*pos + 1] == b'*' {
        *pos += 2;
        while *pos + 1 < bytes.len() && !(bytes[*pos] == b'*' && bytes[*pos + 1] == b'/') {
            *pos += 1;
        }
        *pos = (*pos + 2).min(bytes.len());
        return true;
    }

    false
}

/// Avança até o fim de uma declaração inválida sem consumir a chave de fechamento.
fn skip_invalid_declaration(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos] != b';' && bytes[*pos] != b'}' {
        *pos += 1;
    }
    if *pos < bytes.len() && bytes[*pos] == b';' {
        *pos += 1;
    }
}

fn read_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len() && is_ident_char(bytes[*pos]) {
        *pos += 1;
    }
    std::str::from_utf8(&bytes[start..*pos])
        .unwrap_or("")
        .to_string()
}

/// Caractere válido em identificador CSS/SCSS (letra/dígito/`_`/`-`).
fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'-'
}

fn read_css_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len()
        && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'-' || bytes[*pos] == b'_')
    {
        *pos += 1;
    }
    std::str::from_utf8(&bytes[start..*pos])
        .unwrap_or("")
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::parse_scss;

    #[test]
    fn line_comment_inside_rule_is_ignored() {
        let styles = parse_scss(
            r#"
            .card {
                color: #fff;
                // comentário entre declarações
                background: #000;
            }
            "#,
        );

        let card = styles.get("card").expect("card style");
        assert_eq!(card.color, Some((255, 255, 255)));
        assert_eq!(card.background, Some((0, 0, 0)));
    }

    #[test]
    fn block_comment_inside_rule_is_ignored() {
        let styles = parse_scss(
            r#"
            .card {
                /* comentário entre declarações */
                padding: 12px;
            }
            "#,
        );

        let card = styles.get("card").expect("card style");
        assert_eq!(card.padding, Some(12.0));
    }

    #[test]
    fn invalid_declaration_advances_to_next_property() {
        let styles = parse_scss(
            r#"
            .card {
                /
                color: #123456;
            }
            "#,
        );

        let card = styles.get("card").expect("card style");
        assert_eq!(card.color, Some((18, 52, 86)));
    }
}
