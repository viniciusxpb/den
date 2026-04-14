//! Parser SCSS mínimo usado em compile time pelo pipeline Den.

use super::color::parse_hex_color;
use crate::types::{
    BorderStyle, DisplayMode, LineHeightValue, StyleMap, StyleRule, TextAlign, TextTransform,
    WidthValue,
};
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
            let resolved = resolve_vars(&raw, &vars);
            let value = strip_important(&resolved);

            if pos < bytes.len() && bytes[pos] == b';' {
                pos += 1;
            }

            match prop_name.as_str() {
                "color" => rule.color = parse_hex_color(value),
                "font-size" => rule.font_size = parse_size_value(value),
                "font-family" => rule.font_family = parse_font_family(value),
                "font-weight" => rule.font_weight = parse_font_weight(value),
                "font-style" => rule.font_italic = parse_font_style(value),
                "font" => apply_font_shorthand(value, &mut rule),
                "line-height" => rule.line_height = parse_line_height(value),
                "letter-spacing" => rule.letter_spacing = parse_letter_spacing(value),
                "text-transform" => rule.text_transform = parse_text_transform(value),
                "text-align" => rule.text_align = parse_text_align(value),
                "text-decoration" | "text-decoration-line" => {
                    let (underline, strikethrough) = parse_text_decoration(value);
                    if underline.is_some() {
                        rule.underline = underline;
                    }
                    if strikethrough.is_some() {
                        rule.strikethrough = strikethrough;
                    }
                }
                "background" => rule.background = parse_hex_color(value),
                "padding" => rule.padding = parse_size_value(value),
                "margin" => rule.margin = parse_size_value(value),
                "display" if value == "flex" => rule.display = DisplayMode::Flex,
                "display" if value == "grid" => rule.display = DisplayMode::Grid,
                "border" => rule.border = parse_border_value(value),
                "border-radius" => rule.border_radius = parse_size_value(value),
                "width" => rule.width = parse_width_value(value),
                "height" => rule.height = parse_width_value(value),
                "min-width" => rule.min_width = Some(parse_width_value(value)),
                "max-width" => rule.max_width = Some(parse_width_value(value)),
                "min-height" => rule.min_height = Some(parse_width_value(value)),
                "max-height" => rule.max_height = Some(parse_width_value(value)),
                "gap" => rule.gap = parse_size_value(value),
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

/// Parseia tamanho Den/CSS em pixels, aceitando valor sem unidade ou `px`.
fn parse_size_value(value: &str) -> Option<f32> {
    strip_important(value)
        .trim_end_matches("px")
        .parse::<f32>()
        .ok()
}

/// Remove sufixo `!important` sem alocar quando presente.
fn strip_important(value: &str) -> &str {
    let trimmed = value.trim();
    const IMPORTANT: &str = "!important";
    if trimmed.len() >= IMPORTANT.len()
        && trimmed[trimmed.len() - IMPORTANT.len()..].eq_ignore_ascii_case(IMPORTANT)
    {
        trimmed[..trimmed.len() - IMPORTANT.len()].trim_end()
    } else {
        trimmed
    }
}

/// Mantém a pilha de fontes exatamente como declarada em `font-family`.
fn parse_font_family(value: &str) -> Option<String> {
    let value = strip_important(value).trim();
    if value.is_empty() {
        None
    } else {
        Some(value.to_string())
    }
}

/// Parseia pesos CSS convencionais para valor numérico.
fn parse_font_weight(value: &str) -> Option<u16> {
    let value = strip_important(value).trim().to_ascii_lowercase();
    match value.as_str() {
        "normal" => Some(400),
        "bold" | "bolder" => Some(700),
        "lighter" => Some(300),
        _ => value
            .parse::<u16>()
            .ok()
            .map(|weight| weight.clamp(1, 1000)),
    }
}

/// Parseia `font-style`, mapeando itálico/oblíquo para o flag usado no painter.
fn parse_font_style(value: &str) -> Option<bool> {
    let value = strip_important(value).trim().to_ascii_lowercase();
    match value.as_str() {
        "normal" => Some(false),
        "italic" | "oblique" => Some(true),
        _ => None,
    }
}

/// Parseia `line-height` como pixels absolutos ou fator multiplicador.
fn parse_line_height(value: &str) -> Option<LineHeightValue> {
    let value = strip_important(value).trim();
    if value.eq_ignore_ascii_case("normal") {
        return None;
    }
    if let Some(px) = value.strip_suffix("px")
        && let Ok(v) = px.trim().parse::<f32>()
    {
        return Some(LineHeightValue::Px(v));
    }
    if let Some(percent) = value.strip_suffix('%')
        && let Ok(v) = percent.trim().parse::<f32>()
    {
        return Some(LineHeightValue::Factor(v / 100.0));
    }
    value.parse::<f32>().ok().map(LineHeightValue::Factor)
}

/// Parseia `letter-spacing`, tratando `normal` como zero.
fn parse_letter_spacing(value: &str) -> Option<f32> {
    if strip_important(value).eq_ignore_ascii_case("normal") {
        Some(0.0)
    } else {
        parse_size_value(value)
    }
}

/// Parseia a propriedade CSS `text-transform`.
fn parse_text_transform(value: &str) -> Option<TextTransform> {
    let value = strip_important(value).trim().to_ascii_lowercase();
    match value.as_str() {
        "none" => Some(TextTransform::None),
        "uppercase" => Some(TextTransform::Uppercase),
        "lowercase" => Some(TextTransform::Lowercase),
        "capitalize" => Some(TextTransform::Capitalize),
        _ => None,
    }
}

/// Parseia alinhamento textual horizontal.
fn parse_text_align(value: &str) -> Option<TextAlign> {
    let value = strip_important(value).trim().to_ascii_lowercase();
    match value.as_str() {
        "left" | "start" => Some(TextAlign::Left),
        "center" => Some(TextAlign::Center),
        "right" | "end" => Some(TextAlign::Right),
        _ => None,
    }
}

/// Parseia as linhas de decoração que o painter consegue representar.
fn parse_text_decoration(value: &str) -> (Option<bool>, Option<bool>) {
    let value = strip_important(value).trim().to_ascii_lowercase();
    if value == "none" {
        return (Some(false), Some(false));
    }
    let underline = value
        .split_whitespace()
        .any(|part| part == "underline")
        .then_some(true);
    let strikethrough = value
        .split_whitespace()
        .any(|part| part == "line-through")
        .then_some(true);
    (underline, strikethrough)
}

/// Token lexical simples usado para o shorthand `font`.
#[derive(Debug)]
struct CssToken<'a> {
    text: &'a str,
    start: usize,
}

/// Aplica o shorthand `font` no subconjunto suportado pelo Den.
fn apply_font_shorthand(value: &str, rule: &mut StyleRule) {
    let value = strip_important(value);
    let tokens = css_tokens(value);
    let Some((size_idx, size_part, inline_line_height)) = find_font_shorthand_size(&tokens) else {
        return;
    };

    for token in &tokens[..size_idx] {
        if let Some(italic) = parse_font_style(token.text) {
            rule.font_italic = Some(italic);
        } else if let Some(weight) = parse_font_weight(token.text) {
            rule.font_weight = Some(weight);
        }
    }

    rule.font_size = parse_size_value(size_part);

    let mut family_start_idx = size_idx + 1;
    if let Some(line_height) = inline_line_height {
        rule.line_height = parse_line_height(line_height);
    } else if tokens
        .get(family_start_idx)
        .is_some_and(|token| token.text == "/")
        && let Some(line_height_token) = tokens.get(family_start_idx + 1)
    {
        rule.line_height = parse_line_height(line_height_token.text);
        family_start_idx += 2;
    }

    if let Some(family_token) = tokens.get(family_start_idx) {
        let family = value[family_token.start..].trim();
        if !family.is_empty() {
            rule.font_family = Some(family.to_string());
        }
    }
}

/// Encontra o token de tamanho em `font`; por segurança exige unidade `px`.
fn find_font_shorthand_size<'a>(
    tokens: &'a [CssToken<'a>],
) -> Option<(usize, &'a str, Option<&'a str>)> {
    tokens.iter().enumerate().find_map(|(idx, token)| {
        parse_font_size_token(token.text).map(|(size, line_height)| (idx, size, line_height))
    })
}

/// Divide `font-size` e `line-height` de tokens como `16px/1.4`.
fn parse_font_size_token(token: &str) -> Option<(&str, Option<&str>)> {
    let (size, line_height) = token.split_once('/').unwrap_or((token, ""));
    if !size.trim().ends_with("px") {
        return None;
    }
    parse_size_value(size)?;
    Some((
        size,
        if line_height.is_empty() {
            None
        } else {
            Some(line_height)
        },
    ))
}

/// Divide uma declaração CSS em tokens preservando strings e parênteses.
fn css_tokens(value: &str) -> Vec<CssToken<'_>> {
    let mut tokens = Vec::new();
    let mut start: Option<usize> = None;
    let mut quote: Option<char> = None;
    let mut paren_depth = 0usize;

    for (idx, ch) in value.char_indices() {
        match quote {
            Some(q) => {
                if ch == q {
                    quote = None;
                }
            }
            None => match ch {
                '"' | '\'' => quote = Some(ch),
                '(' => paren_depth += 1,
                ')' => paren_depth = paren_depth.saturating_sub(1),
                _ if ch.is_ascii_whitespace() && paren_depth == 0 => {
                    if let Some(s) = start.take() {
                        tokens.push(CssToken {
                            text: &value[s..idx],
                            start: s,
                        });
                    }
                    continue;
                }
                _ => {}
            },
        }
        if start.is_none() {
            start = Some(idx);
        }
    }

    if let Some(s) = start {
        tokens.push(CssToken {
            text: &value[s..],
            start: s,
        });
    }

    tokens
}

/// Parseia dimensões CSS usadas em width/height e min/max.
fn parse_width_value(value: &str) -> WidthValue {
    let value = strip_important(value);
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

/// Parseia o shorthand `border`, renderizando estilos não sólidos como sólido.
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
    use crate::types::{LineHeightValue, TextAlign, TextTransform};

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

    #[test]
    fn variable_names_with_shared_prefix_resolve_longest_first() {
        let styles = parse_scss(
            r#"
            $text: #c8c8e0;
            $text-dim: #6a6a8a;

            .subtitle {
                color: $text-dim;
            }
            "#,
        );

        let subtitle = styles.get("subtitle").expect("subtitle style");
        assert_eq!(subtitle.color, Some((106, 106, 138)));
    }

    #[test]
    fn important_suffix_is_stripped_before_property_dispatch() {
        let styles = parse_scss(
            r#"
            .alert {
                color: #e94560 !important;
                background: #12121f !important;
            }
            "#,
        );

        let alert = styles.get("alert").expect("alert style");
        assert_eq!(alert.color, Some((233, 69, 96)));
        assert_eq!(alert.background, Some((18, 18, 31)));
    }

    #[test]
    fn parses_text_and_font_rules_that_affect_measurement() {
        let styles = parse_scss(
            r#"
            .title {
                font-family: "Inter Tight", Arial, sans-serif;
                font-weight: 700;
                font-style: italic;
                line-height: 1.35;
                letter-spacing: 0.5px;
                text-transform: uppercase;
                text-align: center;
                text-decoration: underline line-through;
            }
            "#,
        );

        let title = styles.get("title").expect("title style");
        assert_eq!(
            title.font_family.as_deref(),
            Some(r#""Inter Tight", Arial, sans-serif"#)
        );
        assert_eq!(title.font_weight, Some(700));
        assert_eq!(title.font_italic, Some(true));
        assert_eq!(title.line_height, Some(LineHeightValue::Factor(1.35)));
        assert_eq!(title.letter_spacing, Some(0.5));
        assert_eq!(title.text_transform, Some(TextTransform::Uppercase));
        assert_eq!(title.text_align, Some(TextAlign::Center));
        assert_eq!(title.underline, Some(true));
        assert_eq!(title.strikethrough, Some(true));
    }

    #[test]
    fn parses_conventional_font_shorthand() {
        let styles = parse_scss(
            r#"
            .label {
                font: italic 600 16px/140% "Fira Sans", sans-serif;
            }
            "#,
        );

        let label = styles.get("label").expect("label style");
        assert_eq!(label.font_italic, Some(true));
        assert_eq!(label.font_weight, Some(600));
        assert_eq!(label.font_size, Some(16.0));
        assert_eq!(label.line_height, Some(LineHeightValue::Factor(1.4)));
        assert_eq!(
            label.font_family.as_deref(),
            Some(r#""Fira Sans", sans-serif"#)
        );
    }

    #[test]
    fn font_shorthand_without_size_unit_does_not_confuse_weight_for_size() {
        let styles = parse_scss(
            r#"
            .label {
                font: 600 16 Inter, sans-serif;
            }
            "#,
        );

        let label = styles.get("label").expect("label style");
        assert_eq!(label.font_size, None);
        assert_eq!(label.font_family, None);
    }
}
