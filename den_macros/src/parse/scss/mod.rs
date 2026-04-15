//! Parser SCSS mínimo usado em compile time pelo pipeline Den.
//!
//! Estrutura:
//! - [`mod@lexer`] — helpers byte-level (whitespace, comments, identifiers).
//! - [`mod@variables`] — coleta `$nome: valor;` e resolução de referências.
//! - [`mod@values`] — parsers individuais (font, border, position, width, etc).
//! - este `mod.rs` — loop principal: itera regras, dispatcha por property name.

use crate::parse::color::parse_hex_color;
use crate::types::{DisplayMode, StyleMap, StyleRule};

mod lexer;
mod values;
mod variables;

use lexer::{
    is_ident_char, read_css_identifier, read_identifier, skip_comment, skip_invalid_declaration,
    skip_whitespace,
};
use values::{
    apply_font_shorthand, apply_inset_shorthand, parse_border_value, parse_font_family,
    parse_font_style, parse_font_weight, parse_letter_spacing, parse_line_height,
    parse_offset_value, parse_position, parse_size_value, parse_text_align, parse_text_decoration,
    parse_text_transform, parse_width_value, strip_important,
};
use variables::{collect_variables, resolve_vars};

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

            apply_property(&mut rule, &prop_name, value);
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

/// Despacha uma propriedade CSS pro slot certo do `StyleRule`.
///
/// Match grande mas simples — cada arm é uma propriedade. Manter aqui (em vez
/// de espalhar por múltiplos arquivos) facilita o "qual properties Den suporta?"
/// virar uma busca de uma linha só.
fn apply_property(rule: &mut StyleRule, prop_name: &str, value: &str) {
    match prop_name {
        "color" => rule.color = parse_hex_color(value),
        "font-size" => rule.font_size = parse_size_value(value),
        "font-family" => rule.font_family = parse_font_family(value),
        "font-weight" => rule.font_weight = parse_font_weight(value),
        "font-style" => rule.font_italic = parse_font_style(value),
        "font" => apply_font_shorthand(value, rule),
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
        "position" => {
            if let Some(pos) = parse_position(value) {
                rule.position = Some(pos);
            }
        }
        "top" => rule.top = parse_offset_value(value),
        "left" => rule.left = parse_offset_value(value),
        "right" => rule.right = parse_offset_value(value),
        "bottom" => rule.bottom = parse_offset_value(value),
        "z-index" => {
            if let Ok(n) = value.trim().parse::<i32>() {
                rule.z_index = Some(n);
            }
        }
        "inset" => apply_inset_shorthand(value, rule),
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::parse_scss;
    use crate::types::{
        LineHeightValue, PositionKind, StyleRule, TextAlign, TextTransform, WidthValue,
    };

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

    // ---- position / inset / z-index ----

    #[test]
    fn parses_position_absolute() {
        let s = parse_scss(".n { position: absolute; }");
        assert_eq!(s.get("n").unwrap().position, Some(PositionKind::Absolute));
    }

    #[test]
    fn parses_position_relative_fixed_static() {
        let s = parse_scss(
            r#"
            .a { position: relative; }
            .b { position: fixed; }
            .c { position: static; }
            "#,
        );
        assert_eq!(s.get("a").unwrap().position, Some(PositionKind::Relative));
        assert_eq!(s.get("b").unwrap().position, Some(PositionKind::Fixed));
        assert_eq!(s.get("c").unwrap().position, Some(PositionKind::Static));
    }

    #[test]
    fn unknown_position_value_is_ignored() {
        let s = parse_scss(".n { position: banana; }");
        // Valor inválido → propriedade fica None (não decisiva).
        assert_eq!(s.get("n").unwrap().position, None);
    }

    #[test]
    fn position_sticky_fallback_to_static() {
        let s = parse_scss(".n { position: sticky; }");
        // Sticky cai pra Static com warning (printado no eprintln).
        assert_eq!(s.get("n").unwrap().position, Some(PositionKind::Static));
    }

    #[test]
    fn parses_individual_offsets() {
        let s = parse_scss(
            r#".n {
                top: 10px;
                left: 50%;
                right: -6px;
                bottom: 0;
            }"#,
        );
        let n = s.get("n").unwrap();
        assert_eq!(n.top, Some(WidthValue::Px(10.0)));
        assert_eq!(n.left, Some(WidthValue::Percent(0.5)));
        assert_eq!(n.right, Some(WidthValue::Px(-6.0)));
        assert_eq!(n.bottom, Some(WidthValue::Px(0.0)));
    }

    #[test]
    fn offset_auto_resolves_to_none() {
        // Per spec, `top: auto` num positioned não-anchor — engine deve ignorar
        // (e ancorar pelo `bottom`). Parser converte pra None pra impedir que
        // chegue como `Some(Auto)` no layout (que trataria como 0).
        let s = parse_scss(".n { top: auto; left: 10px; }");
        let n = s.get("n").unwrap();
        assert_eq!(n.top, None);
        assert_eq!(n.left, Some(WidthValue::Px(10.0)));
    }

    #[test]
    fn parses_z_index() {
        let s = parse_scss(".n { z-index: 5; }");
        assert_eq!(s.get("n").unwrap().z_index, Some(5));
    }

    #[test]
    fn parses_negative_z_index() {
        let s = parse_scss(".n { z-index: -1; }");
        assert_eq!(s.get("n").unwrap().z_index, Some(-1));
    }

    #[test]
    fn invalid_z_index_is_ignored() {
        let s = parse_scss(".n { z-index: auto; }");
        assert_eq!(s.get("n").unwrap().z_index, None);
    }

    #[test]
    fn inset_shorthand_one_value() {
        let s = parse_scss(".n { inset: 10px; }");
        let n = s.get("n").unwrap();
        assert_eq!(n.top, Some(WidthValue::Px(10.0)));
        assert_eq!(n.right, Some(WidthValue::Px(10.0)));
        assert_eq!(n.bottom, Some(WidthValue::Px(10.0)));
        assert_eq!(n.left, Some(WidthValue::Px(10.0)));
    }

    #[test]
    fn inset_shorthand_two_values() {
        // `inset: V H` → top=bottom=V, right=left=H
        let s = parse_scss(".n { inset: 5px 20px; }");
        let n = s.get("n").unwrap();
        assert_eq!(n.top, Some(WidthValue::Px(5.0)));
        assert_eq!(n.bottom, Some(WidthValue::Px(5.0)));
        assert_eq!(n.right, Some(WidthValue::Px(20.0)));
        assert_eq!(n.left, Some(WidthValue::Px(20.0)));
    }

    #[test]
    fn inset_shorthand_four_values_clockwise() {
        // `inset: T R B L` (TRBL clockwise)
        let s = parse_scss(".n { inset: 1px 2px 3px 4px; }");
        let n = s.get("n").unwrap();
        assert_eq!(n.top, Some(WidthValue::Px(1.0)));
        assert_eq!(n.right, Some(WidthValue::Px(2.0)));
        assert_eq!(n.bottom, Some(WidthValue::Px(3.0)));
        assert_eq!(n.left, Some(WidthValue::Px(4.0)));
    }

    #[test]
    fn inset_zero_works() {
        // Caso comum: overlay full-screen via `inset: 0`.
        let s = parse_scss(".overlay { position: fixed; inset: 0; }");
        let n = s.get("overlay").unwrap();
        assert_eq!(n.top, Some(WidthValue::Px(0.0)));
        assert_eq!(n.left, Some(WidthValue::Px(0.0)));
    }

    #[test]
    fn merge_position_static_overrides_absolute() {
        // Cenário do code review item 9: classe mais específica seta `static`
        // pra cancelar `absolute` herdado de classe anterior. `Option`-based
        // merge respeita isso; antes (com `if != Static`) silenciava o reset.
        let mut a = StyleRule {
            position: Some(PositionKind::Absolute),
            ..Default::default()
        };
        let b = StyleRule {
            position: Some(PositionKind::Static),
            ..Default::default()
        };
        a.merge_from(&b);
        assert_eq!(a.position, Some(PositionKind::Static));
    }

    #[test]
    fn merge_position_none_does_not_overwrite() {
        // Classe sem position declarada não pode anular um absolute herdado.
        let mut a = StyleRule {
            position: Some(PositionKind::Absolute),
            ..Default::default()
        };
        let b = StyleRule::default(); // position = None
        a.merge_from(&b);
        assert_eq!(a.position, Some(PositionKind::Absolute));
    }
}
