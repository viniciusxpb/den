//! Parsers de valores CSS individuais usados pelo `parse_scss`.
//!
//! Cada função aqui parseia UM tipo de valor (font-weight, line-height, width,
//! border, position, etc) e retorna a representação tipada do `den_macros::types`.
//! Funções `apply_*` mutam o `StyleRule` direto pra shorthands com múltiplas
//! propriedades (`font`, `inset`).

use crate::parse::color::parse_color;
use crate::types::{
    AlignItems, BorderStyle, BoxShadow, FlexDirection, JustifyContent, LineHeightValue,
    OverflowKind, PositionKind, StyleRule, TextAlign, TextTransform, Transform2d, WidthValue,
};

/// Parseia tamanho Den/CSS em pixels, aceitando valor sem unidade ou `px`.
pub(super) fn parse_size_value(value: &str) -> Option<f32> {
    strip_important(value)
        .trim_end_matches("px")
        .parse::<f32>()
        .ok()
}

/// Remove sufixo `!important` sem alocar quando presente.
pub(super) fn strip_important(value: &str) -> &str {
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
pub(super) fn parse_font_family(value: &str) -> Option<String> {
    let value = strip_important(value).trim();
    if value.is_empty() {
        None
    } else {
        Some(value.to_string())
    }
}

/// Parseia pesos CSS convencionais para valor numérico.
pub(super) fn parse_font_weight(value: &str) -> Option<u16> {
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
pub(super) fn parse_font_style(value: &str) -> Option<bool> {
    let value = strip_important(value).trim().to_ascii_lowercase();
    match value.as_str() {
        "normal" => Some(false),
        "italic" | "oblique" => Some(true),
        _ => None,
    }
}

/// Parseia `line-height` como pixels absolutos ou fator multiplicador.
pub(super) fn parse_line_height(value: &str) -> Option<LineHeightValue> {
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
pub(super) fn parse_letter_spacing(value: &str) -> Option<f32> {
    if strip_important(value).eq_ignore_ascii_case("normal") {
        Some(0.0)
    } else {
        parse_size_value(value)
    }
}

/// Parseia a propriedade CSS `text-transform`.
pub(super) fn parse_text_transform(value: &str) -> Option<TextTransform> {
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
pub(super) fn parse_text_align(value: &str) -> Option<TextAlign> {
    let value = strip_important(value).trim().to_ascii_lowercase();
    match value.as_str() {
        "left" | "start" => Some(TextAlign::Left),
        "center" => Some(TextAlign::Center),
        "right" | "end" => Some(TextAlign::Right),
        _ => None,
    }
}

/// Parseia as linhas de decoração que o painter consegue representar.
pub(super) fn parse_text_decoration(value: &str) -> (Option<bool>, Option<bool>) {
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
pub(super) fn apply_font_shorthand(value: &str, rule: &mut StyleRule) {
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
pub(super) fn parse_width_value(value: &str) -> WidthValue {
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

/// Parseia `position: static|relative|absolute|fixed|sticky`.
/// `sticky` vira `Static` com warning (ainda não implementado).
pub(super) fn parse_position(value: &str) -> Option<PositionKind> {
    match strip_important(value) {
        "static" => Some(PositionKind::Static),
        "relative" => Some(PositionKind::Relative),
        "absolute" => Some(PositionKind::Absolute),
        "fixed" => Some(PositionKind::Fixed),
        "sticky" => {
            eprintln!("Den: `position: sticky` não é suportado ainda, caindo pra `static`");
            Some(PositionKind::Static)
        }
        other => {
            eprintln!("Den: `position: {other}` desconhecido, ignorando");
            None
        }
    }
}

/// Shorthand `inset: VAL` → top/right/bottom/left = VAL.
/// 2 valores = vertical horizontal. 3 = top horizontal bottom. 4 = top right bottom left.
pub(super) fn apply_inset_shorthand(value: &str, rule: &mut StyleRule) {
    let value = strip_important(value);
    let parts: Vec<&str> = value.split_whitespace().collect();
    let (t, r, b, l) = match parts.len() {
        1 => (parts[0], parts[0], parts[0], parts[0]),
        2 => (parts[0], parts[1], parts[0], parts[1]),
        3 => (parts[0], parts[1], parts[2], parts[1]),
        4 => (parts[0], parts[1], parts[2], parts[3]),
        _ => {
            eprintln!("Den: `inset: {value}` com número inválido de valores, ignorando");
            return;
        }
    };
    rule.top = parse_offset_value(t);
    rule.right = parse_offset_value(r);
    rule.bottom = parse_offset_value(b);
    rule.left = parse_offset_value(l);
}

/// Parseia um offset (`top`/`left`/`right`/`bottom`).
///
/// Diferente de `parse_width_value`: `auto` (explícito) e valores não reconhecidos
/// retornam `None`, não `Some(WidthValue::Auto)`. Isso impede que `auto` mascare
/// como "0" no layout — `None` significa "anchor não fornecido", deixando o engine
/// decidir pelo lado oposto se setado.
pub(super) fn parse_offset_value(value: &str) -> Option<WidthValue> {
    let trimmed = strip_important(value).trim();
    if trimmed == "auto" {
        return None;
    }
    if let Some(pct) = trimmed.strip_suffix('%')
        && let Ok(v) = pct.trim().parse::<f32>()
    {
        return Some(WidthValue::Percent(v / 100.0));
    }
    parse_size_value(trimmed).map(WidthValue::Px)
}

/// Parseia o shorthand `border: <width> <style> <color>`.
/// Estilo não sólido cai pra `solid` com warning.
/// Resulta em borda uniforme nos 4 lados.
pub(super) fn parse_border_value(value: &str) -> Option<BorderStyle> {
    let (width, color) = parse_border_shorthand_parts(value)?;
    Some(BorderStyle::uniform(width, color))
}

/// Parseia os 3 tokens de `<width> <style> <color>` e retorna `(width, color)`.
/// Compartilhado entre o shorthand uniforme e os shorthands per-side
/// (`border-left: 1px solid #...`).
pub(super) fn parse_border_shorthand_parts(value: &str) -> Option<(f32, crate::types::RgbColor)> {
    let parts: Vec<&str> = value.split_whitespace().collect();
    if parts.len() < 3 {
        return None;
    }
    let width = parse_size_value(parts[0])?;
    let style = parts[1];
    if style != "solid" {
        eprintln!("Den: border style '{style}' is not supported, rendering as solid");
    }
    let color = parse_color(parts[2])?;
    Some((width, color))
}

/// Aplica `border-<side>: <width> <style> <color>` na rule existente.
/// Atualiza só o slot do lado afetado em `widths` e a cor (compartilhada).
/// Se ainda não há `border` nesta rule, inicializa zerada antes de setar o slot.
pub(super) fn apply_border_side_shorthand(side_index: usize, value: &str, rule: &mut StyleRule) {
    let Some((width, color)) = parse_border_shorthand_parts(value) else {
        return;
    };
    let border = rule.border.get_or_insert_with(BorderStyle::default_zero);
    border.widths[side_index] = width;
    border.color = color;
}

/// Aplica `border-<side>-width: <width>` na rule existente. Inicializa zerado se necessário.
pub(super) fn apply_border_side_width(side_index: usize, value: &str, rule: &mut StyleRule) {
    let Some(w) = parse_size_value(value) else {
        return;
    };
    let border = rule.border.get_or_insert_with(BorderStyle::default_zero);
    border.widths[side_index] = w;
}

/// Aplica `border-<side>-color: <color>` na rule existente.
/// Como o MVP usa cor única compartilhada, o último `border-*-color` vence.
pub(super) fn apply_border_side_color(value: &str, rule: &mut StyleRule) {
    let Some(c) = parse_color(value) else {
        return;
    };
    let border = rule.border.get_or_insert_with(BorderStyle::default_zero);
    border.color = c;
}

/// Aplica `border-color: <color>` (forma uniforme).
pub(super) fn apply_border_color(value: &str, rule: &mut StyleRule) {
    apply_border_side_color(value, rule);
}

/// Aplica `border-width: <w>` (forma uniforme).
pub(super) fn apply_border_width(value: &str, rule: &mut StyleRule) {
    let Some(w) = parse_size_value(value) else {
        return;
    };
    let border = rule.border.get_or_insert_with(BorderStyle::default_zero);
    border.widths = [w; 4];
}

impl BorderStyle {
    /// Construtor de default zerado pra uso em `get_or_insert_with` quando o
    /// usuário declara só um per-side antes de qualquer `border:` shorthand.
    /// Diferente de `Default::default()`, começa com widths `[0; 4]` (não `[1; 4]`)
    /// pra evitar materializar borda em lados que o usuário não declarou.
    pub fn default_zero() -> Self {
        Self {
            widths: [0.0; 4],
            color: (0, 0, 0, 255),
        }
    }
}

/// Helper zero-alloc pra match case-insensitive de keywords CSS.
///
/// Itera `keywords` e retorna o primeiro valor cujo `key` casa case-insensitive
/// com `value`. Sem alocação (compara byte-a-byte via `eq_ignore_ascii_case`).
/// Caller controla o handling do `None` (warning customizado).
fn match_keyword<T: Copy>(value: &str, keywords: &[(&str, T)]) -> Option<T> {
    keywords
        .iter()
        .find(|(key, _)| value.eq_ignore_ascii_case(key))
        .map(|(_, val)| *val)
}

/// Parseia `flex-direction: row | column`. `row-reverse`/`column-reverse` caem
/// no eixo equivalente sem reverso (com warning) — reverse não implementado.
pub(super) fn parse_flex_direction(value: &str) -> Option<FlexDirection> {
    let trimmed = strip_important(value).trim();
    if let Some(direction) = match_keyword(
        trimmed,
        &[
            ("row", FlexDirection::Row),
            ("column", FlexDirection::Column),
        ],
    ) {
        return Some(direction);
    }
    if trimmed.eq_ignore_ascii_case("row-reverse") {
        eprintln!("Den: `flex-direction: row-reverse` não suportado, caindo pra `row`");
        return Some(FlexDirection::Row);
    }
    if trimmed.eq_ignore_ascii_case("column-reverse") {
        eprintln!("Den: `flex-direction: column-reverse` não suportado, caindo pra `column`");
        return Some(FlexDirection::Column);
    }
    eprintln!("Den: `flex-direction: {trimmed}` desconhecido, ignorando");
    None
}

/// Parseia `align-items: stretch | flex-start | center | flex-end`.
/// `start`/`end` (CSS Box Alignment) viram `flex-start`/`flex-end`.
/// `baseline` cai em `flex-start` com warning.
pub(super) fn parse_align_items(value: &str) -> Option<AlignItems> {
    let trimmed = strip_important(value).trim();
    if let Some(align) = match_keyword(
        trimmed,
        &[
            ("stretch", AlignItems::Stretch),
            ("flex-start", AlignItems::FlexStart),
            ("start", AlignItems::FlexStart),
            ("center", AlignItems::Center),
            ("flex-end", AlignItems::FlexEnd),
            ("end", AlignItems::FlexEnd),
        ],
    ) {
        return Some(align);
    }
    if trimmed.eq_ignore_ascii_case("baseline") {
        eprintln!("Den: `align-items: baseline` não suportado, caindo pra `flex-start`");
        return Some(AlignItems::FlexStart);
    }
    eprintln!("Den: `align-items: {trimmed}` desconhecido, ignorando");
    None
}

/// Parseia `overflow: visible | hidden`. `scroll`/`auto` caem em visible + warning.
pub(super) fn parse_overflow(value: &str) -> Option<OverflowKind> {
    let trimmed = strip_important(value).trim();
    if trimmed.eq_ignore_ascii_case("visible") {
        return Some(OverflowKind::Visible);
    }
    if trimmed.eq_ignore_ascii_case("hidden") {
        return Some(OverflowKind::Hidden);
    }
    if trimmed.eq_ignore_ascii_case("scroll") || trimmed.eq_ignore_ascii_case("auto") {
        eprintln!(
            "Den: `overflow: {trimmed}` não suportado (sem scroll nativo), caindo pra `visible`",
        );
        return Some(OverflowKind::Visible);
    }
    eprintln!("Den: `overflow: {trimmed}` desconhecido, ignorando");
    None
}

/// Parseia `transform: rotate(Ndeg|Nrad|Nturn)`. MVP: só `rotate()` suportado;
/// `scale`/`translate`/`matrix`/múltiplos aninhados caem com warning.
///
/// Retorna `Some(Transform2d)` com a rotação em radianos, ou `None` se falha.
pub(super) fn parse_transform(value: &str) -> Option<Transform2d> {
    let trimmed = strip_important(value).trim();
    // MVP: detecta SÓ `rotate(...)` no começo. Outras funções passam um
    // warning + retornam None (cai pra default identity).
    if let Some(inner) = strip_func_ci(trimmed, "rotate") {
        let angle_rad = parse_rotation_angle(inner.trim())?;
        return Some(Transform2d {
            rotation_rad: angle_rad,
        });
    }
    if trimmed.eq_ignore_ascii_case("none") {
        return Some(Transform2d { rotation_rad: 0.0 });
    }
    eprintln!(
        "Den: `transform: {trimmed}` não suportado (MVP só aceita `rotate()`), ignorando",
    );
    None
}

/// Case-insensitive check pra `name(ARGS)` — retorna `ARGS` ou `None`.
/// Versão local aqui (não usa `parse::color::strip_func` pra não tornar pub).
fn strip_func_ci<'a>(value: &'a str, name: &str) -> Option<&'a str> {
    if value.len() < name.len() + 2 {
        return None;
    }
    let (head, rest) = value.split_at(name.len());
    if !head.eq_ignore_ascii_case(name) {
        return None;
    }
    let rest = rest.trim_start();
    rest.strip_prefix('(')?.strip_suffix(')')
}

/// Parseia ângulo CSS: `Ndeg` / `Nrad` / `Nturn` / `Ngrad`. Número sem unidade
/// é tratado como graus (permissivo; CSS spec pede unidade, mas o warning no
/// parser geral é suficiente).
///
/// **Ordem importa**: `grad` DEVE ser checado antes de `rad` (pois "grad"
/// termina em "rad") e `turn` antes de qualquer substring dele.
fn parse_rotation_angle(raw: &str) -> Option<f32> {
    let trimmed = raw.trim();
    if let Some(grad_str) = trimmed.strip_suffix("grad") {
        // 1 gradiano = π/200 radianos. CHECK ANTES de `rad` (grad ends with rad).
        return grad_str
            .trim()
            .parse::<f32>()
            .ok()
            .map(|v| v * std::f32::consts::PI / 200.0);
    }
    if let Some(turn_str) = trimmed.strip_suffix("turn") {
        return turn_str
            .trim()
            .parse::<f32>()
            .ok()
            .map(|v| v * std::f32::consts::TAU);
    }
    if let Some(deg_str) = trimmed.strip_suffix("deg") {
        return deg_str.trim().parse::<f32>().ok().map(|v| v.to_radians());
    }
    if let Some(rad_str) = trimmed.strip_suffix("rad") {
        return rad_str.trim().parse::<f32>().ok();
    }
    // Fallback permissivo: trata número puro como graus.
    trimmed.parse::<f32>().ok().map(|v| v.to_radians())
}

/// Parseia `justify-content: flex-start | center | flex-end | space-between
/// | space-around | space-evenly`. `start`/`end` viram `flex-start`/`flex-end`.
pub(super) fn parse_justify_content(value: &str) -> Option<JustifyContent> {
    let trimmed = strip_important(value).trim();
    if let Some(justify) = match_keyword(
        trimmed,
        &[
            ("flex-start", JustifyContent::FlexStart),
            ("start", JustifyContent::FlexStart),
            ("center", JustifyContent::Center),
            ("flex-end", JustifyContent::FlexEnd),
            ("end", JustifyContent::FlexEnd),
            ("space-between", JustifyContent::SpaceBetween),
            ("space-around", JustifyContent::SpaceAround),
            ("space-evenly", JustifyContent::SpaceEvenly),
        ],
    ) {
        return Some(justify);
    }
    eprintln!("Den: `justify-content: {trimmed}` desconhecido, ignorando");
    None
}

/// Parseia `box-shadow: none | <shadow> [, <shadow>...]` numa lista de [`BoxShadow`].
///
/// Cada `<shadow>` segue a forma CSS:
/// `[inset] <offset-x> <offset-y> [<blur>] [<spread>] <color>`
///
/// `inset` é opcional (default = drop shadow externo). `blur` e `spread` default
/// `0`. A cor pode ser hex, `rgb()`, `rgba()` ou named — passa por `parse_color`.
///
/// Retorno: `Some(vec![..])` pra lista, `Some(vec![])` pra `none` explícito (a
/// distinção de `None`/`Some(vec![])` importa pra cascade — `:hover` com
/// `box-shadow: none` precisa cancelar a sombra base, e isso só funciona se o
/// merge_from souber que algo foi declarado). `None` só quando todos os
/// `<shadow>` individuais falham no parse.
pub(super) fn parse_box_shadow_value(value: &str) -> Option<Vec<BoxShadow>> {
    let trimmed = strip_important(value).trim();
    if trimmed.eq_ignore_ascii_case("none") || trimmed.is_empty() {
        return Some(Vec::new());
    }
    let shadows: Vec<BoxShadow> = split_top_level_commas(trimmed)
        .into_iter()
        .filter_map(|part| parse_single_box_shadow(part.trim()))
        .collect();
    if shadows.is_empty() {
        // Todos os tokens falharam — não declarou nada parseável.
        None
    } else {
        Some(shadows)
    }
}

/// Quebra `value` em segmentos separados por vírgulas no nível 0 — ignora
/// vírgulas dentro de `rgba(...)` / `rgb(...)`. Necessário porque `box-shadow`
/// aceita lista (vírgula) e cada item pode ter `rgba(r, g, b, a)` (vírgula).
fn split_top_level_commas(input: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '(' => depth += 1,
            ')' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                parts.push(&input[start..idx]);
                start = idx + 1;
            }
            _ => {}
        }
    }
    parts.push(&input[start..]);
    parts
}

/// Parseia UM `<shadow>` individual. Retorna `None` se faltar campo obrigatório
/// (ao menos `offset-x`, `offset-y`, `color`).
fn parse_single_box_shadow(raw: &str) -> Option<BoxShadow> {
    // Tokens são separados por whitespace, mas precisamos preservar `rgba(...)`
    // como um único token porque tem espaços/vírgulas internos.
    let tokens = tokenize_shadow(raw);
    if tokens.len() < 3 {
        eprintln!("Den: box-shadow inválido '{raw}' (mínimo: <x> <y> <color>)");
        return None;
    }

    let mut inset = false;
    let mut idx = 0;
    if tokens[idx].eq_ignore_ascii_case("inset") {
        inset = true;
        idx += 1;
    }

    // Próximos 2-4 tokens são números (px); depois vem a cor (último).
    let lengths_start = idx;
    let color_token = tokens.last()?;
    let color = parse_color(color_token)?;
    let lengths_end = tokens.len() - 1;

    // Trailing `inset` também é válido em algumas variantes CSS.
    let mut lengths = Vec::new();
    for tk in &tokens[lengths_start..lengths_end] {
        if tk.eq_ignore_ascii_case("inset") {
            inset = true;
            continue;
        }
        lengths.push(parse_size_value(tk)?);
    }

    if lengths.len() < 2 {
        eprintln!("Den: box-shadow precisa de offset-x e offset-y em '{raw}'");
        return None;
    }
    let offset_x = lengths[0];
    let offset_y = lengths[1];
    let blur = lengths.get(2).copied().unwrap_or(0.0);
    let spread = lengths.get(3).copied().unwrap_or(0.0);

    Some(BoxShadow {
        offset_x,
        offset_y,
        blur,
        spread,
        color,
        inset,
    })
}

/// Tokeniza uma `box-shadow` única preservando `rgba(...)`/`rgb(...)` inteiros.
fn tokenize_shadow(input: &str) -> Vec<&str> {
    let mut tokens = Vec::new();
    let mut depth = 0usize;
    let mut start: Option<usize> = None;
    for (idx, ch) in input.char_indices() {
        match ch {
            '(' => {
                depth += 1;
                if start.is_none() {
                    start = Some(idx);
                }
            }
            ')' => depth = depth.saturating_sub(1),
            c if c.is_ascii_whitespace() && depth == 0 => {
                if let Some(s) = start.take() {
                    tokens.push(&input[s..idx]);
                }
            }
            _ => {
                if start.is_none() {
                    start = Some(idx);
                }
            }
        }
    }
    if let Some(s) = start {
        tokens.push(&input[s..]);
    }
    tokens
}
