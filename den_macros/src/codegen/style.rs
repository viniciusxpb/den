//! Conversão de `DenVisual` para tokens de `PaintStyle` e `LayoutIntent`.
//!
//! Isola o mapeamento visual/layout do emitter principal. Toda propriedade
//! SCSS que é expressa como campo de `PaintStyle` ou `LayoutIntent` mora aqui.

use super::config::{
    AVERAGE_GLYPH_WIDTH_RATIO, DEFAULT_EXPR_TEXT_WIDTH, DEFAULT_INPUT_LINE_HEIGHT,
    DEFAULT_INPUT_WIDTH, DEFAULT_TEXT_LINE_HEIGHT,
};
use super::flex::{has_flex_grow, is_flex_container};
use crate::types::{
    AlignItems, DenElement, DenVisual, DisplayMode, FlexDirection, JustifyContent,
    LineHeightValue, OverflowKind, TextAlign, TextSegment, TextTransform, Transform2d, WidthValue,
};
use proc_macro2::Span;
use quote::quote;
use syn::LitStr;

/// Emite um literal `den_layout::PaintStyle { .. }` para o visual dado.
pub(crate) fn paint_style_tokens(visual: &DenVisual) -> proc_macro2::TokenStream {
    let color = quote_opt_rgb(visual.color);
    let background = quote_opt_rgb(visual.background);
    let (border_color, border_widths) = match visual.border {
        Some(b) => {
            let (r, g, b_, a) = b.color;
            let [t, ri, bo, le] = b.widths;
            (
                quote! { Some((#r, #g, #b_, #a)) },
                quote! { [#t as f32, #ri as f32, #bo as f32, #le as f32] },
            )
        }
        None => (quote! { None }, quote! { [0.0f32; 4] }),
    };
    let border_radius = visual.border_radius.unwrap_or(0.0);
    let font_size = visual.font_size.unwrap_or(0.0);
    let font_family = quote_opt_static_str(visual.font_family.as_deref());
    let font_weight = visual.font_weight.unwrap_or(400);
    let font_italic = visual.font_italic.unwrap_or(false);
    let (line_height, line_height_factor) = match visual.line_height {
        Some(LineHeightValue::Px(value)) => (value, 0.0),
        Some(LineHeightValue::Factor(value)) => (0.0, value),
        None => (0.0, 0.0),
    };
    let letter_spacing = visual.letter_spacing.unwrap_or(0.0);
    let text_transform = text_transform_tokens(visual.text_transform.unwrap_or_default());
    let text_align = text_align_tokens(visual.text_align.unwrap_or_default());
    let underline = visual.underline.unwrap_or(false);
    let strikethrough = visual.strikethrough.unwrap_or(false);
    let cursor_pointer = visual.cursor_pointer.unwrap_or(false);
    let opacity = visual.opacity.unwrap_or(1.0);
    let white_space_nowrap = visual.white_space_nowrap.unwrap_or(false);
    let text_overflow_ellipsis = visual.text_overflow_ellipsis.unwrap_or(false);
    // Default aplicado AQUI (codegen, antes do runtime) — Option preservado em
    // DenVisual pra cascade. None ou Some(vec![]) ambos viram Vec::new() no paint.
    let box_shadows = box_shadows_tokens(visual.box_shadows.as_deref().unwrap_or_default());
    // Overflow: `None` ou `Some(Visible)` viram `false`; `Some(Hidden)` = `true`.
    let overflow_hidden = matches!(visual.overflow, Some(OverflowKind::Hidden));
    let transform_tokens = transform_tokens(visual.transform);

    quote! {
        den_layout::PaintStyle {
            color: #color,
            background: #background,
            border_color: #border_color,
            border_widths: #border_widths,
            border_radius: #border_radius as f32,
            font_size: #font_size as f32,
            font_family: #font_family,
            font_weight: #font_weight,
            font_italic: #font_italic,
            line_height: #line_height as f32,
            line_height_factor: #line_height_factor as f32,
            letter_spacing: #letter_spacing as f32,
            text_transform: #text_transform,
            text_align: #text_align,
            underline: #underline,
            strikethrough: #strikethrough,
            cursor_pointer: #cursor_pointer,
            opacity: #opacity as f32,
            white_space_nowrap: #white_space_nowrap,
            text_overflow_ellipsis: #text_overflow_ellipsis,
            box_shadows: #box_shadows,
            overflow_hidden: #overflow_hidden,
            transform: #transform_tokens,
        }
    }
}

/// Emite `Option<Transform2d>` pro `PaintStyle`. `None` quando não declarado
/// OU quando a transformação é identity (sem rotação) — assim o paint pode
/// pular o codepath de mesh rotated sem checar `.is_identity()` em runtime.
///
/// Emite `(N_f32).to_radians()` em vez do literal radian convertido pra evitar
/// que clippy confunda ângulos comuns (π/4, π/2) com constantes da stdlib
/// (`FRAC_PI_4`, `FRAC_PI_2`). O `to_radians()` é trivial (multiply por π/180)
/// e roda uma vez por frame por nó rotacionado.
fn transform_tokens(transform: Option<Transform2d>) -> proc_macro2::TokenStream {
    match transform {
        Some(t) if !t.is_identity() => {
            let rotation_deg = t.rotation_rad.to_degrees();
            quote! {
                Some(den_layout::Transform2d {
                    rotation_rad: (#rotation_deg as f32).to_radians(),
                })
            }
        }
        _ => quote! { None },
    }
}

/// Emite `vec![BoxShadow { .. }, ..]` ou `Vec::new()` quando vazio.
fn box_shadows_tokens(shadows: &[crate::types::BoxShadow]) -> proc_macro2::TokenStream {
    if shadows.is_empty() {
        return quote! { ::std::vec::Vec::new() };
    }
    let items = shadows.iter().map(|shadow| {
        let (r, g, b, a) = shadow.color;
        let offset_x = shadow.offset_x;
        let offset_y = shadow.offset_y;
        let blur = shadow.blur;
        let spread = shadow.spread;
        let inset = shadow.inset;
        quote! {
            den_layout::BoxShadow {
                offset_x: #offset_x as f32,
                offset_y: #offset_y as f32,
                blur: #blur as f32,
                spread: #spread as f32,
                color: (#r, #g, #b, #a),
                inset: #inset,
            }
        }
    });
    quote! { vec![ #( #items ),* ] }
}

/// Emite `Option<PaintStyle>` para o override de hover, se existir.
pub(super) fn hover_style_tokens(visual: &DenVisual) -> proc_macro2::TokenStream {
    match &visual.hover_override {
        Some(_) => {
            let hovered = visual.resolve_hover();
            let hs = paint_style_tokens(&hovered);
            quote! { Some(#hs) }
        }
        None => quote! { None },
    }
}

/// Emite um literal `den_layout::LayoutIntent { .. }` para o elemento dado.
pub(super) fn layout_intent_tokens(el: &DenElement) -> proc_macro2::TokenStream {
    let visual = &el.visual;
    // Defaults aplicados aqui (no codegen, ANTES do runtime) — DenVisual mantém
    // Option pra preservar cascade. Ver regra `Option<T>` em types/style.rs.
    let width_rule = dimension_rule_tokens(visual.width.unwrap_or_default());
    let height_rule = dimension_rule_tokens(visual.height.unwrap_or_default());
    let min_width = optional_dimension_tokens(visual.min_width);
    let max_width = optional_dimension_tokens(visual.max_width);
    let min_height = optional_dimension_tokens(visual.min_height);
    let max_height = optional_dimension_tokens(visual.max_height);
    let display = display_mode_tokens(if is_flex_container(el) {
        DisplayMode::Flex
    } else {
        visual.display.unwrap_or_default()
    });
    let padding = visual.padding.unwrap_or(0.0);
    let margin = visual.margin.unwrap_or(0.0);
    let gap = visual.gap.unwrap_or(0.0);
    let [bw_t, bw_r, bw_b, bw_l] = visual.border.map(|b| b.widths).unwrap_or([0.0; 4]);
    let flex_grow: f32 = if has_flex_grow(el) { 1.0 } else { 0.0 };
    let intrinsic_width = intrinsic_width_for(el);
    let intrinsic_height = intrinsic_height_for(el);
    let position = position_tokens(visual.position.unwrap_or_default());
    let top = optional_dimension_tokens(visual.top);
    let left = optional_dimension_tokens(visual.left);
    let right = optional_dimension_tokens(visual.right);
    let bottom = optional_dimension_tokens(visual.bottom);
    let z_index = match visual.z_index {
        Some(z) => quote! { Some(#z) },
        None => quote! { None },
    };
    let flex_direction = flex_direction_tokens(visual.flex_direction.unwrap_or_default());
    let align_items = align_items_tokens(visual.align_items.unwrap_or_default());
    let justify_content = justify_content_tokens(visual.justify_content.unwrap_or_default());

    quote! {
        den_layout::LayoutIntent {
            width_rule: #width_rule,
            height_rule: #height_rule,
            min_width: #min_width,
            max_width: #max_width,
            min_height: #min_height,
            max_height: #max_height,
            display: #display,
            padding: #padding as f32,
            border_widths: [#bw_t as f32, #bw_r as f32, #bw_b as f32, #bw_l as f32],
            margin: #margin as f32,
            gap: #gap as f32,
            flex_grow: #flex_grow as f32,
            intrinsic_width: #intrinsic_width as f32,
            intrinsic_height: #intrinsic_height as f32,
            position: #position,
            top: #top,
            left: #left,
            right: #right,
            bottom: #bottom,
            z_index: #z_index,
            flex_direction: #flex_direction,
            align_items: #align_items,
            justify_content: #justify_content,
        }
    }
}

/// Emite o token equivalente ao `den_layout::FlexDirection` espelhado.
fn flex_direction_tokens(direction: FlexDirection) -> proc_macro2::TokenStream {
    match direction {
        FlexDirection::Row => quote! { den_layout::FlexDirection::Row },
        FlexDirection::Column => quote! { den_layout::FlexDirection::Column },
    }
}

/// Emite o token equivalente ao `den_layout::AlignItems` espelhado.
fn align_items_tokens(align: AlignItems) -> proc_macro2::TokenStream {
    match align {
        AlignItems::Stretch => quote! { den_layout::AlignItems::Stretch },
        AlignItems::FlexStart => quote! { den_layout::AlignItems::FlexStart },
        AlignItems::Center => quote! { den_layout::AlignItems::Center },
        AlignItems::FlexEnd => quote! { den_layout::AlignItems::FlexEnd },
    }
}

/// Emite o token equivalente ao `den_layout::JustifyContent` espelhado.
fn justify_content_tokens(justify: JustifyContent) -> proc_macro2::TokenStream {
    match justify {
        JustifyContent::FlexStart => quote! { den_layout::JustifyContent::FlexStart },
        JustifyContent::Center => quote! { den_layout::JustifyContent::Center },
        JustifyContent::FlexEnd => quote! { den_layout::JustifyContent::FlexEnd },
        JustifyContent::SpaceBetween => quote! { den_layout::JustifyContent::SpaceBetween },
        JustifyContent::SpaceAround => quote! { den_layout::JustifyContent::SpaceAround },
        JustifyContent::SpaceEvenly => quote! { den_layout::JustifyContent::SpaceEvenly },
    }
}

fn position_tokens(p: crate::types::PositionKind) -> proc_macro2::TokenStream {
    use crate::types::PositionKind;
    match p {
        PositionKind::Static => quote! { den_layout::PositionKind::Static },
        PositionKind::Relative => quote! { den_layout::PositionKind::Relative },
        PositionKind::Absolute => quote! { den_layout::PositionKind::Absolute },
        PositionKind::Fixed => quote! { den_layout::PositionKind::Fixed },
    }
}

/// Emite `Option<DimensionRule>` pra min/max-width/height.
pub(super) fn optional_dimension_tokens(w: Option<WidthValue>) -> proc_macro2::TokenStream {
    match w {
        None => quote! { None },
        Some(v) => {
            let rule = dimension_rule_tokens(v);
            quote! { Some(#rule) }
        }
    }
}

pub(super) fn dimension_rule_tokens(w: WidthValue) -> proc_macro2::TokenStream {
    match w {
        WidthValue::Auto => quote! { den_layout::DimensionRule::Auto },
        WidthValue::Px(v) => quote! { den_layout::DimensionRule::Px(#v) },
        WidthValue::Percent(v) => quote! { den_layout::DimensionRule::Percent(#v) },
    }
}

pub(super) fn display_mode_tokens(d: DisplayMode) -> proc_macro2::TokenStream {
    match d {
        DisplayMode::Flex => quote! { den_layout::DisplayMode::Flex },
        DisplayMode::Grid => quote! { den_layout::DisplayMode::Grid },
        DisplayMode::Block => quote! { den_layout::DisplayMode::Block },
    }
}

pub(super) fn quote_opt_rgb(opt: Option<(u8, u8, u8, u8)>) -> proc_macro2::TokenStream {
    match opt {
        Some((r, g, b, a)) => quote! { Some((#r, #g, #b, #a)) },
        None => quote! { None },
    }
}

/// Emite `Option<&'static str>` para strings literais geradas pelo macro.
fn quote_opt_static_str(opt: Option<&str>) -> proc_macro2::TokenStream {
    match opt {
        Some(value) => {
            let lit = LitStr::new(value, Span::call_site());
            quote! { Some(#lit) }
        }
        None => quote! { None },
    }
}

/// Converte o enum textual do parser no enum público de `den_layout`.
fn text_transform_tokens(value: TextTransform) -> proc_macro2::TokenStream {
    match value {
        TextTransform::None => quote! { den_layout::TextTransform::None },
        TextTransform::Uppercase => quote! { den_layout::TextTransform::Uppercase },
        TextTransform::Lowercase => quote! { den_layout::TextTransform::Lowercase },
        TextTransform::Capitalize => quote! { den_layout::TextTransform::Capitalize },
    }
}

/// Converte alinhamento textual do parser no enum público de `den_layout`.
fn text_align_tokens(value: TextAlign) -> proc_macro2::TokenStream {
    match value {
        TextAlign::Left => quote! { den_layout::TextAlign::Left },
        TextAlign::Center => quote! { den_layout::TextAlign::Center },
        TextAlign::Right => quote! { den_layout::TextAlign::Right },
    }
}

/// Estima a largura própria de um elemento. Fallback compile-time; o painter
/// substitui via medição real em `paint_tree::measure_tree_text`.
pub(super) fn intrinsic_width_for(el: &DenElement) -> f32 {
    if el.bind_expr.is_some() {
        return DEFAULT_INPUT_WIDTH;
    }
    if el.segments.is_empty() {
        return 0.0;
    }
    let font_size = el.visual.font_size.unwrap_or(DEFAULT_TEXT_LINE_HEIGHT);
    let letter_spacing = el.visual.letter_spacing.unwrap_or(0.0);
    el.segments
        .iter()
        .map(|segment| match segment {
            TextSegment::Literal(text) => estimate_text_width(text, font_size, letter_spacing),
            TextSegment::Expr { .. } => DEFAULT_EXPR_TEXT_WIDTH,
        })
        .sum()
}

/// Estima a altura própria de um elemento. Fallback compile-time.
pub(super) fn intrinsic_height_for(el: &DenElement) -> f32 {
    if el.bind_expr.is_some() {
        return text_line_height_for_visual(&el.visual, DEFAULT_INPUT_LINE_HEIGHT);
    }
    if el.segments.is_empty() {
        0.0
    } else {
        text_line_height_for_visual(&el.visual, DEFAULT_TEXT_LINE_HEIGHT)
    }
}

/// Estima largura textual compile-time até o painter medir a galley real.
fn estimate_text_width(text: &str, font_size: f32, letter_spacing: f32) -> f32 {
    let char_count = text.chars().count();
    if char_count == 0 {
        return 0.0;
    }
    let glyph_width = char_count as f32 * font_size * AVERAGE_GLYPH_WIDTH_RATIO;
    let spacing_width = char_count.saturating_sub(1) as f32 * letter_spacing;
    glyph_width + spacing_width
}

/// Estima altura de linha compile-time a partir do visual resolvido.
fn text_line_height_for_visual(visual: &DenVisual, fallback_font_size: f32) -> f32 {
    let font_size = visual.font_size.unwrap_or(fallback_font_size);
    match visual.line_height {
        Some(LineHeightValue::Px(value)) => value,
        Some(LineHeightValue::Factor(value)) => font_size * value,
        None => font_size,
    }
}
