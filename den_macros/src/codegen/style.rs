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
    DenElement, DenVisual, DisplayMode, LineHeightValue, TextAlign, TextSegment, TextTransform,
    WidthValue,
};
use proc_macro2::Span;
use quote::quote;
use syn::LitStr;

/// Emite um literal `den_layout::PaintStyle { .. }` para o visual dado.
pub(crate) fn paint_style_tokens(visual: &DenVisual) -> proc_macro2::TokenStream {
    let color = quote_opt_rgb(visual.color);
    let background = quote_opt_rgb(visual.background);
    let (border_color, border_width) = match visual.border {
        Some(b) => {
            let (r, g, b_) = b.color;
            let w = b.width;
            (quote! { Some((#r, #g, #b_)) }, quote! { #w as f32 })
        }
        None => (quote! { None }, quote! { 0.0f32 }),
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
    let cursor_pointer = visual.cursor_pointer;

    quote! {
        den_layout::PaintStyle {
            color: #color,
            background: #background,
            border_color: #border_color,
            border_width: #border_width,
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
        }
    }
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
    let width_rule = dimension_rule_tokens(visual.width);
    let height_rule = dimension_rule_tokens(visual.height);
    let min_width = optional_dimension_tokens(visual.min_width);
    let max_width = optional_dimension_tokens(visual.max_width);
    let min_height = optional_dimension_tokens(visual.min_height);
    let max_height = optional_dimension_tokens(visual.max_height);
    let display = display_mode_tokens(if is_flex_container(el) {
        DisplayMode::Flex
    } else {
        visual.display
    });
    let padding = visual.padding.unwrap_or(0.0);
    let margin = visual.margin.unwrap_or(0.0);
    let gap = visual.gap.unwrap_or(0.0);
    let border_width = visual.border.map(|b| b.width).unwrap_or(0.0);
    let flex_grow: f32 = if has_flex_grow(el) { 1.0 } else { 0.0 };
    let intrinsic_width = intrinsic_width_for(el);
    let intrinsic_height = intrinsic_height_for(el);

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
            border_width: #border_width as f32,
            margin: #margin as f32,
            gap: #gap as f32,
            flex_grow: #flex_grow as f32,
            intrinsic_width: #intrinsic_width as f32,
            intrinsic_height: #intrinsic_height as f32,
        }
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

pub(super) fn quote_opt_rgb(opt: Option<(u8, u8, u8)>) -> proc_macro2::TokenStream {
    match opt {
        Some((r, g, b)) => quote! { Some((#r, #g, #b)) },
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
