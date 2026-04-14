//! Conversão de `DenVisual` para tokens de `PaintStyle` e `LayoutIntent`.
//!
//! Isola o mapeamento visual/layout do emitter principal. Toda propriedade
//! SCSS que é expressa como campo de `PaintStyle` ou `LayoutIntent` mora aqui.

use super::flex::{has_flex_grow, is_flex_container};
use crate::types::{DenElement, DenVisual, DisplayMode, TextSegment, WidthValue};
use quote::quote;

/// Altura de linha usada quando texto não define `font-size`.
pub(super) const DEFAULT_TEXT_LINE_HEIGHT: f32 = 14.0;
/// Altura de linha usada para inputs sem `font-size`.
pub(super) const DEFAULT_INPUT_LINE_HEIGHT: f32 = 16.0;
/// Largura média de glifo usada na estimativa textual (fallback antes do measure runtime).
const AVERAGE_GLYPH_WIDTH_RATIO: f32 = 0.55;
/// Largura estimada para expressões dinâmicas desconhecidas.
const DEFAULT_EXPR_TEXT_WIDTH: f32 = 48.0;
/// Largura estimada para inputs sem largura explícita.
pub(super) const DEFAULT_INPUT_WIDTH: f32 = 180.0;

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
    let cursor_pointer = visual.cursor_pointer;

    quote! {
        den_layout::PaintStyle {
            color: #color,
            background: #background,
            border_color: #border_color,
            border_width: #border_width,
            border_radius: #border_radius as f32,
            font_size: #font_size as f32,
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
    // Usa `flex::is_flex_container` pra garantir que a detecção de flex mora
    // num lugar só (reaproveita quando precisar de lógica mais fina por flex).
    let display = display_mode_tokens(if is_flex_container(el) {
        DisplayMode::Flex
    } else {
        visual.display
    });
    let padding = visual.padding.unwrap_or(0.0);
    let margin = visual.margin.unwrap_or(0.0);
    let gap = visual.gap.unwrap_or(0.0);
    let flex_grow: f32 = if has_flex_grow(el) { 1.0 } else { 0.0 };
    let intrinsic_width = intrinsic_width_for(el);
    let intrinsic_height = intrinsic_height_for(el);

    quote! {
        den_layout::LayoutIntent {
            width_rule: #width_rule,
            height_rule: #height_rule,
            display: #display,
            padding: #padding as f32,
            margin: #margin as f32,
            gap: #gap as f32,
            flex_grow: #flex_grow as f32,
            intrinsic_width: #intrinsic_width as f32,
            intrinsic_height: #intrinsic_height as f32,
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
    el.segments
        .iter()
        .map(|segment| match segment {
            TextSegment::Literal(text) => {
                text.chars().count() as f32 * font_size * AVERAGE_GLYPH_WIDTH_RATIO
            }
            TextSegment::Expr(_) => DEFAULT_EXPR_TEXT_WIDTH,
        })
        .sum()
}

/// Estima a altura própria de um elemento. Fallback compile-time.
pub(super) fn intrinsic_height_for(el: &DenElement) -> f32 {
    if el.bind_expr.is_some() {
        return el.visual.font_size.unwrap_or(DEFAULT_INPUT_LINE_HEIGHT);
    }
    if el.segments.is_empty() {
        0.0
    } else {
        el.visual.font_size.unwrap_or(DEFAULT_TEXT_LINE_HEIGHT)
    }
}
