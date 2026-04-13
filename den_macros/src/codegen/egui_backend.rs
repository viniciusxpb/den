//! Fronteira de geração de código específica do backend egui.

use crate::types::DenVisual;
use quote::quote;

/// Menor largura de borda visível no egui após escala.
const MIN_BORDER_WIDTH: f32 = 1.0;

/// Menor tamanho de fonte aceito pelo renderer egui.
const MIN_FONT_SIZE: f32 = 6.0;

/// Gera expressão `egui::Frame::default().fill(...).inner_margin(...)...`
/// Valores em px são multiplicados por `__den_scale` (variável no escopo do caller).
/// Cores não escalam.
pub fn build_frame_expr(visual: &DenVisual) -> proc_macro2::TokenStream {
    let mut expr = quote! { egui::Frame::default() };

    if let Some((r, g, b)) = visual.background {
        expr = quote! { #expr.fill(egui::Color32::from_rgb(#r, #g, #b)) };
    }
    if let Some(pad) = visual.padding {
        expr = quote! { #expr.inner_margin(#pad * __den_scale) };
    }
    if let Some(margin) = visual.margin {
        expr = quote! { #expr.outer_margin(#margin * __den_scale) };
    }
    if let Some(radius) = visual.border_radius {
        expr = quote! { #expr.corner_radius(#radius * __den_scale) };
    }
    if let Some(border) = visual.border {
        let w = border.width;
        let (r, g, b) = border.color;
        expr = quote! {
            #expr.stroke(egui::Stroke::new(
                (#w * __den_scale).max(#MIN_BORDER_WIDTH),
                egui::Color32::from_rgb(#r, #g, #b),
            ))
        };
    }
    expr
}

/// Gera expressão `egui::RichText::new(text).color(...).size(...)`
/// Font-size escala. Cor não escala.
pub fn build_rich_text_expr(
    text_ts: &proc_macro2::TokenStream,
    visual: &DenVisual,
) -> proc_macro2::TokenStream {
    let mut rt = quote! { egui::RichText::new(#text_ts) };

    if let Some((r, g, b)) = visual.color {
        rt = quote! { #rt.color(egui::Color32::from_rgb(#r, #g, #b)) };
    }
    if let Some(size) = visual.font_size {
        rt = quote! { #rt.size((#size * __den_scale).max(#MIN_FONT_SIZE)) };
    }
    rt
}

/// Gera a expressão base de `TextEdit` para `<input bind="...">`.
pub fn build_text_edit_expr(
    bind_tokens: &proc_macro2::TokenStream,
    placeholder: Option<&str>,
    visual: &DenVisual,
) -> proc_macro2::TokenStream {
    let mut textedit = quote! { egui::TextEdit::singleline(#bind_tokens) };

    if visual.needs_frame() {
        textedit = quote! { #textedit.frame(false) };
    }
    if let Some(hint) = placeholder {
        textedit = quote! { #textedit.hint_text(#hint) };
    }
    if let Some(size) = visual.font_size {
        textedit = quote! {
            #textedit.font(egui::FontId::proportional((#size * __den_scale).max(#MIN_FONT_SIZE)))
        };
    }
    if let Some((r, g, b)) = visual.color {
        textedit = quote! {
            #textedit.text_color(egui::Color32::from_rgb(#r, #g, #b))
        };
    }
    textedit
}

/// Aplica a largura desejada calculada pelo layout em um `TextEdit`.
pub fn text_edit_desired_width_expr(
    textedit: proc_macro2::TokenStream,
    layout_index: usize,
    horizontal_padding: f32,
) -> proc_macro2::TokenStream {
    quote! {
        #textedit.desired_width(
            ((__den_layout.sizes[#layout_index].unwrap_or(0.0) - #horizontal_padding as f32)
                .max(0.0)) * __den_scale
        )
    }
}

/// Gera expressão para cursor de pointer.
pub fn pointing_hand_cursor_expr() -> proc_macro2::TokenStream {
    quote! { egui::CursorIcon::PointingHand }
}

/// Gera expressão para a sense de click.
pub fn click_sense_expr() -> proc_macro2::TokenStream {
    quote! { egui::Sense::click() }
}

/// Gera expressão `egui::Id::new(...)`.
pub fn element_id_expr(seed: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! { egui::Id::new(#seed) }
}

/// Envolve um elemento flex-grow em um viewport vertical limitado por largura.
pub fn flex_child_wrapper(
    child_width: proc_macro2::TokenStream,
    element_code: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    quote! {
        ui.allocate_ui_with_layout(
            egui::vec2(#child_width, ui.available_height()),
            egui::Layout::top_down(egui::Align::Min),
            |ui| {
                #element_code
            },
        );
    }
}
