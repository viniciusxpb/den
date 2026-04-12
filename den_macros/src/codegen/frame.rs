use crate::types::DenVisual;
use quote::quote;

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
    if let Some(radius) = visual.border_radius {
        expr = quote! { #expr.corner_radius(#radius * __den_scale) };
    }
    if let Some(border) = visual.border {
        let w = border.width;
        let (r, g, b) = border.color;
        expr = quote! {
            #expr.stroke(egui::Stroke::new(
                (#w * __den_scale).max(1.0),
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
        rt = quote! { #rt.size((#size * __den_scale).max(6.0)) };
    }
    rt
}
