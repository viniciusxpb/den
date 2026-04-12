use crate::types::DenVisual;
use quote::quote;

/// Gera expressão `egui::Frame::default().fill(...).inner_margin(...)...`
pub fn build_frame_expr(visual: &DenVisual) -> proc_macro2::TokenStream {
    let mut expr = quote! { egui::Frame::default() };

    if let Some((r, g, b)) = visual.background {
        expr = quote! { #expr.fill(egui::Color32::from_rgb(#r, #g, #b)) };
    }
    if let Some(pad) = visual.padding {
        expr = quote! { #expr.inner_margin(#pad) };
    }
    if let Some(radius) = visual.border_radius {
        expr = quote! { #expr.corner_radius(#radius) };
    }
    if let Some(border) = visual.border {
        let w = border.width;
        let (r, g, b) = border.color;
        expr = quote! {
            #expr.stroke(egui::Stroke::new(#w, egui::Color32::from_rgb(#r, #g, #b)))
        };
    }
    expr
}

/// Gera expressão `egui::RichText::new(text).color(...).size(...)`
pub fn build_rich_text_expr(
    text_ts: &proc_macro2::TokenStream,
    visual: &DenVisual,
) -> proc_macro2::TokenStream {
    let mut rt = quote! { egui::RichText::new(#text_ts) };

    if let Some((r, g, b)) = visual.color {
        rt = quote! { #rt.color(egui::Color32::from_rgb(#r, #g, #b)) };
    }
    if let Some(size) = visual.font_size {
        rt = quote! { #rt.size(#size) };
    }
    rt
}
