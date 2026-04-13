//! Tradução de argumentos de click handlers e geração de DenElementStyle.

use crate::types::{DenVisual, DisplayMode, WidthValue};
use super::CodegenCtx;
use quote::quote;

/// Traduz um argumento de click pra TokenStream.
/// - `idx` → `__den_idx_N` (variável de índice do loop mais interno)
/// - `style` → `__den_element_style` (struct gerado a partir do DenVisual)
/// - qualquer outra expressão → passa direto pro rustc resolver
pub(super) fn translate_click_arg(
    arg: &str,
    ctx: &CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let arg = arg.trim();
    if arg == "idx" && ctx.loop_depth > 0 {
        let idx_var = format!("__den_idx_{}", ctx.loop_depth - 1);
        return idx_var.parse().map_err(|e| format!("Internal error: {e}"));
    }
    if arg == "style" {
        return Ok(quote! { __den_element_style });
    }
    arg.parse().map_err(|e| format!("Invalid click argument '{arg}': {e}"))
}

/// Gera `let __den_element_style = DenElementStyle { ... }` a partir do DenVisual.
/// Valores são literais baked em compile time a partir do SCSS resolvido.
pub(super) fn generate_style_struct(visual: &DenVisual) -> proc_macro2::TokenStream {
    let color = quote_opt_rgb(visual.color);
    let background = quote_opt_rgb(visual.background);
    let font_size = quote_opt_f32(visual.font_size);
    let padding = quote_opt_f32(visual.padding);
    let border_radius = quote_opt_f32(visual.border_radius);
    let (border_width, border_color) = match visual.border {
        Some(b) => {
            let w = b.width;
            let (r, g, b) = b.color;
            (quote! { Some(#w) }, quote! { Some((#r, #g, #b)) })
        }
        None => (quote! { None }, quote! { None }),
    };
    let (width_px, width_percent) = match visual.width {
        WidthValue::Px(v) => (quote! { Some(#v) }, quote! { None }),
        WidthValue::Percent(v) => (quote! { None }, quote! { Some(#v) }),
        WidthValue::Auto => (quote! { None }, quote! { None }),
    };
    let is_flex = visual.display == DisplayMode::Flex;

    quote! {
        let __den_element_style = den_layout::DenElementStyle {
            color: #color,
            background: #background,
            font_size: #font_size,
            padding: #padding,
            border_radius: #border_radius,
            border_width: #border_width,
            border_color: #border_color,
            width_px: #width_px,
            width_percent: #width_percent,
            is_flex: #is_flex,
        };
    }
}

fn quote_opt_rgb(opt: Option<(u8, u8, u8)>) -> proc_macro2::TokenStream {
    match opt {
        Some((r, g, b)) => quote! { Some((#r, #g, #b)) },
        None => quote! { None },
    }
}

fn quote_opt_f32(opt: Option<f32>) -> proc_macro2::TokenStream {
    match opt {
        Some(v) => quote! { Some(#v) },
        None => quote! { None },
    }
}
