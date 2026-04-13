//! Geração de código pra `<input bind="...">` (two-way binding).

use super::CodegenCtx;
use super::egui_backend::{
    build_frame_expr, build_text_edit_expr, flex_child_wrapper, text_edit_desired_width_expr,
};
use crate::types::{DenElement, WidthValue};
use quote::quote;

/// Quantidade de lados horizontais para padding uniforme.
const HORIZONTAL_SIDES: f32 = 2.0;

/// Gera código para `<input bind="self.field" />`.
///
/// Produz o widget de texto do backend atual com styling SCSS.
/// Não suporta (click), (change), hover, ou den-bind em v1.
pub fn generate_input_element(
    el: &DenElement,
    ctx: &mut CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    // bind requer self no template
    if !ctx.has_self {
        return Err(
            "Template uses <input bind=\"...\"> but `self` was not passed to den_template!. \
             Use: den_template!(\"path\", self);"
                .to_string(),
        );
    }

    // (click) em <input> não é suportado em v1
    if el.on_click.is_some() {
        return Err("Den: <input> does not support (click) events in v1. \
             Use the bind attribute for two-way data binding."
            .to_string());
    }

    let bind_expr = el.bind_expr.as_ref().unwrap(); // safe: caller verificou is_some()
    let visual = &el.visual;

    // Consome layout index — input é um elemento na flat list como qualquer outro
    let my_layout_index = ctx.layout_index;
    ctx.layout_index += 1;

    // Monta a expressão `&mut self.field`
    let bind_tokens: proc_macro2::TokenStream = format!("&mut {bind_expr}")
        .parse()
        .map_err(|e| format!("Invalid bind expression '{bind_expr}': {e}"))?;

    let mut textedit = build_text_edit_expr(&bind_tokens, el.placeholder.as_deref(), visual);

    // Constraint de largura — via desired_width no TextEdit.
    // Px e Percent usam o layout system (mesma fonte de verdade que elementos regulares).
    // Auto: TextEdit preenche available_width por padrão no egui.
    textedit = match visual.width {
        WidthValue::Percent(_) | WidthValue::Px(_) => {
            let idx = my_layout_index;
            let horizontal_padding = visual.padding.unwrap_or(0.0) * HORIZONTAL_SIDES;
            text_edit_desired_width_expr(textedit, idx, horizontal_padding)
        }
        WidthValue::Auto => textedit,
    };

    // Renderização final: frame wrapper ou add direto
    let element_code = if visual.needs_frame() {
        let frame = build_frame_expr(visual);
        quote! {
            #frame.show(ui, |ui| {
                ui.add(#textedit);
            });
        }
    } else {
        quote! { ui.add(#textedit); }
    };

    // Filho `flex: 1`: limita largura ao tamanho calculado pelo layout runtime.
    let is_flex_auto_child =
        ctx.parent_is_flex && visual.width == WidthValue::Auto && visual.flex_grow;
    if is_flex_auto_child {
        let idx = my_layout_index;
        let wrapped_flex_child = flex_child_wrapper(quote! { __den_child_width }, element_code);
        return Ok(quote! {
            let __den_child_width = __den_layout.sizes[#idx]
                .unwrap_or_else(|| ui.available_width() / __den_scale)
                * __den_scale;
            #wrapped_flex_child
        });
    }

    Ok(element_code)
}
