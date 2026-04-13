//! Geração de código pra `<input bind="...">` (two-way binding).

use crate::types::{DenElement, WidthValue};
use super::CodegenCtx;
use super::frame::build_frame_expr;
use quote::quote;

/// Gera código para `<input bind="self.field" />`.
///
/// Produz `egui::TextEdit::singleline(&mut self.field)` com styling SCSS.
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
        return Err(
            "Den: <input> does not support (click) events in v1. \
             Use the bind attribute for two-way data binding."
                .to_string(),
        );
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

    // Constrói a chain do TextEdit
    let mut textedit = quote! { egui::TextEdit::singleline(#bind_tokens) };

    // Desabilita frame nativo quando Den fornece frame via SCSS
    if visual.needs_frame() {
        textedit = quote! { #textedit.frame(false) };
    }

    if let Some(hint) = &el.placeholder {
        textedit = quote! { #textedit.hint_text(#hint) };
    }

    if let Some(size) = visual.font_size {
        textedit = quote! {
            #textedit.font(egui::FontId::proportional((#size * __den_scale).max(6.0)))
        };
    }

    if let Some((r, g, b)) = visual.color {
        textedit = quote! {
            #textedit.text_color(egui::Color32::from_rgb(#r, #g, #b))
        };
    }

    // Constraint de largura — via desired_width no TextEdit.
    // Px usa o layout system (mesma fonte de verdade que elementos regulares).
    // Percent usa available_width inline (não passa pelo layout system).
    // Auto: TextEdit preenche available_width por padrão no egui.
    textedit = match visual.width {
        WidthValue::Percent(pct) => {
            quote! { #textedit.desired_width(ui.available_width() * #pct) }
        }
        WidthValue::Px(_) => {
            let idx = my_layout_index;
            quote! {
                #textedit.desired_width(
                    __den_layout.sizes[#idx].unwrap_or(0.0) * __den_scale
                )
            }
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

    // Filho Auto de flex: limita largura ao share calculado pelo pai
    let is_flex_auto_child = ctx.parent_is_flex && visual.width == WidthValue::Auto;
    if is_flex_auto_child {
        return Ok(quote! {
            ui.allocate_ui_with_layout(
                egui::vec2(__den_flex_share, ui.available_height()),
                egui::Layout::top_down(egui::Align::Min),
                |ui| { #element_code },
            );
        });
    }

    Ok(element_code)
}
