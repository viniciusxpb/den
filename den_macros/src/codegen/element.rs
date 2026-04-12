use crate::types::{DenElement, DenVisual, DisplayMode, WidthValue};
use super::{generate_node, CodegenCtx};
use super::frame::{build_frame_expr, build_rich_text_expr};
use super::text::build_text_token_stream;
use quote::quote;
use std::hash::{Hash, Hasher};

pub fn generate_element(
    el: &DenElement,
    ctx: &mut CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let visual = &el.visual;

    // Valida uso de (click) sem self
    if el.on_click.is_some() && !ctx.has_self {
        return Err(
            "Template uses (click) event but `self` was not passed to den_template!. \
             Use: den_template!(\"path\", self);"
                .to_string(),
        );
    }

    // Constrói conteúdo de texto
    let text_ts = build_text_token_stream(&el.segments, ctx.has_self)?;

    // Gera código dos filhos (visual já resolvido, sem propagação aqui)
    let mut children_code = Vec::new();
    for (i, child) in el.children.iter().enumerate() {
        ctx.tree_path.push(i);
        children_code.push(generate_node(child, ctx)?);
        ctx.tree_path.pop();
    }

    let tag = el.tag.as_str();
    let has_hover = visual.needs_hover();
    let has_click = el.on_click.is_some();
    let needs_interaction = has_hover || has_click;

    // Constrói o call do click handler
    let click_call = if let Some(func_name) = &el.on_click {
        let tokens: proc_macro2::TokenStream = format!("self.{func_name}()")
            .parse()
            .map_err(|e| format!("Invalid function name '{func_name}': {e}"))?;
        Some(tokens)
    } else {
        None
    };

    if needs_interaction {
        let element_id = den_element_id(ctx.template_path, &ctx.tree_path, tag, &el.classes);

        let render_code = if has_hover {
            let hovered = visual.resolve_hover();

            let base_inner = build_inner(visual, &text_ts, &children_code, tag);
            let hover_inner = build_inner(&hovered, &text_ts, &children_code, tag);

            let base_code = if visual.needs_frame() {
                let frame = build_frame_expr(visual);
                quote! { #frame.show(ui, |ui| { #base_inner }); }
            } else {
                base_inner
            };

            let hover_code = if hovered.needs_frame() {
                let frame = build_frame_expr(&hovered);
                quote! { #frame.show(ui, |ui| { #hover_inner }); }
            } else {
                hover_inner
            };

            let cursor_code = if hovered.cursor_pointer {
                quote! {
                    if __den_is_hovered {
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                }
            } else {
                quote! {}
            };

            quote! {
                let __den_was_hovered = ui.data(|d| d.get_temp::<bool>(__den_id).unwrap_or(false));
                let __den_scope = ui.scope(|ui| {
                    if __den_was_hovered {
                        #hover_code
                    } else {
                        #base_code
                    }
                });
                let __den_is_hovered = ui.rect_contains_pointer(__den_scope.response.rect);
                ui.data_mut(|d| d.insert_temp(__den_id, __den_is_hovered));
                #cursor_code
            }
        } else {
            // Click apenas, sem hover — wrap em scope pra capturar rect
            let inner_code = build_inner(visual, &text_ts, &children_code, tag);
            let wrapped = if visual.needs_frame() {
                let frame_expr = build_frame_expr(visual);
                quote! { #frame_expr.show(ui, |ui| { #inner_code }); }
            } else {
                inner_code
            };
            quote! {
                let __den_scope = ui.scope(|ui| { #wrapped });
            }
        };

        let click_code = if let Some(call) = click_call {
            quote! {
                let __den_resp = ui.interact(
                    __den_scope.response.rect,
                    __den_id.with("click"),
                    egui::Sense::click(),
                );
                if __den_resp.clicked() {
                    #call;
                }
            }
        } else {
            quote! {}
        };

        let id_expr = if ctx.loop_depth > 0 {
            let mut salt = quote! { 0u64 };
            for d in 0..ctx.loop_depth {
                let idx: proc_macro2::TokenStream = format!("__den_idx_{d}")
                    .parse()
                    .map_err(|e| format!("Internal error building loop salt ident: {e}"))?;
                salt = quote! { (#salt).wrapping_mul(31).wrapping_add(#idx as u64) };
            }
            quote! { egui::Id::new(#element_id ^ #salt) }
        } else {
            quote! { egui::Id::new(#element_id) }
        };

        Ok(quote! {
            {
                let __den_id = #id_expr;
                #render_code
                #click_code
            }
        })
    } else {
        // Sem hover, sem click — caminho simples
        let inner_code = build_inner(visual, &text_ts, &children_code, tag);

        Ok(if visual.needs_frame() {
            let frame_expr = build_frame_expr(visual);
            quote! {
                #frame_expr.show(ui, |ui| {
                    #inner_code
                });
            }
        } else {
            inner_code
        })
    }
}

fn build_inner(
    visual: &DenVisual,
    text_ts: &Option<proc_macro2::TokenStream>,
    children_code: &[proc_macro2::TokenStream],
    tag: &str,
) -> proc_macro2::TokenStream {
    let text_expr = text_ts.as_ref().map(|ts| build_rich_text_expr(ts, visual));

    let inner = match tag {
        "heading" | "h1" | "h2" | "h3" => {
            if let Some(rt) = text_expr {
                quote! { ui.heading(#rt); }
            } else if !children_code.is_empty() {
                quote! { #( #children_code )* }
            } else {
                quote! {}
            }
        }
        _ => {
            let mut stmts = Vec::new();
            if let Some(rt) = text_expr {
                stmts.push(quote! { ui.label(#rt); });
            }
            for child in children_code {
                stmts.push(child.clone());
            }
            quote! { #( #stmts )* }
        }
    };

    let inner = if visual.display == DisplayMode::Flex {
        quote! { ui.horizontal(|ui| { #inner }); }
    } else {
        inner
    };

    match visual.width {
        WidthValue::Percent(pct) => quote! {
            ui.set_width(ui.available_width() * #pct);
            #inner
        },
        WidthValue::Px(px) => quote! {
            ui.set_width(#px * __den_scale);
            #inner
        },
        WidthValue::Auto => inner,
    }
}

fn den_element_id(
    template_path: &str,
    tree_path: &[usize],
    tag: &str,
    classes: &[String],
) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    template_path.hash(&mut hasher);
    tree_path.hash(&mut hasher);
    tag.hash(&mut hasher);
    classes.hash(&mut hasher);
    hasher.finish()
}
