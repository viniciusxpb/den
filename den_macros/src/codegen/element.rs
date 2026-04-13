//! Geração de código pra elementos HTML regulares e containers flex.

use super::click::{generate_style_struct, translate_click_arg};
use super::flex::build_flex_layout;
use super::frame::{build_frame_expr, build_rich_text_expr};
use super::input::generate_input_element;
use super::navigation::generate_goto_call;
use super::text::build_text_token_stream;
use super::{CodegenCtx, generate_node};
use crate::types::{DenElement, DenVisual, DisplayMode, WidthValue};
use quote::quote;
use std::hash::{Hash, Hasher};

pub fn generate_element(
    el: &DenElement,
    ctx: &mut CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    // Input element — branch separado, sem hover/click
    if el.bind_expr.is_some() {
        return generate_input_element(el, ctx);
    }
    if el.tag == "input" {
        return Err("Den: <input> requires a bind attribute. \
             Use: <input bind=\"self.field\" />"
            .to_string());
    }

    let visual = &el.visual;

    let goto_call = generate_goto_call(el)?;

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

    // Layout index deste elemento. Deve ser capturado ANTES de gerar filhos
    // pra que os filhos recebam os índices subsequentes (mesmo DFS do pré-passo).
    let my_layout_index = ctx.layout_index;
    ctx.layout_index += 1;

    // Gera filhos. Se este elemento é flex, seta parent_is_flex pros filhos
    // pra que filhos Auto sem flex-grow deixem o egui medir conteúdo.
    let prev_parent_is_flex = ctx.parent_is_flex;
    ctx.parent_is_flex = visual.display == DisplayMode::Flex;

    let mut children_code = Vec::new();
    for (i, child) in el.children.iter().enumerate() {
        ctx.tree_path.push(i);
        children_code.push(generate_node(child, ctx)?);
        ctx.tree_path.pop();
    }

    // Restaura o estado anterior (pra irmãos não herdarem).
    ctx.parent_is_flex = prev_parent_is_flex;

    let tag = el.tag.as_str();
    let has_hover = visual.needs_hover();
    let has_click = el.on_click.is_some();
    let has_goto = goto_call.is_some();
    let needs_interaction = has_hover || has_click || has_goto;

    // Constrói o call do click handler + clones de argumentos
    let (click_call, click_clone_stmts) = if let Some(func_name) = &el.on_click {
        if !el.on_click_args.is_empty() {
            // Tem args → requer den-bind
            if el.den_bind.is_none() {
                return Err(format!(
                    "Den: (click)=\"{func_name}({})\" has arguments but no den-bind attribute. \
                     Add den-bind=\"...\" to the element.",
                    el.on_click_args.join(", ")
                ));
            }

            // Gera clones dos argumentos antes do render (resolve borrow conflicts)
            let mut clones = Vec::new();
            let mut arg_idents = Vec::new();
            for (i, arg) in el.on_click_args.iter().enumerate() {
                let var_name = format!("__den_click_arg_{i}");
                let var_ident: proc_macro2::TokenStream = var_name
                    .parse()
                    .map_err(|e| format!("Internal error building click arg ident: {e}"))?;
                let arg_expr = translate_click_arg(arg, ctx)?;

                // style não precisa de clone (já é owned), outros sim
                if arg.trim() == "style" {
                    clones.push(quote! { let #var_ident = #arg_expr; });
                } else {
                    clones.push(quote! { let #var_ident = (#arg_expr).clone(); });
                }
                arg_idents.push(var_ident);
            }

            let func_ident: proc_macro2::TokenStream = format!("self.{func_name}")
                .parse()
                .map_err(|e| format!("Invalid function name '{func_name}': {e}"))?;
            let call = quote! { #func_ident(#(#arg_idents),*) };
            (Some(call), clones)
        } else {
            // Sem args — comportamento original
            let tokens: proc_macro2::TokenStream = format!("self.{func_name}()")
                .parse()
                .map_err(|e| format!("Invalid function name '{func_name}': {e}"))?;
            (Some(tokens), vec![])
        }
    } else {
        (None, vec![])
    };

    // Se args contém "style", gera o DenElementStyle struct
    let style_stmt = if el.on_click_args.iter().any(|a| a.trim() == "style") {
        generate_style_struct(&el.visual)
    } else {
        quote! {}
    };

    // Determina se este elemento tem flex-grow num container flex.
    // Só flex_grow=true cresce pra preencher o share — comportamento CSS `flex: 1`.
    // Auto sem flex_grow é content-sized (padrão CSS).
    let is_flex_auto_child = prev_parent_is_flex && visual.flex_grow;
    let force_auto_width = !prev_parent_is_flex || visual.flex_grow;

    let mut element_code = if needs_interaction {
        let element_id = den_element_id(ctx.template_path, &ctx.tree_path, tag, &el.classes);

        let render_code = if has_hover {
            let hovered = visual.resolve_hover();

            let base_inner = build_inner(
                visual,
                &text_ts,
                &children_code,
                tag,
                my_layout_index,
                force_auto_width,
            );
            let hover_inner = build_inner(
                &hovered,
                &text_ts,
                &children_code,
                tag,
                my_layout_index,
                force_auto_width,
            );

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
            let inner_code = build_inner(
                visual,
                &text_ts,
                &children_code,
                tag,
                my_layout_index,
                force_auto_width,
            );
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

        let action_call = click_call.or(goto_call);
        let click_code = if let Some(call) = action_call {
            let cursor_code = if has_goto {
                quote! {
                    if __den_resp.hovered() {
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                }
            } else {
                quote! {}
            };
            quote! {
                let __den_resp = ui.interact(
                    __den_scope.response.rect,
                    __den_id.with("click"),
                    egui::Sense::click(),
                );
                #cursor_code
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

        quote! {
            {
                #style_stmt
                #( #click_clone_stmts )*
                let __den_id = #id_expr;
                #render_code
                #click_code
            }
        }
    } else {
        // Sem hover, sem click — caminho simples
        let inner_code = build_inner(
            visual,
            &text_ts,
            &children_code,
            tag,
            my_layout_index,
            force_auto_width,
        );

        if visual.needs_frame() {
            let frame_expr = build_frame_expr(visual);
            quote! {
                #frame_expr.show(ui, |ui| {
                    #inner_code
                });
            }
        } else {
            inner_code
        }
    };

    // Se este elemento é um filho `flex: 1`, limita sua largura ao tamanho
    // calculado pelo layout runtime.
    if is_flex_auto_child {
        element_code = quote! {
            let __den_child_width = __den_layout.sizes[#my_layout_index]
                .unwrap_or_else(|| ui.available_width() / __den_scale)
                * __den_scale;
            ui.allocate_ui_with_layout(
                egui::vec2(__den_child_width, ui.available_height()),
                egui::Layout::top_down(egui::Align::Min),
                |ui| {
                    #element_code
                },
            );
        };
    }

    Ok(element_code)
}

fn build_inner(
    visual: &DenVisual,
    text_ts: &Option<proc_macro2::TokenStream>,
    children_code: &[proc_macro2::TokenStream],
    tag: &str,
    layout_index: usize,
    force_auto_width: bool,
) -> proc_macro2::TokenStream {
    let text_expr = text_ts.as_ref().map(|ts| build_rich_text_expr(ts, visual));
    let content_width_expr = layout_content_width_expr(layout_index, visual);

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
        build_flex_layout(inner, visual.gap)
    } else {
        let gap = visual.gap.unwrap_or(0.0);
        quote! {
            ui.scope(|ui| {
                ui.spacing_mut().item_spacing.y = #gap * __den_scale;
                #inner
            });
        }
    };

    // A largura vem do layout runtime. O pai decide o contexto dos filhos
    // (block/flex/grid), então o render só obedece o tamanho resolvido.
    let inner = match visual.width {
        WidthValue::Percent(_) | WidthValue::Px(_) => quote! {
            if let Some(__den_content_width) = #content_width_expr {
                ui.set_width(__den_content_width);
            }
            #inner
        },
        WidthValue::Auto => {
            if force_auto_width {
                quote! {
                    if let Some(__den_content_width) = #content_width_expr {
                        ui.set_width(__den_content_width);
                    }
                    #inner
                }
            } else {
                inner
            }
        }
    };

    // Height explícita vem do layout runtime. Auto fica com a altura natural do egui.
    match visual.height {
        WidthValue::Percent(_) | WidthValue::Px(_) => quote! {
            let __den_h = __den_layout.rects[#layout_index].height;
            if __den_h > 0.0 {
                ui.set_height(__den_h * __den_scale);
            }
            #inner
        },
        WidthValue::Auto => inner,
    }
}

fn layout_content_width_expr(layout_index: usize, visual: &DenVisual) -> proc_macro2::TokenStream {
    let horizontal_padding = visual.padding.unwrap_or(0.0) * 2.0;
    quote! {
        __den_layout.sizes[#layout_index].map(|__lw| {
            ((__lw - #horizontal_padding as f32).max(0.0)) * __den_scale
        })
    }
}

/// Gera um ID estável para um elemento dentro de uma mesma compilação.
///
/// Usa `DefaultHasher` que NÃO é determinístico entre compilações diferentes,
/// mas isso é aceitável porque o ID é usado apenas dentro de um mesmo frame
/// do egui (hover state via `data_mut`). Se o binário for recompilado, os IDs
/// mudam mas o estado transiente do egui também reseta.
fn den_element_id(template_path: &str, tree_path: &[usize], tag: &str, classes: &[String]) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    template_path.hash(&mut hasher);
    tree_path.hash(&mut hasher);
    tag.hash(&mut hasher);
    classes.hash(&mut hasher);
    hasher.finish()
}
