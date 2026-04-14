//! Fase 3 do pipeline: geração de código que constrói a `RenderTree`.
//!
//! O `den_template!` gera código que:
//! 1. BUILD — empurra `RenderNode`s numa `__den_tree` a cada frame.
//! 2. RESOLVE — alimenta a `LayoutTable` com `to_layout_entries()` e chama `resolve_in_viewport`.
//! 3. PAINT — delega pro `crate::den_paint::paint_tree` (egui-specific vive lá).
//! 4. DISPATCH — roteia `PaintEvent`s pros handlers registrados no build.

mod click;
mod control_flow;
mod element;
mod flex;
mod input;
mod navigation;
mod render_tree;
mod style;
mod text;

use crate::types::DenNode;
use quote::quote;
use render_tree::{BuildCtx, emit_build_node};

/// Gera TokenStream a partir da árvore resolvida.
pub fn generate(
    nodes: &[DenNode],
    has_self: bool,
    template_path: &str,
) -> Result<proc_macro2::TokenStream, String> {
    let mut ctx = BuildCtx::new(has_self, template_path);

    let mut build_stmts = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        ctx.tree_path.push(i);
        build_stmts.push(emit_build_node(node, &mut ctx)?);
        ctx.tree_path.pop();
    }

    // Tabelas de dispatch
    let click_arms = ctx
        .handlers
        .iter()
        .enumerate()
        .map(|(idx, call)| {
            let slot = idx as u32;
            quote! { #slot => { #call; } }
        })
        .collect::<Vec<_>>();

    let goto_arms = ctx
        .goto_slots
        .iter()
        .enumerate()
        .map(|(idx, nav)| {
            let slot = idx as u32;
            quote! { #slot => { #nav } }
        })
        .collect::<Vec<_>>();

    let input_mirrors = ctx.input_mirrors.clone();

    Ok(quote! {
        {
            // Constrói a RenderTree a cada frame.
            let mut __den_tree = den_layout::RenderTree::new();
            {
                let __den_parent: usize = usize::MAX;
                let _ = __den_parent; // evita warning em templates sem filhos
                #( #build_stmts )*
            }

            // LayoutTable em thread_local — reutiliza allocation entre frames.
            ::std::thread_local! {
                static __DEN_LAYOUT_STORE: ::std::cell::RefCell<den_layout::LayoutTable> =
                    ::std::cell::RefCell::new(den_layout::LayoutTable::new(::std::vec::Vec::new()));
                static __DEN_LAYOUT_DEBUG_DUMPED: ::std::cell::Cell<bool> =
                    ::std::cell::Cell::new(false);
            }

            // PAINT + RESOLVE + MEASURE tudo no backend egui.
            let __den_events = __DEN_LAYOUT_STORE.with(|__tl| {
                let mut __den_layout = __tl.borrow_mut();
                let __events = crate::den_paint::paint_tree(
                    ui,
                    __den_scale,
                    &mut __den_tree,
                    &mut __den_layout,
                    __den_route_state,
                );

                if den_layout::layout_debug_enabled() {
                    __DEN_LAYOUT_DEBUG_DUMPED.with(|__dumped| {
                        if !__dumped.get() {
                            let __len = __den_layout.entries.len();
                            let __labels: Vec<String> = (0..__len)
                                .map(|i| {
                                    if i == 0 {
                                        "body".to_string()
                                    } else {
                                        format!("node#{}", i)
                                    }
                                })
                                .collect();
                            let __label_refs: Vec<&str> =
                                __labels.iter().map(String::as_str).collect();
                            __den_layout.debug_dump(#template_path, &__label_refs);
                            __dumped.set(true);
                        }
                    });
                }

                __events
            });

            // DISPATCH (fora do borrow da LayoutTable, handlers podem tocar self).
            for __ev in __den_events {
                match __ev {
                    crate::den_paint::PaintEvent::Click { handler } => match handler {
                        #( #click_arms )*
                        _ => {}
                    },
                    crate::den_paint::PaintEvent::Goto { slot } => match slot {
                        #( #goto_arms )*
                        _ => {}
                    },
                    crate::den_paint::PaintEvent::InputChanged { node_id: __ev_node_id, value: __ev_value } => {
                        __den_route_state
                            .inputs_mut()
                            .set(__ev_node_id, __ev_value.clone());
                        #( #input_mirrors )*
                    }
                }
            }
        }
    })
}
