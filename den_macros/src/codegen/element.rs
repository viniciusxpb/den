//! Emissão do push de um `RenderNode` na `__den_tree` pra cada `<div>`, `<h1>`,
//! `<input>`, etc. Chama os módulos especializados (`click`, `navigation`,
//! `input`, `style`, `text`) pra cada responsabilidade.

use super::click::build_click_slot;
use super::input::{emit_kind_tokens as emit_input_kind, emit_sync_pre as emit_input_sync};
use super::navigation::build_goto_slot;
use super::render_tree::{BuildCtx, emit_build_node};
use super::style::{hover_style_tokens, layout_intent_tokens, paint_style_tokens};
use super::text::build_text_token_stream;
use crate::types::DenElement;
use quote::quote;
use std::hash::{Hash, Hasher};

/// Emite o bloco de código que empurra este elemento como `RenderNode` e
/// depois processa recursivamente seus filhos com `__den_parent = __den_idx`.
pub(super) fn emit_element(
    el: &DenElement,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    validate_element_shape(el, ctx)?;

    let style_tokens = paint_style_tokens(&el.visual);
    let hover_tokens = hover_style_tokens(&el.visual);
    let layout_tokens = layout_intent_tokens(el);

    let interact_tokens = emit_interact(el, ctx)?;

    let base_hash = element_id_hash(ctx.template_path, &ctx.tree_path, &el.tag, &el.classes);
    let node_id_expr = node_id_with_loop_salt(base_hash, ctx.loop_depth)?;

    let kind_tokens = emit_kind(el, ctx)?;
    let input_pre = emit_input_sync(el, &node_id_expr, ctx)?;

    let mut children_stmts = Vec::new();
    for (i, child) in el.children.iter().enumerate() {
        ctx.tree_path.push(i);
        children_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }

    Ok(quote! {
        {
            #input_pre
            let __den_li = __den_tree.nodes.len() + 1;
            let __den_node_id = den_layout::DenNodeId::new(#node_id_expr);
            let __den_node = den_layout::RenderNode {
                node_id: __den_node_id,
                layout_index: __den_li,
                kind: #kind_tokens,
                style: #style_tokens,
                hover_style: #hover_tokens,
                interact: #interact_tokens,
                layout: #layout_tokens,
                children: Vec::new(),
            };
            let __den_idx = __den_tree.push(__den_node);
            if __den_parent == usize::MAX {
                __den_tree.roots.push(__den_idx);
            } else {
                __den_tree.nodes[__den_parent].children.push(__den_idx);
            }
            {
                let __den_parent: usize = __den_idx;
                #( #children_stmts )*
            }
        }
    })
}

/// Valida tag/atributos do elemento antes de emitir.
fn validate_element_shape(el: &DenElement, ctx: &BuildCtx) -> Result<(), String> {
    if el.tag == "input" && el.bind_expr.is_none() {
        return Err(
            "Den: <input> requires a bind attribute. Use: <input bind=\"self.field\" />"
                .to_string(),
        );
    }
    if el.on_click.is_some() && !ctx.has_self {
        return Err(
            "Template uses (click) event but `self` was not passed to den_template!. \
             Use: den_template!(\"path\", self);"
                .to_string(),
        );
    }
    Ok(())
}

/// Emite o `RenderKind` — Container, Text, ou Input.
fn emit_kind(el: &DenElement, ctx: &BuildCtx) -> Result<proc_macro2::TokenStream, String> {
    if el.bind_expr.is_some() {
        return Ok(emit_input_kind(el));
    }

    let text_ts = build_text_token_stream(&el.segments, ctx.has_self)?;
    if let Some(text_expr) = text_ts {
        let heading = matches!(el.tag.as_str(), "heading" | "h1" | "h2" | "h3");
        return Ok(quote! {
            den_layout::RenderKind::Text {
                content: #text_expr.to_string(),
                heading: #heading,
            }
        });
    }

    Ok(quote! { den_layout::RenderKind::Container })
}

/// Emite o `Interact { click_handler, goto_slot, pointer_on_hover }`.
fn emit_interact(
    el: &DenElement,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let click = build_click_slot(el, ctx)?;
    let goto = build_goto_slot(el, ctx)?;
    let click_tokens = match click {
        Some(slot) => quote! { Some(#slot) },
        None => quote! { None },
    };
    let goto_tokens = match goto {
        Some(slot) => quote! { Some(#slot) },
        None => quote! { None },
    };
    let pointer_on_hover = el.goto_page.is_some();
    Ok(quote! {
        den_layout::Interact {
            click_handler: #click_tokens,
            goto_slot: #goto_tokens,
            pointer_on_hover: #pointer_on_hover,
        }
    })
}

/// Hash estável dentro de uma compilação (template + caminho + tag + classes).
fn element_id_hash(
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

/// Aplica o sal dos índices de loop encapsulantes ao hash estático pra que o
/// `node_id` seja único por iteração (hover/focus estáveis por item).
fn node_id_with_loop_salt(
    base: u64,
    loop_depth: usize,
) -> Result<proc_macro2::TokenStream, String> {
    if loop_depth == 0 {
        return Ok(quote! { #base });
    }
    let mut salt = quote! { 0u64 };
    for d in 0..loop_depth {
        let idx: proc_macro2::TokenStream = format!("__den_idx_{d}")
            .parse()
            .map_err(|e| format!("Internal error building loop salt ident: {e}"))?;
        salt = quote! { (#salt).wrapping_mul(31).wrapping_add(#idx as u64) };
    }
    Ok(quote! { #base ^ (#salt) })
}
