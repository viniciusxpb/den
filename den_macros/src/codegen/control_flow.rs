//! Emissão de `<for>` e `<if>/<else>` como controle de fluxo Rust em volta
//! dos pushes de `RenderNode` na tree.

use super::render_tree::{BuildCtx, emit_build_node};
use crate::types::{DenForLoop, DenIfChain};
use quote::quote;

/// `<for each="var" in="expr">children</for>` → `for (idx, var) in (expr).iter().enumerate() { children_push }`.
/// Incrementa `ctx.loop_depth` durante a recursão nos filhos — usado pro sal do `node_id`.
pub(super) fn emit_for_loop(
    fl: &DenForLoop,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let var_ident: proc_macro2::TokenStream = fl
        .each_var
        .parse()
        .map_err(|e| format!("Invalid loop variable '{}': {e}", fl.each_var))?;
    let iter_expr: proc_macro2::TokenStream = fl
        .iterable_expr
        .parse()
        .map_err(|e| format!("Invalid iterable '{}': {e}", fl.iterable_expr))?;
    let idx_ident: proc_macro2::TokenStream = format!("__den_idx_{}", ctx.loop_depth)
        .parse()
        .map_err(|e| format!("Internal error building loop index ident: {e}"))?;

    ctx.loop_depth += 1;
    let mut children_stmts = Vec::new();
    for (i, child) in fl.children.iter().enumerate() {
        ctx.tree_path.push(i);
        children_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }
    ctx.loop_depth -= 1;

    Ok(quote! {
        for (#idx_ident, #var_ident) in (#iter_expr).iter().enumerate() {
            #( #children_stmts )*
        }
    })
}

/// `<if cond="...">then</if><else>else</else>` → `if cond { then_push } else { else_push }`.
/// Branch ausente emite zero nós (sem ramo else).
pub(super) fn emit_if_chain(
    ic: &DenIfChain,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let cond: proc_macro2::TokenStream = ic
        .condition
        .parse()
        .map_err(|e| format!("Invalid condition '{}': {e}", ic.condition))?;

    let mut then_stmts = Vec::new();
    for (i, child) in ic.then_children.iter().enumerate() {
        ctx.tree_path.push(i);
        then_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }

    if ic.else_children.is_empty() {
        return Ok(quote! {
            if #cond {
                #( #then_stmts )*
            }
        });
    }

    let mut else_stmts = Vec::new();
    for (i, child) in ic.else_children.iter().enumerate() {
        ctx.tree_path.push(i);
        else_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }
    Ok(quote! {
        if #cond {
            #( #then_stmts )*
        } else {
            #( #else_stmts )*
        }
    })
}
