use crate::types::{DenForLoop, DenIfChain};
use super::{generate_node, CodegenCtx};
use quote::quote;

pub fn generate_for_loop(
    fl: &DenForLoop,
    ctx: &mut CodegenCtx,
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
    let mut children_code = Vec::new();
    for (i, child) in fl.children.iter().enumerate() {
        ctx.tree_path.push(i);
        children_code.push(generate_node(child, ctx)?);
        ctx.tree_path.pop();
    }
    ctx.loop_depth -= 1;

    Ok(quote! {
        for (#idx_ident, #var_ident) in (#iter_expr).iter().enumerate() {
            #( #children_code )*
        }
    })
}

pub fn generate_if_chain(
    ic: &DenIfChain,
    ctx: &mut CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let cond: proc_macro2::TokenStream = ic
        .condition
        .parse()
        .map_err(|e| format!("Invalid condition '{}': {e}", ic.condition))?;

    let mut then_code = Vec::new();
    for (i, child) in ic.then_children.iter().enumerate() {
        ctx.tree_path.push(i);
        then_code.push(generate_node(child, ctx)?);
        ctx.tree_path.pop();
    }

    if ic.else_children.is_empty() {
        Ok(quote! {
            if #cond {
                #( #then_code )*
            }
        })
    } else {
        let mut else_code = Vec::new();
        for (i, child) in ic.else_children.iter().enumerate() {
            ctx.tree_path.push(i);
            else_code.push(generate_node(child, ctx)?);
            ctx.tree_path.pop();
        }
        Ok(quote! {
            if #cond {
                #( #then_code )*
            } else {
                #( #else_code )*
            }
        })
    }
}
