//! Emissão de `@for`/`@empty` e `@if`/`!`/`!` como controle de fluxo Rust em volta
//! dos pushes de `RenderNode` na tree.

use super::render_tree::{BuildCtx, TreeSegment, emit_build_node};
use crate::types::{DenForLoop, DenIfChain};
use quote::quote;

/// `@for(var in expr) { children } @empty { empty_children }`
/// →
/// ```ignore
/// {
///     let __iter = &(expr);
///     if __iter.iter().len() == 0 {
///         /* empty_children */
///     } else {
///         for (idx, var) in __iter.iter().enumerate() {
///             /* children */
///         }
///     }
/// }
/// ```
/// Sem `@empty` vira apenas o `for` cru.
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
        ctx.tree_path.push(TreeSegment::Child(i));
        children_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }
    ctx.loop_depth -= 1;

    if fl.empty_children.is_empty() {
        return Ok(quote! {
            for (#idx_ident, #var_ident) in (#iter_expr).iter().enumerate() {
                #( #children_stmts )*
            }
        });
    }

    let mut empty_stmts = Vec::new();
    ctx.tree_path.push(TreeSegment::EmptyBranch);
    for (i, child) in fl.empty_children.iter().enumerate() {
        ctx.tree_path.push(TreeSegment::Child(i));
        empty_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }
    ctx.tree_path.pop();

    Ok(quote! {
        {
            let __den_iter = &(#iter_expr);
            if __den_iter.is_empty() {
                #( #empty_stmts )*
            } else {
                for (#idx_ident, #var_ident) in __den_iter.iter().enumerate() {
                    #( #children_stmts )*
                }
            }
        }
    })
}

/// `@if(c1) { A } !c2 { B } !c3 { C } ! { D }` → `if c1 { A } else if c2 { B } else if c3 { C } else { D }`.
/// Cadeia com 1+ branches e `else` opcional.
pub(super) fn emit_if_chain(
    ic: &DenIfChain,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    if ic.branches.is_empty() {
        return Err(
            "Den (codegen bug): IfChain chegou ao codegen sem branches — \
             parser deveria ter rejeitado antes"
                .to_string(),
        );
    }

    // Emite cada branch como `if { ... }` ou `else if { ... }`.
    let mut branch_tokens: Vec<proc_macro2::TokenStream> = Vec::new();
    for (branch_idx, branch) in ic.branches.iter().enumerate() {
        let cond: proc_macro2::TokenStream = branch
            .condition
            .parse()
            .map_err(|e| format!("Invalid condition '{}': {e}", branch.condition))?;
        let mut stmts = Vec::new();
        ctx.tree_path.push(TreeSegment::IfBranch(branch_idx));
        for (i, child) in branch.children.iter().enumerate() {
            ctx.tree_path.push(TreeSegment::Child(i));
            stmts.push(emit_build_node(child, ctx)?);
            ctx.tree_path.pop();
        }
        ctx.tree_path.pop();
        let keyword = if branch_idx == 0 {
            quote! { if }
        } else {
            quote! { else if }
        };
        branch_tokens.push(quote! {
            #keyword #cond {
                #( #stmts )*
            }
        });
    }

    if ic.else_children.is_empty() {
        return Ok(quote! {
            #( #branch_tokens )*
        });
    }

    let mut else_stmts = Vec::new();
    ctx.tree_path.push(TreeSegment::ElseBranch);
    for (i, child) in ic.else_children.iter().enumerate() {
        ctx.tree_path.push(TreeSegment::Child(i));
        else_stmts.push(emit_build_node(child, ctx)?);
        ctx.tree_path.pop();
    }
    ctx.tree_path.pop();

    Ok(quote! {
        #( #branch_tokens )*
        else {
            #( #else_stmts )*
        }
    })
}
