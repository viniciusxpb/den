mod control_flow;
mod element;
mod frame;
mod text;

use crate::types::DenNode;
use quote::quote;

/// Contexto passado pelo codegen pra rastrear posição na árvore.
pub(crate) struct CodegenCtx<'a> {
    pub has_self: bool,
    pub template_path: &'a str,
    pub tree_path: Vec<usize>,
    pub loop_depth: usize,
}

/// Gera TokenStream a partir da árvore resolvida.
pub fn generate(
    nodes: &[DenNode],
    has_self: bool,
    template_path: &str,
) -> Result<proc_macro2::TokenStream, String> {
    let mut ctx = CodegenCtx {
        has_self,
        template_path,
        tree_path: Vec::new(),
        loop_depth: 0,
    };

    let mut stmts = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        ctx.tree_path.push(i);
        stmts.push(generate_node(node, &mut ctx)?);
        ctx.tree_path.pop();
    }

    Ok(quote! { #( #stmts )* })
}

/// Dispatch por tipo de nó.
pub(crate) fn generate_node(
    node: &DenNode,
    ctx: &mut CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    match node {
        DenNode::Element(el) => element::generate_element(el, ctx),
        DenNode::ForLoop(fl) => control_flow::generate_for_loop(fl, ctx),
        DenNode::IfChain(ic) => control_flow::generate_if_chain(ic, ctx),
    }
}
