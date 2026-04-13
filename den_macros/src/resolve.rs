//! Fase 2 do pipeline: resolve estilos e constrói DenNode com DenVisual.

use crate::types::{
    DenElement, DenForLoop, DenIfChain, DenNode, DenVisual, RawElement, RawForLoop, RawIfChain,
    RawNode, StyleMap, StyleRule,
};

/// Resolve uma árvore raw (sem styles) em árvore com visual resolvido.
/// Cada fase recebe input imutável e devolve output owned.
pub fn resolve(raw_nodes: &[RawNode], styles: &StyleMap) -> Vec<DenNode> {
    let inherited = StyleRule::default();
    raw_nodes
        .iter()
        .map(|n| resolve_node(n, styles, &inherited))
        .collect()
}

fn resolve_node(node: &RawNode, styles: &StyleMap, inherited: &StyleRule) -> DenNode {
    match node {
        RawNode::Element(el) => resolve_element(el, styles, inherited),
        RawNode::ForLoop(fl) => resolve_for_loop(fl, styles, inherited),
        RawNode::IfChain(ic) => resolve_if_chain(ic, styles, inherited),
    }
}

fn resolve_element(el: &RawElement, styles: &StyleMap, inherited: &StyleRule) -> DenNode {
    // Começa dos styles herdados
    let mut resolved_style = inherited.inheritable();

    // Aplica as classes deste elemento (last-wins)
    for class in &el.classes {
        if let Some(rule) = styles.get(class) {
            resolved_style.merge_from(rule);
        }
    }

    // Constrói o DenVisual a partir do StyleRule resolvido
    let visual = DenVisual::from_style_rule(&resolved_style);

    // Resolve filhos com herança propagada
    let child_inherited = resolved_style.inheritable();
    let children = el
        .children
        .iter()
        .map(|c| resolve_node(c, styles, &child_inherited))
        .collect();

    // Parseia on_click em func_name + args
    let (on_click, on_click_args) = match &el.on_click {
        Some(raw) => {
            let (name, args) = crate::parse::text::parse_click_call(raw);
            (Some(name), args)
        }
        None => (None, vec![]),
    };

    DenNode::Element(DenElement {
        tag: el.tag.clone(),
        classes: el.classes.clone(),
        on_click,
        on_click_args,
        den_bind: el.den_bind.clone(),
        segments: el.segments.clone(),
        children,
        visual,
        bind_expr: el.bind_expr.clone(),
        placeholder: el.placeholder.clone(),
    })
}

fn resolve_for_loop(fl: &RawForLoop, styles: &StyleMap, inherited: &StyleRule) -> DenNode {
    // ForLoop é transparente: passa inherited direto pros filhos
    let children = fl
        .children
        .iter()
        .map(|c| resolve_node(c, styles, inherited))
        .collect();

    DenNode::ForLoop(DenForLoop {
        each_var: fl.each_var.clone(),
        iterable_expr: fl.iterable_expr.clone(),
        children,
    })
}

fn resolve_if_chain(ic: &RawIfChain, styles: &StyleMap, inherited: &StyleRule) -> DenNode {
    // IfChain é transparente: passa inherited direto pros filhos
    let then_children = ic
        .then_children
        .iter()
        .map(|c| resolve_node(c, styles, inherited))
        .collect();
    let else_children = ic
        .else_children
        .iter()
        .map(|c| resolve_node(c, styles, inherited))
        .collect();

    DenNode::IfChain(DenIfChain {
        condition: ic.condition.clone(),
        then_children,
        else_children,
    })
}
