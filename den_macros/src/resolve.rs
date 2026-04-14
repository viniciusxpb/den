//! Fase 2 do pipeline: resolve estilos e constrói DenNode com DenVisual.

use crate::types::{
    DenElement, DenForLoop, DenIfChain, DenNode, DenVisual, RawElement, RawForLoop, RawIfChain,
    RawNode, StyleMap, StyleRule,
};

/// Output da fase 2: árvore resolvida + visual opcional do `body` (seletor de tag no SCSS).
pub struct ResolveOutput {
    pub nodes: Vec<DenNode>,
    pub body_visual: Option<DenVisual>,
}

/// Resolve uma árvore raw (sem styles) em árvore com visual resolvido.
/// Extrai separadamente o seletor `body { ... }` caso exista.
/// Propriedades herdáveis de texto do body são usadas como inheritance inicial
/// pra todos os elementos top-level.
pub fn resolve(raw_nodes: &[RawNode], styles: &StyleMap) -> ResolveOutput {
    let body_rule = styles.get("body").cloned();
    let body_visual = body_rule.as_ref().map(DenVisual::from_style_rule);

    // Inheritance inicial vem do body. Filhos herdam propriedades textuais.
    let inherited = body_rule
        .as_ref()
        .map(StyleRule::inheritable)
        .unwrap_or_default();

    let nodes = raw_nodes
        .iter()
        .map(|n| resolve_node(n, styles, &inherited))
        .collect();

    ResolveOutput { nodes, body_visual }
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
        goto_page: el.goto_page.clone(),
        goto_with: el.goto_with.clone(),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{parse_html, parse_scss};
    use crate::types::{LineHeightValue, TextAlign, TextTransform};

    const HOME_HTML: &str = include_str!("../../den_app/src/pages/home/home.html");
    const HOME_SCSS: &str = include_str!("../../den_app/src/pages/home/home.scss");

    #[test]
    fn home_font_tags_resolve_into_tree_output() {
        let raw_nodes = parse_html(HOME_HTML);
        let styles = parse_scss(HOME_SCSS);
        let output = resolve(&raw_nodes, &styles);

        assert_font_tag(
            &output.nodes,
            "font-serif",
            FontTagExpectation {
                family: r#"Georgia, "Times New Roman", serif"#,
                font_size: 15.0,
                color: Some((192, 192, 216)),
                line_height: Some(LineHeightValue::Factor(1.2)),
                text_align: Some(TextAlign::Center),
                ..Default::default()
            },
        );
        assert_font_tag(
            &output.nodes,
            "font-mono",
            FontTagExpectation {
                family: r#""Courier New", monospace"#,
                font_size: 13.0,
                color: Some((92, 219, 149)),
                letter_spacing: Some(1.0),
                line_height: Some(LineHeightValue::Factor(1.2)),
                text_align: Some(TextAlign::Center),
                ..Default::default()
            },
        );
        assert_font_tag(
            &output.nodes,
            "font-script",
            FontTagExpectation {
                family: r#""Comic Sans MS", "Brush Script MT", cursive"#,
                font_size: 15.0,
                color: Some((240, 160, 80)),
                font_italic: Some(true),
                line_height: Some(LineHeightValue::Factor(1.2)),
                text_align: Some(TextAlign::Center),
                ..Default::default()
            },
        );
        assert_font_tag(
            &output.nodes,
            "font-impact",
            FontTagExpectation {
                family: r#"Impact, "Arial Black", fantasy"#,
                font_size: 15.0,
                color: Some((233, 69, 96)),
                line_height: Some(LineHeightValue::Factor(1.2)),
                text_align: Some(TextAlign::Center),
                text_transform: Some(TextTransform::Uppercase),
                ..Default::default()
            },
        );
        assert_font_tag(
            &output.nodes,
            "font-system",
            FontTagExpectation {
                family: r#""Trebuchet MS", Verdana, sans-serif"#,
                font_size: 14.0,
                color: Some((126, 184, 247)),
                font_weight: Some(700),
                line_height: Some(LineHeightValue::Factor(1.2)),
                text_align: Some(TextAlign::Center),
                ..Default::default()
            },
        );
    }

    #[derive(Default)]
    struct FontTagExpectation {
        family: &'static str,
        font_size: f32,
        color: Option<(u8, u8, u8)>,
        font_weight: Option<u16>,
        font_italic: Option<bool>,
        line_height: Option<LineHeightValue>,
        letter_spacing: Option<f32>,
        text_transform: Option<TextTransform>,
        text_align: Option<TextAlign>,
    }

    fn assert_font_tag(nodes: &[DenNode], class_name: &str, expected: FontTagExpectation) {
        let element = find_element_by_class(nodes, class_name)
            .unwrap_or_else(|| panic!("class '{class_name}' should exist in resolved tree"));
        let visual = &element.visual;

        assert_eq!(
            visual.font_family.as_deref(),
            Some(expected.family),
            "{class_name} font-family"
        );
        assert_eq!(
            visual.font_size,
            Some(expected.font_size),
            "{class_name} font-size"
        );
        assert_eq!(visual.color, expected.color, "{class_name} color");
        assert_eq!(
            visual.font_weight, expected.font_weight,
            "{class_name} font-weight"
        );
        assert_eq!(
            visual.font_italic, expected.font_italic,
            "{class_name} font-style"
        );
        assert_eq!(
            visual.line_height, expected.line_height,
            "{class_name} line-height"
        );
        assert_eq!(
            visual.letter_spacing, expected.letter_spacing,
            "{class_name} letter-spacing"
        );
        assert_eq!(
            visual.text_transform, expected.text_transform,
            "{class_name} text-transform"
        );
        assert_eq!(
            visual.text_align, expected.text_align,
            "{class_name} text-align"
        );
    }

    fn find_element_by_class<'a>(nodes: &'a [DenNode], class_name: &str) -> Option<&'a DenElement> {
        nodes
            .iter()
            .find_map(|node| find_node_by_class(node, class_name))
    }

    fn find_node_by_class<'a>(node: &'a DenNode, class_name: &str) -> Option<&'a DenElement> {
        match node {
            DenNode::Element(element) => {
                if element.classes.iter().any(|class| class == class_name) {
                    return Some(element);
                }
                find_element_by_class(&element.children, class_name)
            }
            DenNode::ForLoop(for_loop) => find_element_by_class(&for_loop.children, class_name),
            DenNode::IfChain(if_chain) => {
                find_element_by_class(&if_chain.then_children, class_name)
                    .or_else(|| find_element_by_class(&if_chain.else_children, class_name))
            }
        }
    }
}
