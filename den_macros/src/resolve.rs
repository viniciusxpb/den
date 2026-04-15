//! Fase 2 do pipeline: resolve estilos e constrói DenNode com DenVisual.

use crate::types::{
    DenElement, DenForLoop, DenIfBranch, DenIfChain, DenNode, DenVisual, RawElement, RawForLoop,
    RawIfChain, RawNode, RawObject, StyleMap, StyleRule,
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

    let inherited = body_rule
        .as_ref()
        .map(StyleRule::inheritable)
        .unwrap_or_default();

    let mut scope: Vec<String> = Vec::new();
    let nodes = raw_nodes
        .iter()
        .map(|n| resolve_node(n, styles, &inherited, &mut scope))
        .collect();

    ResolveOutput { nodes, body_visual }
}

fn resolve_node(
    node: &RawNode,
    styles: &StyleMap,
    inherited: &StyleRule,
    scope: &mut Vec<String>,
) -> DenNode {
    match node {
        RawNode::Element(el) => resolve_element(el, styles, inherited, scope),
        RawNode::ForLoop(fl) => resolve_for_loop(fl, styles, inherited, scope),
        RawNode::IfChain(ic) => resolve_if_chain(ic, styles, inherited, scope),
        RawNode::Object(obj) => resolve_object(obj, styles, inherited, scope),
    }
}

fn resolve_element(
    el: &RawElement,
    styles: &StyleMap,
    inherited: &StyleRule,
    scope: &mut Vec<String>,
) -> DenNode {
    let mut resolved_style = inherited.inheritable();
    for class in &el.classes {
        if let Some(rule) = styles.get(class) {
            resolved_style.merge_from(rule);
        }
    }
    let visual = DenVisual::from_style_rule(&resolved_style);
    let child_inherited = resolved_style.inheritable();

    let children = el
        .children
        .iter()
        .map(|c| resolve_node(c, styles, &child_inherited, scope))
        .collect();

    let (on_click, on_click_args) = match &el.on_click {
        Some(raw) => {
            let (name, args) = crate::parse::text::parse_click_call(raw);
            (Some(name), args)
        }
        None => (None, vec![]),
    };

    // Resolve `@bind` contra o escopo ativo (`@object`).
    let bind_expr = el.bind_expr.as_ref().map(|raw| apply_scope(raw, scope));

    DenNode::Element(DenElement {
        tag: el.tag.clone(),
        classes: el.classes.clone(),
        on_click,
        on_click_args,
        den_bind: el.den_bind.clone(),
        segments: el.segments.clone(),
        children,
        visual,
        bind_expr,
        placeholder: el.placeholder.clone(),
        goto_page: el.goto_page.clone(),
        goto_with: el.goto_with.clone(),
    })
}

fn resolve_for_loop(
    fl: &RawForLoop,
    styles: &StyleMap,
    inherited: &StyleRule,
    scope: &mut Vec<String>,
) -> DenNode {
    let children = fl
        .children
        .iter()
        .map(|c| resolve_node(c, styles, inherited, scope))
        .collect();
    let empty_children = fl
        .empty_children
        .iter()
        .map(|c| resolve_node(c, styles, inherited, scope))
        .collect();

    DenNode::ForLoop(DenForLoop {
        each_var: fl.each_var.clone(),
        iterable_expr: fl.iterable_expr.clone(),
        children,
        empty_children,
    })
}

fn resolve_if_chain(
    ic: &RawIfChain,
    styles: &StyleMap,
    inherited: &StyleRule,
    scope: &mut Vec<String>,
) -> DenNode {
    let branches = ic
        .branches
        .iter()
        .map(|b| DenIfBranch {
            condition: b.condition.clone(),
            children: b
                .children
                .iter()
                .map(|c| resolve_node(c, styles, inherited, scope))
                .collect(),
        })
        .collect();
    let else_children = ic
        .else_children
        .iter()
        .map(|c| resolve_node(c, styles, inherited, scope))
        .collect();

    DenNode::IfChain(DenIfChain {
        branches,
        else_children,
    })
}

/// `@object(scope) { ... }` — resolve filhos com o scope empilhado e retorna
/// um ElemSyntetico sem visual (usa `div` transparente) ou — melhor — "desaparece",
/// devolvendo os filhos diretamente. Como DenNode só tem 3 variantes visíveis e
/// o codegen quer um único nó, wrapamos num DenNode::Element com tag especial
/// `"__den_object"` e visual default; o codegen emite só os filhos.
fn resolve_object(
    obj: &RawObject,
    styles: &StyleMap,
    inherited: &StyleRule,
    scope: &mut Vec<String>,
) -> DenNode {
    scope.push(obj.scope.clone());
    let children = obj
        .children
        .iter()
        .map(|c| resolve_node(c, styles, inherited, scope))
        .collect();
    scope.pop();

    DenNode::Element(DenElement {
        tag: "__den_object".to_string(),
        classes: Vec::new(),
        on_click: None,
        on_click_args: Vec::new(),
        den_bind: None,
        segments: Vec::new(),
        children,
        visual: DenVisual::default(),
        bind_expr: None,
        placeholder: None,
        goto_page: None,
        goto_with: None,
    })
}

/// Aplica o escopo de `@object` ativo ao raw bind:
/// - `self.x` ou `self` (prefixo explícito) → mantém.
/// - `field` ou `field.sub` → prepend `scope.` usando o último `@object`.
/// - Se não há scope ativo e não começa com `self.`, assume que é erro do dev mas
///   entrega literalmente — o rustc vai pegar.
fn apply_scope(raw: &str, scope: &[String]) -> String {
    let t = raw.trim();
    if t.starts_with("self.") || t == "self" {
        return t.to_string();
    }
    if let Some(current) = scope.last() {
        return format!("{current}.{t}");
    }
    t.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse_scss;
    use crate::parse::html::parse_html_ok;
    use crate::types::{LineHeightValue, TextAlign, TextTransform};

    const HOME_HTML: &str = include_str!("../../den_app/src/pages/home/home.html");
    const HOME_SCSS: &str = include_str!("../../den_app/src/pages/home/home.scss");

    #[test]
    fn home_font_tags_resolve_into_tree_output() {
        let raw_nodes = parse_html_ok(HOME_HTML);
        let styles = parse_scss(HOME_SCSS);
        let output = resolve(&raw_nodes, &styles);

        assert_font_tag(
            &output.nodes,
            "font-serif",
            FontTagExpectation {
                family: r#"Georgia, "Times New Roman", serif"#,
                font_size: 15.0,
                color: Some((192, 192, 216, 255)),
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
                color: Some((92, 219, 149, 255)),
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
                color: Some((240, 160, 80, 255)),
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
                color: Some((233, 69, 96, 255)),
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
                color: Some((126, 184, 247, 255)),
                font_weight: Some(700),
                line_height: Some(LineHeightValue::Factor(1.2)),
                text_align: Some(TextAlign::Center),
                ..Default::default()
            },
        );
    }

    #[test]
    fn text_decoration_does_not_inherit_as_resolved_property() {
        let raw_nodes = parse_html_ok(
            r#"
            <div class="decorated">
                <div class="plain-child">Child</div>
            </div>
            "#,
        );
        let styles = parse_scss(
            r#"
            .decorated {
                text-decoration: underline line-through;
            }

            .plain-child {
                font-size: 12px;
            }
            "#,
        );
        let output = resolve(&raw_nodes, &styles);

        let parent = find_element_by_class(&output.nodes, "decorated").expect("decorated");
        let child = find_element_by_class(&output.nodes, "plain-child").expect("plain-child");

        assert_eq!(parent.visual.underline, Some(true));
        assert_eq!(parent.visual.strikethrough, Some(true));
        assert_eq!(child.visual.underline, None);
        assert_eq!(child.visual.strikethrough, None);
    }

    #[test]
    fn object_scope_applied_to_bind() {
        let raw_nodes = parse_html_ok(
            r#"@object(self.pessoa) {
                <input @bind="nome" />
                <input @bind="telefone" />
            }"#,
        );
        let styles = StyleMap::default();
        let output = resolve(&raw_nodes, &styles);
        let inputs = collect_inputs(&output.nodes);
        assert_eq!(inputs.len(), 2);
        assert_eq!(inputs[0].bind_expr.as_deref(), Some("self.pessoa.nome"));
        assert_eq!(inputs[1].bind_expr.as_deref(), Some("self.pessoa.telefone"));
    }

    #[test]
    fn object_scope_respects_explicit_self() {
        let raw_nodes = parse_html_ok(
            r#"@object(self.pessoa) {
                <input @bind="self.other" />
            }"#,
        );
        let styles = StyleMap::default();
        let output = resolve(&raw_nodes, &styles);
        let inputs = collect_inputs(&output.nodes);
        assert_eq!(inputs[0].bind_expr.as_deref(), Some("self.other"));
    }

    fn collect_inputs(nodes: &[DenNode]) -> Vec<&DenElement> {
        let mut out = Vec::new();
        for n in nodes {
            collect_inputs_into(n, &mut out);
        }
        out
    }

    fn collect_inputs_into<'a>(node: &'a DenNode, out: &mut Vec<&'a DenElement>) {
        match node {
            DenNode::Element(el) => {
                if el.tag == "input" {
                    out.push(el);
                }
                for c in &el.children {
                    collect_inputs_into(c, out);
                }
            }
            DenNode::ForLoop(fl) => {
                for c in &fl.children {
                    collect_inputs_into(c, out);
                }
                for c in &fl.empty_children {
                    collect_inputs_into(c, out);
                }
            }
            DenNode::IfChain(ic) => {
                for b in &ic.branches {
                    for c in &b.children {
                        collect_inputs_into(c, out);
                    }
                }
                for c in &ic.else_children {
                    collect_inputs_into(c, out);
                }
            }
        }
    }

    #[derive(Default)]
    struct FontTagExpectation {
        family: &'static str,
        font_size: f32,
        color: Option<(u8, u8, u8, u8)>,
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
            DenNode::ForLoop(for_loop) => {
                find_element_by_class(&for_loop.children, class_name)
                    .or_else(|| find_element_by_class(&for_loop.empty_children, class_name))
            }
            DenNode::IfChain(if_chain) => {
                for b in &if_chain.branches {
                    if let Some(e) = find_element_by_class(&b.children, class_name) {
                        return Some(e);
                    }
                }
                find_element_by_class(&if_chain.else_children, class_name)
            }
        }
    }
}
