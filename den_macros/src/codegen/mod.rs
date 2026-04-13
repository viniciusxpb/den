//! Fase 3 do pipeline: geração de código egui via `quote!`.

mod click;
mod control_flow;
mod egui_backend;
mod element;
mod flex;
mod input;
mod navigation;
mod text;

use crate::types::{DenNode, DisplayMode, TextSegment, WidthValue};
use quote::quote;

/// Altura de linha usada quando texto não define `font-size`.
const DEFAULT_TEXT_LINE_HEIGHT: f32 = 14.0;

/// Altura de linha usada para inputs sem `font-size`.
const DEFAULT_INPUT_LINE_HEIGHT: f32 = 16.0;

/// Largura média de glifo usada na estimativa textual.
const AVERAGE_GLYPH_WIDTH_RATIO: f32 = 0.55;

/// Largura estimada para expressões dinâmicas desconhecidas.
const DEFAULT_EXPR_TEXT_WIDTH: f32 = 48.0;

/// Largura estimada para inputs sem largura explícita.
const DEFAULT_INPUT_WIDTH: f32 = 180.0;

/// Contexto passado pelo codegen pra rastrear posição na árvore.
pub(crate) struct CodegenCtx<'a> {
    pub has_self: bool,
    pub template_path: &'a str,
    pub tree_path: Vec<usize>,
    pub loop_depth: usize,
    /// Índice deste elemento na LayoutTable. Incrementado a cada elemento.
    /// 0 é reservado pro body (invisível, raiz). Começa em 1.
    pub layout_index: usize,
    /// `true` quando o pai direto deste elemento é `display: flex`.
    /// Filhos Auto sem flex-grow usam largura intrínseca estimada.
    pub parent_is_flex: bool,
}

/// Gera TokenStream a partir da árvore resolvida.
pub fn generate(
    nodes: &[DenNode],
    has_self: bool,
    template_path: &str,
) -> Result<proc_macro2::TokenStream, String> {
    // Pré-passo: coleta entries pra LayoutTable (mesma ordem DFS do codegen).
    let entries_init = generate_layout_init(nodes);
    let layout_labels = generate_layout_labels(nodes);

    let mut ctx = CodegenCtx {
        has_self,
        template_path,
        tree_path: Vec::new(),
        loop_depth: 0,
        layout_index: 1, // 0 é o body
        parent_is_flex: false,
    };

    let mut stmts = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        ctx.tree_path.push(i);
        stmts.push(generate_node(node, &mut ctx)?);
        ctx.tree_path.pop();
    }

    // Envolve o render num thread_local pra reutilizar a LayoutTable entre frames.
    Ok(quote! {
        {
            ::std::thread_local! {
                static __DEN_LAYOUT_STORE: ::std::cell::RefCell<den_layout::LayoutTable> =
                    ::std::cell::RefCell::new(den_layout::LayoutTable::new(#entries_init));
                static __DEN_LAYOUT_DEBUG_DUMPED: ::std::cell::Cell<bool> =
                    ::std::cell::Cell::new(false);
            }
            __DEN_LAYOUT_STORE.with(|__tl| {
                let mut __den_layout = __tl.borrow_mut();
                // resolve_in_viewport() em CSS pixels; render multiplica por __den_scale
                __den_layout.resolve_in_viewport(
                    ui.available_width() / __den_scale,
                    ui.available_height() / __den_scale,
                );
                __den_layout.distribute_flex();
                if den_layout::layout_debug_enabled() {
                    __DEN_LAYOUT_DEBUG_DUMPED.with(|__dumped| {
                        if !__dumped.get() {
                            __den_layout.debug_dump(#template_path, #layout_labels);
                            __dumped.set(true);
                        }
                    });
                }
                #( #stmts )*
            });
        }
    })
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

// ============================================================================
// Layout entries — pré-passo que espelha exatamente a ordem DFS do codegen
// ============================================================================

/// Entrada temporária pro pré-passo de layout.
struct FlatEntry {
    label: String,
    parent: usize,
    width: WidthValue,
    height: WidthValue,
    display: DisplayMode,
    padding: Option<f32>,
    margin: Option<f32>,
    gap: Option<f32>,
    flex_grow: bool,
    intrinsic_width: f32,
    intrinsic_height: f32,
}

/// Coleta flat entries usando `walk_den_nodes` (fonte única de verdade pra DFS).
fn collect_flat_entries(nodes: &[DenNode]) -> Vec<FlatEntry> {
    let mut entries = Vec::new();
    let mut counter = 1usize; // 0 = body

    crate::types::walk_den_nodes(nodes, 0, &mut counter, &mut |el, _idx, parent| {
        entries.push(FlatEntry {
            label: layout_label(el),
            parent,
            width: el.visual.width,
            height: el.visual.height,
            display: el.visual.display,
            padding: el.visual.padding,
            margin: el.visual.margin,
            gap: el.visual.gap,
            flex_grow: el.visual.flex_grow,
            intrinsic_width: intrinsic_width_for(el),
            intrinsic_height: intrinsic_height_for(el),
        });
    });

    entries
}

/// Estima a largura própria de um elemento antes do renderer backend medir texto.
fn intrinsic_width_for(el: &crate::types::DenElement) -> f32 {
    if el.bind_expr.is_some() {
        return DEFAULT_INPUT_WIDTH;
    }

    if el.segments.is_empty() {
        return 0.0;
    }

    let font_size = el.visual.font_size.unwrap_or(DEFAULT_TEXT_LINE_HEIGHT);
    el.segments
        .iter()
        .map(|segment| match segment {
            TextSegment::Literal(text) => {
                text.chars().count() as f32 * font_size * AVERAGE_GLYPH_WIDTH_RATIO
            }
            TextSegment::Expr(_) => DEFAULT_EXPR_TEXT_WIDTH,
        })
        .sum()
}

/// Estima a altura própria de um elemento antes do renderer backend medir texto.
fn intrinsic_height_for(el: &crate::types::DenElement) -> f32 {
    if el.bind_expr.is_some() {
        return el.visual.font_size.unwrap_or(DEFAULT_INPUT_LINE_HEIGHT);
    }

    if el.segments.is_empty() {
        0.0
    } else {
        el.visual.font_size.unwrap_or(DEFAULT_TEXT_LINE_HEIGHT)
    }
}

/// Monta um label estável e legível para dumps de layout.
fn layout_label(el: &crate::types::DenElement) -> String {
    if el.classes.is_empty() {
        el.tag.clone()
    } else {
        format!("{}.{}", el.tag, el.classes.join("."))
    }
}

/// Gera o bloco de inicialização da LayoutTable:
/// ```rust,ignore
/// { let mut __e = vec![...]; /* fill children */ __e }
/// ```
fn generate_layout_init(nodes: &[DenNode]) -> proc_macro2::TokenStream {
    let flat = collect_flat_entries(nodes);

    // Entry do body (índice 0, raiz invisível)
    let mut entries_code = vec![quote! {
        den_layout::LayoutEntry {
            parent: None,
            children: vec![],
            width_rule: den_layout::DimensionRule::Auto,
            height_rule: den_layout::DimensionRule::Auto,
            display: den_layout::DisplayMode::Block,
            padding: 0.0,
            margin: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
            intrinsic_width: 0.0,
            intrinsic_height: 0.0,
        }
    }];

    for e in &flat {
        let parent = e.parent;
        let width_ts = match e.width {
            WidthValue::Auto => quote! { den_layout::DimensionRule::Auto },
            WidthValue::Px(v) => quote! { den_layout::DimensionRule::Px(#v) },
            WidthValue::Percent(v) => quote! { den_layout::DimensionRule::Percent(#v) },
        };
        let height_ts = match e.height {
            WidthValue::Auto => quote! { den_layout::DimensionRule::Auto },
            WidthValue::Px(v) => quote! { den_layout::DimensionRule::Px(#v) },
            WidthValue::Percent(v) => quote! { den_layout::DimensionRule::Percent(#v) },
        };
        let display_ts = match e.display {
            DisplayMode::Flex => quote! { den_layout::DisplayMode::Flex },
            DisplayMode::Grid => quote! { den_layout::DisplayMode::Grid },
            DisplayMode::Block => quote! { den_layout::DisplayMode::Block },
        };
        let padding = e.padding.unwrap_or(0.0);
        let margin = e.margin.unwrap_or(0.0);
        let gap = e.gap.unwrap_or(0.0);
        let flex_grow = if e.flex_grow { 1.0 } else { 0.0 };
        let intrinsic_width = e.intrinsic_width;
        let intrinsic_height = e.intrinsic_height;
        entries_code.push(quote! {
            den_layout::LayoutEntry {
                parent: Some(#parent),
                children: vec![],
                width_rule: #width_ts,
                height_rule: #height_ts,
                display: #display_ts,
                padding: #padding as f32,
                margin: #margin as f32,
                gap: #gap as f32,
                flex_grow: #flex_grow as f32,
                intrinsic_width: #intrinsic_width as f32,
                intrinsic_height: #intrinsic_height as f32,
            }
        });
    }

    // Preenche children a partir dos parent refs em runtime (uma vez).
    // expect() pra detectar bug: todo entry não-body DEVE ter parent.
    quote! {
        {
            let mut __e: Vec<den_layout::LayoutEntry> = vec![ #( #entries_code ),* ];
            for __i in 1..__e.len() {
                let __p = __e[__i].parent
                    .expect("Den: non-body layout entry must have parent");
                __e[__p].children.push(__i);
            }
            __e
        }
    }
}

/// Gera labels paralelos à LayoutTable para debug textual.
fn generate_layout_labels(nodes: &[DenNode]) -> proc_macro2::TokenStream {
    let mut labels = vec!["body".to_string()];
    labels.extend(
        collect_flat_entries(nodes)
            .into_iter()
            .map(|entry| entry.label),
    );

    quote! {
        &[ #( #labels ),* ]
    }
}
