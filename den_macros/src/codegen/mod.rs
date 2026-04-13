//! Fase 3 do pipeline: geração de código egui via `quote!`.

mod click;
mod control_flow;
mod element;
mod flex;
mod frame;
mod input;
mod navigation;
mod text;

use crate::types::{DenNode, DisplayMode, WidthValue};
use quote::quote;

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
    /// Filhos Auto sem flex-grow deixam o egui medir conteúdo.
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
            }
            __DEN_LAYOUT_STORE.with(|__tl| {
                let mut __den_layout = __tl.borrow_mut();
                // resolve_in_viewport() em CSS pixels; render multiplica por __den_scale
                __den_layout.resolve_in_viewport(
                    ui.available_width() / __den_scale,
                    ui.available_height() / __den_scale,
                );
                __den_layout.distribute_flex();
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
    parent: usize,
    width: WidthValue,
    height: WidthValue,
    display: DisplayMode,
    padding: Option<f32>,
    gap: Option<f32>,
    flex_grow: bool,
}

/// Coleta flat entries usando `walk_den_nodes` (fonte única de verdade pra DFS).
fn collect_flat_entries(nodes: &[DenNode]) -> Vec<FlatEntry> {
    let mut entries = Vec::new();
    let mut counter = 1usize; // 0 = body

    crate::types::walk_den_nodes(nodes, 0, &mut counter, &mut |el, _idx, parent| {
        entries.push(FlatEntry {
            parent,
            width: el.visual.width,
            height: el.visual.height,
            display: el.visual.display,
            padding: el.visual.padding,
            gap: el.visual.gap,
            flex_grow: el.visual.flex_grow,
        });
    });

    entries
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
            width_rule: den_layout::WidthRule::Auto,
            height_rule: den_layout::WidthRule::Auto,
            display: den_layout::DisplayMode::Block,
            padding: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
        }
    }];

    for e in &flat {
        let parent = e.parent;
        let width_ts = match e.width {
            WidthValue::Auto => quote! { den_layout::WidthRule::Auto },
            WidthValue::Px(v) => quote! { den_layout::WidthRule::Px(#v) },
            WidthValue::Percent(v) => quote! { den_layout::WidthRule::Percent(#v) },
        };
        let height_ts = match e.height {
            WidthValue::Auto => quote! { den_layout::WidthRule::Auto },
            WidthValue::Px(v) => quote! { den_layout::WidthRule::Px(#v) },
            WidthValue::Percent(v) => quote! { den_layout::WidthRule::Percent(#v) },
        };
        let display_ts = match e.display {
            DisplayMode::Flex => quote! { den_layout::DisplayMode::Flex },
            DisplayMode::Grid => quote! { den_layout::DisplayMode::Grid },
            DisplayMode::Block => quote! { den_layout::DisplayMode::Block },
        };
        let padding = e.padding.unwrap_or(0.0);
        let gap = e.gap.unwrap_or(0.0);
        let flex_grow = if e.flex_grow { 1.0 } else { 0.0 };
        entries_code.push(quote! {
            den_layout::LayoutEntry {
                parent: Some(#parent),
                children: vec![],
                width_rule: #width_ts,
                height_rule: #height_ts,
                display: #display_ts,
                padding: #padding as f32,
                gap: #gap as f32,
                flex_grow: #flex_grow as f32,
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
