mod control_flow;
mod element;
mod frame;
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
    /// Filhos Auto de flex usam `__den_flex_share` (calculado pelo pai)
    /// em vez de deixar o egui decidir pelo conteúdo.
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
                // resolve() em CSS pixels; render multiplica por __den_scale
                __den_layout.resolve(ui.available_width() / __den_scale);
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
/// Espelha a ordem DFS do codegen pra garantir que os layout_index coincidem.
struct FlatEntry {
    index: usize,
    parent: usize,
    width: WidthValue,
    display: DisplayMode,
    depth: usize,
}

/// DFS sobre DenNodes na mesma ordem que generate_node — garante que os
/// layout_index atribuídos aqui coincidem com os do codegen.
///
/// INVARIANTE: esta função DEVE caminhar na mesma ordem DFS que
/// `collect_flex_children_info`/`skip_descendants` em `element.rs` e
/// `generate_element`. Se adicionar um novo `DenNode` variant, atualizar as TRÊS.
fn collect_flat_entries(
    nodes: &[DenNode],
    parent: usize,
    depth: usize,
    counter: &mut usize,
    out: &mut Vec<FlatEntry>,
) {
    for node in nodes {
        match node {
            DenNode::Element(el) => {
                let idx = *counter;
                *counter += 1;
                out.push(FlatEntry {
                    index: idx,
                    parent,
                    width: el.visual.width,
                    display: el.visual.display,
                    depth,
                });
                // Filhos de ForLoop/IfChain dentro deste elemento são recursados
                // pelo mesmo caminho — sem pular profundidade.
                collect_flat_entries(&el.children, idx, depth + 1, counter, out);
            }
            // ForLoop e IfChain são transparentes: filhos pertencem ao mesmo pai.
            DenNode::ForLoop(fl) => {
                collect_flat_entries(&fl.children, parent, depth, counter, out);
            }
            DenNode::IfChain(ic) => {
                collect_flat_entries(&ic.then_children, parent, depth, counter, out);
                collect_flat_entries(&ic.else_children, parent, depth, counter, out);
            }
        }
    }
}

/// Gera o bloco de inicialização da LayoutTable:
/// ```rust,ignore
/// { let mut __e = vec![...]; /* fill children */ __e }
/// ```
fn generate_layout_init(nodes: &[DenNode]) -> proc_macro2::TokenStream {
    let mut counter = 1usize; // 0 = body
    let mut flat = Vec::new();
    collect_flat_entries(nodes, 0, 1, &mut counter, &mut flat);

    // Entry do body (índice 0, raiz invisível)
    let mut entries_code = vec![quote! {
        den_layout::LayoutEntry {
            index: 0,
            parent: None,
            children: vec![],
            depth: 0,
            width_rule: den_layout::WidthRule::Auto,
            display: den_layout::DisplayMode::Block,
        }
    }];

    // Entries dos elementos do template
    for e in &flat {
        let idx = e.index;
        let parent = e.parent;
        let depth = e.depth;
        let width_ts = match e.width {
            WidthValue::Auto => quote! { den_layout::WidthRule::Auto },
            WidthValue::Px(v) => quote! { den_layout::WidthRule::Px(#v) },
            WidthValue::Percent(v) => quote! { den_layout::WidthRule::Percent(#v) },
        };
        let display_ts = if e.display == DisplayMode::Flex {
            quote! { den_layout::DisplayMode::Flex }
        } else {
            quote! { den_layout::DisplayMode::Block }
        };
        entries_code.push(quote! {
            den_layout::LayoutEntry {
                index: #idx,
                parent: Some(#parent),
                children: vec![],
                depth: #depth,
                width_rule: #width_ts,
                display: #display_ts,
            }
        });
    }

    // Bloco que constrói o Vec e depois preenche os children a partir dos parents
    quote! {
        {
            let mut __e: Vec<den_layout::LayoutEntry> = vec![ #( #entries_code ),* ];
            for __i in 1..__e.len() {
                let __p = __e[__i].parent.unwrap_or(0);
                let __c = __e[__i].index;
                __e[__p].children.push(__c);
            }
            __e
        }
    }
}
