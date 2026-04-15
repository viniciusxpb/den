//! Dispatch por tipo de `DenNode` e estado acumulado durante o codegen.
//!
//! A implementação concreta de cada variante vive em módulos dedicados:
//! - [`super::element`] pra `DenNode::Element`
//! - [`super::control_flow`] pra `DenNode::ForLoop` e `DenNode::IfChain`
//!
//! `BuildCtx` acumula as tabelas de dispatch (handlers/goto_slots/input_mirrors)
//! que o `codegen/mod.rs` consome pra gerar os match arms do `PaintEvent`.

use super::control_flow::{emit_for_loop, emit_if_chain};
use super::element::emit_element;
use crate::types::DenNode;

/// Posição de um nó dentro da árvore de templates, segmentada por tipo de
/// caminho (filho posicional, branch de `@if`, branch `else`, branch `@empty`).
/// Substitui o antigo `Vec<usize>` com salts mágicos (`EMPTY_BRANCH_SALT`, etc.).
/// O hash de `node_id` consome `&[TreeSegment]`; basta `derive(Hash)` aqui pra
/// que branches diferentes nunca colidam.
#[derive(Debug, Clone, Hash)]
pub(crate) enum TreeSegment {
    /// Filho posicional `i` dentro de um pai (elemento, loop, branch).
    Child(usize),
    /// Filho dentro do branch número `i` de um `@if` chain (`@if`/`!cond`).
    IfBranch(usize),
    /// Filho dentro do branch `else` (`!` sem condição).
    ElseBranch,
    /// Filho dentro do branch `@empty` de um `@for`.
    EmptyBranch,
}

/// Estado acumulado durante o codegen da RenderTree.
pub(crate) struct BuildCtx<'a> {
    pub has_self: bool,
    pub template_path: &'a str,
    pub tree_path: Vec<TreeSegment>,
    pub loop_depth: usize,

    /// Chamadas Rust registradas pra `(click)`; índice == slot.
    pub handlers: Vec<proc_macro2::TokenStream>,
    /// Chamadas `__den_router.goto(...)` registradas; índice == slot.
    pub goto_slots: Vec<proc_macro2::TokenStream>,
    /// Statements que espelham `InputChanged` de volta pro page field.
    pub input_mirrors: Vec<proc_macro2::TokenStream>,
}

impl<'a> BuildCtx<'a> {
    pub fn new(has_self: bool, template_path: &'a str) -> Self {
        Self {
            has_self,
            template_path,
            tree_path: Vec::new(),
            loop_depth: 0,
            handlers: Vec::new(),
            goto_slots: Vec::new(),
            input_mirrors: Vec::new(),
        }
    }
}

/// Entry point — emite statements de build pra um `DenNode`.
/// Dispatch simples por variante; cada variante tem seu próprio módulo.
pub(crate) fn emit_build_node(
    node: &DenNode,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    match node {
        DenNode::Element(el) => emit_element(el, ctx),
        DenNode::ForLoop(fl) => emit_for_loop(fl, ctx),
        DenNode::IfChain(ic) => emit_if_chain(ic, ctx),
    }
}
