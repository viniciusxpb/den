// ============================================================================
// Fase 1 output: tipos "raw" do parsing (sem styles resolvidos)
// ============================================================================

/// Um segmento de texto — literal ou expressão interpolada `{{ expr }}`.
#[derive(Debug, Clone)]
pub enum TextSegment {
    Literal(String),
    /// Expressão passada direto do template (templates usam `self.` diretamente).
    Expr(String),
}

/// Nó raw do HTML parser. Ainda não tem visual resolvido.
#[derive(Debug)]
pub enum RawNode {
    Element(RawElement),
    ForLoop(RawForLoop),
    IfChain(RawIfChain),
}

#[derive(Debug)]
pub struct RawElement {
    pub tag: String,
    pub classes: Vec<String>,
    pub segments: Vec<TextSegment>,
    pub children: Vec<RawNode>,
    /// Expressão completa do click handler (e.g. "on_edit(user.id)").
    /// Parseada em func_name + args no resolve.
    pub on_click: Option<String>,
    /// Variável vinculada por `den-bind="var"`. Usada pra auto-clone de args.
    pub den_bind: Option<String>,
}

#[derive(Debug)]
pub struct RawForLoop {
    pub each_var: String,
    pub iterable_expr: String,
    pub children: Vec<RawNode>,
}

#[derive(Debug)]
pub struct RawIfChain {
    pub condition: String,
    pub then_children: Vec<RawNode>,
    pub else_children: Vec<RawNode>,
}
