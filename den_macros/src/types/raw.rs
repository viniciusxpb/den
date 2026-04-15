//! Tipos "raw" do parsing HTML (fase 1). Sem visual resolvido.

/// Um segmento de texto — literal ou expressão interpolada `{{ expr }}`.
///
/// Expressões podem ter uma pipeline de transformações aplicadas: `{{ self.name | upper | trim }}`.
#[derive(Debug, Clone)]
pub enum TextSegment {
    Literal(String),
    /// Expressão passada direto do template + lista de pipes aplicados em ordem.
    /// Ex: `{{ self.name | upper | truncate(3) }}` → `Expr { expr: "self.name", pipes: [("upper", []), ("truncate", ["3"])] }`.
    Expr {
        expr: String,
        pipes: Vec<PipeCall>,
    },
}

/// Chamada de pipe com nome e argumentos literais.
#[derive(Debug, Clone)]
pub struct PipeCall {
    pub name: String,
    pub args: Vec<String>,
}

/// Nó raw do HTML parser. Ainda não tem visual resolvido.
#[derive(Debug)]
pub enum RawNode {
    Element(RawElement),
    ForLoop(RawForLoop),
    IfChain(RawIfChain),
    /// `@object(self.pessoa) { ... }` — escopo para bindings curtos.
    Object(RawObject),
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
    /// Expressão de binding bidirecional (e.g. "self.name" ou "nome" dentro de `@object`).
    /// Presente só em `<input @bind="...">`.
    pub bind_expr: Option<String>,
    /// Texto placeholder para inputs.
    pub placeholder: Option<String>,
    /// Nome da página alvo em `@goto="PageName"`.
    pub goto_page: Option<String>,
    /// Expressão opcional de dados para navegação em `@with="expr"`.
    pub goto_with: Option<String>,
}

#[derive(Debug)]
pub struct RawForLoop {
    pub each_var: String,
    pub iterable_expr: String,
    pub children: Vec<RawNode>,
    /// Bloco `@empty { ... }` renderizado quando a iterável está vazia.
    pub empty_children: Vec<RawNode>,
}

/// Cadeia de condições `@if(...) { ... } !cond { ... } !cond { ... } ! { ... }`.
///
/// - `branches` são os ramos com condição (inclui o `@if` inicial e cada `!cond`).
/// - `else_children` é o catch-all `! { ... }` (sem condição), opcional.
#[derive(Debug)]
pub struct RawIfChain {
    pub branches: Vec<RawIfBranch>,
    pub else_children: Vec<RawNode>,
}

#[derive(Debug)]
pub struct RawIfBranch {
    pub condition: String,
    pub children: Vec<RawNode>,
}

/// `@object(scope) { ... }` — bloco que define escopo de binding para `@bind` curtos.
#[derive(Debug)]
pub struct RawObject {
    pub scope: String,
    pub children: Vec<RawNode>,
}
