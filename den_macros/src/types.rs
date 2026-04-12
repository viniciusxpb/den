//! Tipos compartilhados entre as fases parse, resolve e codegen.
//!
//! Este módulo não contém lógica, só definições de dados.

use std::collections::HashMap;

// ============================================================================
// Fase 1 output: tipos "raw" do parsing (sem styles resolvidos)
// ============================================================================

/// Um segmento de texto — literal ou expressão interpolada `{{ expr }}`.
#[derive(Debug, Clone)]
pub enum TextSegment {
    Literal(String),
    /// Expressão já com `this` mapeado pra `self`.
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
    pub on_click: Option<String>,
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

// ============================================================================
// SCSS types
// ============================================================================

pub type RgbColor = (u8, u8, u8);

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum DisplayMode {
    #[default]
    Block,
    Flex,
}

#[derive(Debug, Clone, Copy)]
pub struct BorderStyle {
    pub width: f32,
    pub color: RgbColor,
}

impl Default for BorderStyle {
    fn default() -> Self {
        Self {
            width: 1.0,
            color: (0, 0, 0),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum WidthValue {
    #[default]
    Auto,
    Percent(f32),
    Px(f32),
}

/// Regra de estilo bruta, output do SCSS parser.
#[derive(Debug, Clone, Default)]
pub struct StyleRule {
    pub color: Option<RgbColor>,
    pub font_size: Option<f32>,
    pub background: Option<RgbColor>,
    pub padding: Option<f32>,
    pub display: DisplayMode,
    pub border: Option<BorderStyle>,
    pub border_radius: Option<f32>,
    pub width: WidthValue,
    pub cursor_pointer: bool,
    pub hover: Option<Box<StyleRule>>,
}

impl StyleRule {
    /// Merge outra regra nesta (last-wins pra propriedades definidas).
    pub fn merge_from(&mut self, other: &Self) {
        if other.color.is_some() {
            self.color = other.color;
        }
        if other.font_size.is_some() {
            self.font_size = other.font_size;
        }
        if other.background.is_some() {
            self.background = other.background;
        }
        if other.padding.is_some() {
            self.padding = other.padding;
        }
        if other.display != DisplayMode::Block {
            self.display = other.display;
        }
        if other.border.is_some() {
            self.border = other.border;
        }
        if other.border_radius.is_some() {
            self.border_radius = other.border_radius;
        }
        if other.width != WidthValue::Auto {
            self.width = other.width;
        }
        if other.cursor_pointer {
            self.cursor_pointer = true;
        }
        if other.hover.is_some() {
            self.hover = other.hover.clone();
        }
    }

    /// Extrai só propriedades herdáveis (color, font-size) pra propagar pros filhos.
    /// Hover NÃO é herdável.
    pub fn inheritable(&self) -> Self {
        Self {
            color: self.color,
            font_size: self.font_size,
            ..Default::default()
        }
    }
}

/// Mapa de classe CSS → StyleRule.
pub type StyleMap = HashMap<String, StyleRule>;

// ============================================================================
// Fase 2 output: nós com visual resolvido
// ============================================================================

/// Visual resolvido de um elemento. Derivado dos StyleRules aplicados.
///
/// Cada DenVisual sabe como se renderizar e como muda em hover.
/// Não sabe nada sobre filhos ou eventos.
#[derive(Debug, Clone, Default)]
pub struct DenVisual {
    pub color: Option<RgbColor>,
    pub font_size: Option<f32>,
    pub background: Option<RgbColor>,
    pub padding: Option<f32>,
    pub display: DisplayMode,
    pub border: Option<BorderStyle>,
    pub border_radius: Option<f32>,
    pub width: WidthValue,
    pub cursor_pointer: bool,
    /// Visual override quando hover. None = sem hover behavior.
    pub hover_override: Option<Box<DenVisual>>,
}

impl DenVisual {
    /// Constrói a partir de um StyleRule resolvido.
    pub fn from_style_rule(rule: &StyleRule) -> Self {
        Self {
            color: rule.color,
            font_size: rule.font_size,
            background: rule.background,
            padding: rule.padding,
            display: rule.display,
            border: rule.border,
            border_radius: rule.border_radius,
            width: rule.width,
            cursor_pointer: rule.cursor_pointer,
            hover_override: rule
                .hover
                .as_ref()
                .map(|h| Box::new(Self::from_style_rule(h))),
        }
    }

    /// Precisa de um egui::Frame wrapper?
    pub fn needs_frame(&self) -> bool {
        self.background.is_some()
            || self.padding.is_some()
            || self.border.is_some()
            || self.border_radius.is_some()
    }

    /// Tem hover behavior?
    pub fn needs_hover(&self) -> bool {
        self.hover_override.is_some()
    }

    /// Merge outro visual neste (last-wins pra propriedades definidas).
    /// Ponto único de merge — adicionar nova propriedade CSS aqui só.
    pub fn merge_from(&mut self, other: &Self) {
        if other.color.is_some() { self.color = other.color; }
        if other.font_size.is_some() { self.font_size = other.font_size; }
        if other.background.is_some() { self.background = other.background; }
        if other.padding.is_some() { self.padding = other.padding; }
        if other.display != DisplayMode::Block { self.display = other.display; }
        if other.border.is_some() { self.border = other.border; }
        if other.border_radius.is_some() { self.border_radius = other.border_radius; }
        if other.width != WidthValue::Auto { self.width = other.width; }
        if other.cursor_pointer { self.cursor_pointer = true; }
    }

    /// Resolve o visual final em estado hover (base + overrides).
    pub fn resolve_hover(&self) -> Self {
        let mut hovered = self.clone();
        if let Some(h) = &self.hover_override {
            hovered.merge_from(h);
        }
        hovered.hover_override = None;
        hovered
    }

    /// Extrai só propriedades herdáveis pra propagar pros filhos.
    #[allow(dead_code)] // PENDING.md — usado pelo scale system futuro
    pub fn inheritable(&self) -> Self {
        Self {
            color: self.color,
            font_size: self.font_size,
            ..Default::default()
        }
    }
}

/// Nó resolvido: lógica + visual linkados.
#[derive(Debug)]
pub enum DenNode {
    Element(DenElement),
    ForLoop(DenForLoop),
    IfChain(DenIfChain),
}

/// Elemento resolvido. Sabe o que é (tag, eventos) e como parece (visual).
#[derive(Debug)]
pub struct DenElement {
    pub tag: String,
    pub classes: Vec<String>,
    pub on_click: Option<String>,
    pub segments: Vec<TextSegment>,
    pub children: Vec<DenNode>,
    pub visual: DenVisual,
}

/// ForLoop resolvido. Transparente visualmente.
#[derive(Debug)]
pub struct DenForLoop {
    pub each_var: String,
    pub iterable_expr: String,
    pub children: Vec<DenNode>,
}

/// IfChain resolvido. Transparente visualmente.
#[derive(Debug)]
pub struct DenIfChain {
    pub condition: String,
    pub then_children: Vec<DenNode>,
    pub else_children: Vec<DenNode>,
}

// ============================================================================
// DFS genérico — fonte única de verdade pra ordem de travessia
// ============================================================================

/// Caminha a árvore de `DenNode` em DFS pré-ordem, chamando `visitor` pra cada
/// `DenElement` encontrado. `ForLoop` e `IfChain` são transparentes: seus filhos
/// pertencem ao pai do control flow.
///
/// `counter` é incrementado pra cada `DenElement` visitado (layout_index).
///
/// Toda função que precise atribuir layout indices ou iterar elementos na mesma
/// ordem do codegen DEVE usar esta função. Isso garante que a ordem de travessia
/// é definida num único lugar.
pub fn walk_den_nodes<F>(
    nodes: &[DenNode],
    parent_index: usize,
    counter: &mut usize,
    visitor: &mut F,
)
where
    F: FnMut(&DenElement, usize, usize), // (element, my_index, parent_index)
{
    for node in nodes {
        match node {
            DenNode::Element(el) => {
                let idx = *counter;
                *counter += 1;
                visitor(el, idx, parent_index);
                walk_den_nodes(&el.children, idx, counter, visitor);
            }
            // ForLoop e IfChain são transparentes: parent_index não muda.
            DenNode::ForLoop(fl) => {
                walk_den_nodes(&fl.children, parent_index, counter, visitor);
            }
            DenNode::IfChain(ic) => {
                walk_den_nodes(&ic.then_children, parent_index, counter, visitor);
                walk_den_nodes(&ic.else_children, parent_index, counter, visitor);
            }
        }
    }
}
