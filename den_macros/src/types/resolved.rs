//! Tipos resolvidos (fase 2): nós com visual linkado.

use super::raw::TextSegment;
use super::style::*;

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
    pub height: WidthValue,
    pub cursor_pointer: bool,
    /// `flex: 1` — cresce pra preencher o share do flex pai igualmente.
    pub flex_grow: bool,
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
            height: rule.height,
            cursor_pointer: rule.cursor_pointer,
            flex_grow: rule.flex_grow,
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
    ///
    /// INVARIANTE: esta função DEVE ser atualizada junto com StyleRule::merge_from
    /// quando adicionar nova propriedade CSS.
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
        if other.height != WidthValue::Auto {
            self.height = other.height;
        }
        if other.cursor_pointer {
            self.cursor_pointer = true;
        }
        if other.flex_grow {
            self.flex_grow = true;
        }
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
    #[allow(dead_code)] // Reservado para propagação de scale no resolve. O scale system atual entra no codegen. Ver PENDING.md.
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
#[allow(clippy::large_enum_variant)]
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
    /// Nome da função do click handler (sem args, sem parens).
    pub on_click: Option<String>,
    /// Argumentos parseados do click handler (e.g. ["user.id", "user.name"]).
    pub on_click_args: Vec<String>,
    /// Variável vinculada por `den-bind="var"`.
    pub den_bind: Option<String>,
    pub segments: Vec<TextSegment>,
    pub children: Vec<DenNode>,
    pub visual: DenVisual,
    /// Expressão de binding bidirecional (e.g. "self.name").
    /// Presente só em `<input bind="...">`.
    pub bind_expr: Option<String>,
    /// Texto placeholder para inputs.
    pub placeholder: Option<String>,
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
