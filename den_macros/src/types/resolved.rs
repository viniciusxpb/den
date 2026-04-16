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
    pub font_family: Option<String>,
    pub font_weight: Option<u16>,
    pub font_italic: Option<bool>,
    pub line_height: Option<LineHeightValue>,
    pub letter_spacing: Option<f32>,
    pub text_transform: Option<TextTransform>,
    pub text_align: Option<TextAlign>,
    pub underline: Option<bool>,
    pub strikethrough: Option<bool>,
    /// Background shorthand (cor sólida ou gradient). Ver regra `Option<T>` em [`super::style`].
    pub background: Option<Background>,
    pub padding: Option<f32>,
    pub margin: Option<f32>,
    /// Ver regra `Option<T>` em [`super::style`] (topo do arquivo).
    pub display: Option<DisplayMode>,
    pub border: Option<BorderStyle>,
    pub border_radius: Option<f32>,
    /// Ver regra `Option<T>` em [`super::style`].
    pub width: Option<WidthValue>,
    /// Ver regra `Option<T>` em [`super::style`].
    pub height: Option<WidthValue>,
    pub min_width: Option<WidthValue>,
    pub max_width: Option<WidthValue>,
    pub min_height: Option<WidthValue>,
    pub max_height: Option<WidthValue>,
    pub gap: Option<f32>,
    /// Ver regra `Option<T>` em [`super::style`].
    pub cursor_pointer: Option<bool>,
    /// `flex: 1` — `Some(true)` cresce, `None` = não declarado.
    pub flex_grow: Option<bool>,
    /// Esquema de posicionamento CSS. Ver regra `Option<T>` em [`super::style`].
    pub position: Option<PositionKind>,
    /// Offsets do containing block (só aplicados se `position != Static`).
    pub top: Option<WidthValue>,
    pub left: Option<WidthValue>,
    pub right: Option<WidthValue>,
    pub bottom: Option<WidthValue>,
    /// `z-index` — ordena paint entre positioned siblings. `None` = auto (= 0).
    pub z_index: Option<i32>,
    /// `opacity: 0..1` — multiplicador aplicado ao alpha de todas as cores no paint.
    /// `None` = não declarado (= 1.0 opaco).
    pub opacity: Option<f32>,
    /// `white-space: nowrap` declarado. Default Den é single-line.
    pub white_space_nowrap: Option<bool>,
    /// `text-overflow: ellipsis` — trunca com `…` quando texto não cabe no rect.
    pub text_overflow_ellipsis: Option<bool>,
    /// Lista de `box-shadow`s. `None` = não declarado, `Some(vec![])` = `none`
    /// explícito. Ver regra `Option<T>` em [`super::style`].
    pub box_shadows: Option<Vec<BoxShadow>>,
    /// `flex-direction`. Ver regra `Option<T>` em [`super::style`].
    pub flex_direction: Option<FlexDirection>,
    /// `align-items` — alinhamento cruzado. Ver regra `Option<T>` em [`super::style`].
    pub align_items: Option<AlignItems>,
    /// `justify-content`. Ver regra `Option<T>` em [`super::style`].
    pub justify_content: Option<JustifyContent>,
    /// `overflow: visible|hidden`. Ver regra `Option<T>` em [`super::style`].
    pub overflow: Option<OverflowKind>,
    /// `transform: rotate(...)`. Ver regra `Option<T>` em [`super::style`].
    pub transform: Option<Transform2d>,
    /// Visual override quando hover. None = sem hover behavior.
    pub hover_override: Option<Box<DenVisual>>,
}

impl DenVisual {
    /// Constrói a partir de um StyleRule resolvido.
    pub fn from_style_rule(rule: &StyleRule) -> Self {
        Self {
            color: rule.color,
            font_size: rule.font_size,
            font_family: rule.font_family.clone(),
            font_weight: rule.font_weight,
            font_italic: rule.font_italic,
            line_height: rule.line_height,
            letter_spacing: rule.letter_spacing,
            text_transform: rule.text_transform,
            text_align: rule.text_align,
            underline: rule.underline,
            strikethrough: rule.strikethrough,
            background: rule.background.clone(),
            padding: rule.padding,
            margin: rule.margin,
            display: rule.display,
            border: rule.border,
            border_radius: rule.border_radius,
            width: rule.width,
            height: rule.height,
            min_width: rule.min_width,
            max_width: rule.max_width,
            min_height: rule.min_height,
            max_height: rule.max_height,
            gap: rule.gap,
            cursor_pointer: rule.cursor_pointer,
            flex_grow: rule.flex_grow,
            position: rule.position,
            top: rule.top,
            left: rule.left,
            right: rule.right,
            bottom: rule.bottom,
            z_index: rule.z_index,
            opacity: rule.opacity,
            white_space_nowrap: rule.white_space_nowrap,
            text_overflow_ellipsis: rule.text_overflow_ellipsis,
            box_shadows: rule.box_shadows.clone(),
            // (Option preservada — codegen aplica .as_deref().unwrap_or_default()
            // pra emitir Vec<BoxShadow> concreto no PaintStyle.)
            flex_direction: rule.flex_direction,
            align_items: rule.align_items,
            justify_content: rule.justify_content,
            overflow: rule.overflow,
            transform: rule.transform,
            hover_override: rule
                .hover
                .as_ref()
                .map(|h| Box::new(Self::from_style_rule(h))),
        }
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
        if other.font_family.is_some() {
            self.font_family = other.font_family.clone();
        }
        if other.font_weight.is_some() {
            self.font_weight = other.font_weight;
        }
        if other.font_italic.is_some() {
            self.font_italic = other.font_italic;
        }
        if other.line_height.is_some() {
            self.line_height = other.line_height;
        }
        if other.letter_spacing.is_some() {
            self.letter_spacing = other.letter_spacing;
        }
        if other.text_transform.is_some() {
            self.text_transform = other.text_transform;
        }
        if other.text_align.is_some() {
            self.text_align = other.text_align;
        }
        if other.underline.is_some() {
            self.underline = other.underline;
        }
        if other.strikethrough.is_some() {
            self.strikethrough = other.strikethrough;
        }
        if other.background.is_some() {
            self.background = other.background.clone();
        }
        if other.padding.is_some() {
            self.padding = other.padding;
        }
        if other.margin.is_some() {
            self.margin = other.margin;
        }
        if other.display.is_some() {
            self.display = other.display;
        }
        if other.border.is_some() {
            self.border = other.border;
        }
        if other.border_radius.is_some() {
            self.border_radius = other.border_radius;
        }
        if other.width.is_some() {
            self.width = other.width;
        }
        if other.height.is_some() {
            self.height = other.height;
        }
        if other.min_width.is_some() {
            self.min_width = other.min_width;
        }
        if other.max_width.is_some() {
            self.max_width = other.max_width;
        }
        if other.min_height.is_some() {
            self.min_height = other.min_height;
        }
        if other.max_height.is_some() {
            self.max_height = other.max_height;
        }
        if other.gap.is_some() {
            self.gap = other.gap;
        }
        if other.cursor_pointer.is_some() {
            self.cursor_pointer = other.cursor_pointer;
        }
        if other.flex_grow.is_some() {
            self.flex_grow = other.flex_grow;
        }
        if other.position.is_some() {
            self.position = other.position;
        }
        if other.top.is_some() {
            self.top = other.top;
        }
        if other.left.is_some() {
            self.left = other.left;
        }
        if other.right.is_some() {
            self.right = other.right;
        }
        if other.bottom.is_some() {
            self.bottom = other.bottom;
        }
        if other.z_index.is_some() {
            self.z_index = other.z_index;
        }
        if other.opacity.is_some() {
            self.opacity = other.opacity;
        }
        if other.white_space_nowrap.is_some() {
            self.white_space_nowrap = other.white_space_nowrap;
        }
        if other.text_overflow_ellipsis.is_some() {
            self.text_overflow_ellipsis = other.text_overflow_ellipsis;
        }
        if other.box_shadows.is_some() {
            self.box_shadows = other.box_shadows.clone();
        }
        if other.flex_direction.is_some() {
            self.flex_direction = other.flex_direction;
        }
        if other.align_items.is_some() {
            self.align_items = other.align_items;
        }
        if other.justify_content.is_some() {
            self.justify_content = other.justify_content;
        }
        if other.overflow.is_some() {
            self.overflow = other.overflow;
        }
        if other.transform.is_some() {
            self.transform = other.transform;
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
            font_family: self.font_family.clone(),
            font_weight: self.font_weight,
            font_italic: self.font_italic,
            line_height: self.line_height,
            letter_spacing: self.letter_spacing,
            text_transform: self.text_transform,
            text_align: self.text_align,
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
    #[allow(dead_code)]
    pub den_bind: Option<String>,
    pub segments: Vec<TextSegment>,
    pub children: Vec<DenNode>,
    pub visual: DenVisual,
    /// Expressão de binding bidirecional (e.g. "self.name").
    /// Presente só em `<input @bind="...">`.
    /// Já resolvida com escopo de `@object` aplicado.
    pub bind_expr: Option<String>,
    /// Texto placeholder para inputs.
    pub placeholder: Option<String>,
    /// Nome da página alvo em `@goto="PageName"`.
    pub goto_page: Option<String>,
    /// Expressão opcional de dados para navegação em `@with="expr"`.
    pub goto_with: Option<String>,
}

/// ForLoop resolvido. Transparente visualmente.
#[derive(Debug)]
pub struct DenForLoop {
    pub each_var: String,
    pub iterable_expr: String,
    pub children: Vec<DenNode>,
    /// `@empty { ... }` — nós renderizados quando a iterável é vazia.
    pub empty_children: Vec<DenNode>,
}

/// IfChain resolvido. Transparente visualmente.
#[derive(Debug)]
pub struct DenIfChain {
    pub branches: Vec<DenIfBranch>,
    pub else_children: Vec<DenNode>,
}

#[derive(Debug)]
pub struct DenIfBranch {
    pub condition: String,
    pub children: Vec<DenNode>,
}
