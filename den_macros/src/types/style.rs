//! Tipos de estilo SCSS: regras, cores, bordas, display modes.

use std::collections::HashMap;

pub type RgbColor = (u8, u8, u8);

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum DisplayMode {
    #[default]
    Block,
    Flex,
    Grid,
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

/// Esquema de posicionamento CSS `position: ...`.
///
/// - `Static` (default): flow normal, ignora `top/left/right/bottom`.
/// - `Relative`: flow normal, mas vira containing block pros filhos absolute.
/// - `Absolute`: fora de flow, posicionado contra o nearest positioned ancestor (ou body).
/// - `Fixed`: fora de flow, posicionado contra o viewport (body root).
///
/// `sticky` ainda não é suportado; é aceito pelo parser mas cai em `Static` com warning.
///
/// **ESPELHO**: este enum é gêmeo de [`den_layout::PositionKind`]. Adicionar/remover
/// variante aqui exige atualizar o do `den_layout` E o `position_tokens` em
/// `codegen/style.rs` que faz a tradução. Ao contrário, usar uma variante do
/// macro que não existe no runtime compila mas quebra silenciosamente. Os crates
/// não compartilham types porque `den_macros` é proc-macro (não pode depender
/// de um crate runtime) — extrair `den_common` resolveria, mas é trabalho separado
/// (ver PENDING.md "Extração pra `den_core`").
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum PositionKind {
    #[default]
    Static,
    Relative,
    Absolute,
    Fixed,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LineHeightValue {
    Px(f32),
    Factor(f32),
}

/// SYNC: espelho de `den_layout::TextTransform`; manter variantes sincronizadas.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum TextTransform {
    #[default]
    None,
    Uppercase,
    Lowercase,
    Capitalize,
}

/// SYNC: espelho de `den_layout::TextAlign`; manter variantes sincronizadas.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum TextAlign {
    #[default]
    Left,
    Center,
    Right,
}

/// Regra de estilo bruta, output do SCSS parser.
#[derive(Debug, Clone, Default)]
pub struct StyleRule {
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
    pub background: Option<RgbColor>,
    pub padding: Option<f32>,
    pub margin: Option<f32>,
    pub display: DisplayMode,
    pub border: Option<BorderStyle>,
    pub border_radius: Option<f32>,
    pub width: WidthValue,
    pub height: WidthValue,
    pub min_width: Option<WidthValue>,
    pub max_width: Option<WidthValue>,
    pub min_height: Option<WidthValue>,
    pub max_height: Option<WidthValue>,
    pub gap: Option<f32>,
    pub cursor_pointer: bool,
    /// `flex: 1` / `flex-grow: 1` — elemento cresce pra preencher o share do flex pai.
    pub flex_grow: bool,
    /// Esquema de posicionamento. `None` = não declarado em nenhuma regra que
    /// se aplica; o codegen colapsa pra `Static` na hora de emitir o `LayoutIntent`.
    /// Manter como `Option` permite que `position: static` explícito num seletor
    /// mais específico cancele um `position: absolute` herdado de classe anterior
    /// — coisa que `PositionKind::Static` direto não distingue do default.
    pub position: Option<PositionKind>,
    /// Offset do top do containing block (só aplicado se positioned).
    pub top: Option<WidthValue>,
    /// Offset do left do containing block.
    pub left: Option<WidthValue>,
    /// Offset do right do containing block.
    pub right: Option<WidthValue>,
    /// Offset do bottom do containing block.
    pub bottom: Option<WidthValue>,
    /// `z-index` — ordenação de pintura entre elementos positioned do mesmo pai.
    /// Só tem efeito em positioned; ignorado em static.
    pub z_index: Option<i32>,
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
            self.background = other.background;
        }
        if other.padding.is_some() {
            self.padding = other.padding;
        }
        if other.margin.is_some() {
            self.margin = other.margin;
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
        if other.cursor_pointer {
            self.cursor_pointer = true;
        }
        if other.flex_grow {
            self.flex_grow = true;
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
        if other.hover.is_some() {
            self.hover = other.hover.clone();
        }
    }

    /// Extrai só propriedades CSS herdáveis de texto pra propagar pros filhos.
    /// Hover NÃO é herdável.
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

/// Mapa de classe CSS → StyleRule.
pub type StyleMap = HashMap<String, StyleRule>;
