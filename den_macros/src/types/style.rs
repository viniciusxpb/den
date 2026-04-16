//! Tipos de estilo SCSS: regras, cores, bordas, display modes.
//!
//! # ⚠️ REGRA INEGOCIÁVEL — TODA PROPRIEDADE CSS É `Option<T>`
//!
//! **Toda** propriedade CSS sobreescrevível (color, display, position, flex_*,
//! cursor, etc.) DEVE ser `Option<T>` em `StyleRule` E em `DenVisual`. Nunca
//! `T` direto, nunca enum-com-default, nunca `bool`-direto.
//!
//! ## Por quê
//!
//! O CSS tem **cascade**: regra mais específica sobreescreve a menos específica.
//! Em Den, isso aparece em duas formas:
//! 1. **Múltiplas classes no mesmo elemento** — `class="a b"` faz merge `b` por cima de `a`.
//! 2. **`:hover` override** — `.btn:hover { ... }` sobreescreve `.btn`.
//!
//! Se a propriedade for `T` direto (ex.: `display: DisplayMode`), o `merge_from`
//! tem que comparar `other != Default::default()` pra decidir se sobreescreve. Isso
//! quebra silenciosamente quando o usuário quer **forçar o default explicitamente**:
//!
//! ```scss
//! .col            { display: flex;  flex-direction: column; }
//! .col:hover      { flex-direction: row; }   /* row é o default → IGNORADO se não for Option */
//! ```
//!
//! Com `Option<FlexDirection>`, o merge usa `is_some()` e o override sempre passa.
//!
//! ## Como aplicar
//!
//! - `StyleRule.foo: Option<Foo>` (parse-time, vem do parser SCSS)
//! - `DenVisual.foo: Option<Foo>` (resolve-time, depois de merge de classes)
//! - `merge_from`: `if other.foo.is_some() { self.foo = other.foo; }` — SEMPRE.
//! - **Nunca** comparar com default (`if other.foo != Default::default()`).
//! - O default só é aplicado **uma vez**, no **codegen**, ao emitir
//!   `LayoutIntent`/`PaintStyle` em runtime via `.unwrap_or_default()` ou
//!   `.unwrap_or(<concreto>)`.
//!
//! ## Como o reviewer pega
//!
//! Procura por:
//! - Campo novo de `StyleRule` ou `DenVisual` que não é `Option<T>`.
//! - `merge_from` com `if other.x != Default::default()` (ou comparando com
//!   variante específica de enum) — bug latente garantido.
//! - `if other.x { ... }` com `other.x: bool` em merge — não dá pra unsetar.

use std::collections::HashMap;

/// Um stop de gradient com cor e posição opcional.
///
/// `position: None` significa "posição automática" — distribuída igualmente
/// entre os stops sem posição explícita. MVP aceita stops sem position; quando
/// position explícita (`red 50%`) for implementada, stops mistos serão suportados.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GradientStop {
    pub color: RgbColor,
    /// `0.0..=1.0`. `None` = auto-distribuído pelo paint.
    pub position: Option<f32>,
}

/// Gradient linear CSS (`linear-gradient(<direction>, stop, stop, ...)`).
///
/// `angle_rad` segue a convenção CSS: `0rad` = "to top" (gradient de baixo pra
/// cima), `π/2 rad` (90deg) = "to right", `π rad` (180deg) = "to bottom", etc.
/// Aumenta no sentido horário.
#[derive(Debug, Clone, PartialEq)]
pub struct LinearGradient {
    pub angle_rad: f32,
    pub stops: Vec<GradientStop>,
}

/// Preenchimento de background: cor sólida ou gradient. `PaintStyle.background`
/// vira `Option<Background>` em vez de `Option<RgbColor>` direto — `background`
/// como shorthand CSS pode ser qualquer um dos dois.
///
/// Extensões futuras (mantendo compat via match): `RadialGradient`, múltiplos
/// backgrounds em camadas, `url(...)` para imagens.
#[derive(Debug, Clone, PartialEq)]
pub enum Background {
    Solid(RgbColor),
    LinearGradient(LinearGradient),
}

/// Cor RGBA em u8 (`(r, g, b, a)`). Alpha 255 = opaco total, 0 = transparente.
///
/// Parseres que não declaram alpha (`#RGB`, `#RRGGBB`, `rgb(...)`, `$var` hex sem
/// alpha) geram tuplas com `a = 255`. Alpha declarado explícito (`#RRGGBBAA`,
/// `rgba(...)`) preserva o canal.
///
/// A multiplicação com `opacity` acontece em paint-time, não aqui — essa tupla
/// guarda exatamente o que a folha CSS declarou.
pub type RgbColor = (u8, u8, u8, u8);

#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum DisplayMode {
    #[default]
    Block,
    Flex,
    Grid,
}

/// Eixo principal de um container flex. `Row` (default CSS) = horizontal;
/// `Column` = vertical. `row-reverse` / `column-reverse` ainda não suportados;
/// caem em `Row`/`Column` com warning.
///
/// **ESPELHO**: gêmeo de [`den_layout::FlexDirection`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum FlexDirection {
    #[default]
    Row,
    Column,
}

/// Alinhamento dos filhos no eixo CRUZADO de um flex container (vertical em
/// `flex-direction: row`, horizontal em `column`).
///
/// `Stretch` é o default CSS — filhos com tamanho cruzado `auto` esticam pra
/// preencher o cross. `Baseline` ainda não é suportado (depende de baseline da
/// fonte); cai em `FlexStart` com warning.
///
/// **ESPELHO**: gêmeo de [`den_layout::AlignItems`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum AlignItems {
    #[default]
    Stretch,
    FlexStart,
    Center,
    FlexEnd,
}

/// Comportamento quando o conteúdo excede o rect do container (`overflow`).
/// MVP suporta `Visible` (default CSS) e `Hidden`. `Scroll`/`Auto` ainda não
/// implementados — caem em `Visible` com warning no parser.
///
/// **ESPELHO**: gêmeo de [`den_layout::OverflowKind`]. Adicionar variante aqui
/// exige atualizar o do `den_layout` E o `overflow_tokens` em `codegen/style.rs`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum OverflowKind {
    #[default]
    Visible,
    Hidden,
}

/// Transformações 2D CSS (`transform: rotate(...) [scale(...) translate(...)]`).
///
/// MVP: só rotação. `scale`/`translate`/`matrix` ainda não parseados — cairão
/// aqui quando implementados, mesmo type, novos campos. Cada transform que o
/// dev declara é combinado numa `Transform2d` resolvida.
///
/// `rotation_rad: 0.0` = sem rotação (equivalente a `None` no `Option<Transform2d>`).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Transform2d {
    /// Rotação em radianos aplicada em volta do centro do rect do nó
    /// (CSS default `transform-origin: 50% 50%`).
    pub rotation_rad: f32,
}

impl Transform2d {
    /// `true` se nenhuma transformação foi efetivamente declarada (rotação = 0).
    /// Paint pode pular o codepath de mesh rotated quando é identity.
    pub fn is_identity(&self) -> bool {
        self.rotation_rad == 0.0
    }
}

/// Distribuição dos filhos no eixo PRINCIPAL de um flex container.
///
/// `FlexStart` é o default CSS. `space-*` distribuem o espaço remanescente
/// (depois dos children + gaps fixos) em diferentes lugares.
///
/// **ESPELHO**: gêmeo de [`den_layout::JustifyContent`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum JustifyContent {
    #[default]
    FlexStart,
    Center,
    FlexEnd,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

/// Borda CSS resolvida com largura por lado e cor única.
///
/// Layout `widths`: `[top, right, bottom, left]` — ordem CSS clockwise.
/// O parser preenche todos os 4 com o mesmo valor pra `border: shorthand` (caminho
/// uniforme) e atualiza só o slot afetado pra `border-<side>-width` /
/// `border-<side>: shorthand`.
///
/// Cor única por enquanto (MVP). Per-side color é incremento futuro: a struct
/// já está pronta, falta só `colors: [RgbColor; 4]`.
#[derive(Debug, Clone, Copy)]
pub struct BorderStyle {
    pub widths: [f32; 4],
    pub color: RgbColor,
}

impl BorderStyle {
    /// Cria com largura e cor uniformes nos 4 lados.
    pub fn uniform(width: f32, color: RgbColor) -> Self {
        Self {
            widths: [width; 4],
            color,
        }
    }
}

impl Default for BorderStyle {
    fn default() -> Self {
        Self {
            widths: [1.0; 4],
            color: (0, 0, 0, 255),
        }
    }
}

/// Uma sombra CSS (`box-shadow: <x> <y> <blur> [<spread>] <color> [inset]`).
///
/// CSS aceita lista vírgula-separada (`box-shadow: 0 2px red, inset 0 0 4px blue`),
/// representada como `Vec<BoxShadow>` no `StyleRule`. Pintura: ordem de stacking
/// = primeira sombra na frente, última no fundo (CSS spec).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BoxShadow {
    /// Offset horizontal em CSS px (positivo = direita).
    pub offset_x: f32,
    /// Offset vertical em CSS px (positivo = baixo).
    pub offset_y: f32,
    /// Raio do blur em CSS px. `0` = sombra nítida.
    pub blur: f32,
    /// Spread em CSS px — expande a sombra em todos os lados antes do blur.
    /// Negativo encolhe (útil pra sombras inset).
    pub spread: f32,
    /// Cor (com alpha já consumido — paint não multiplica `style.opacity` por cima).
    pub color: RgbColor,
    /// `true` = sombra interna (inset). `false` = drop shadow externo (default).
    pub inset: bool,
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
    /// Background shorthand: pode ser cor sólida OU gradient. Ver regra `Option<T>`
    /// no topo do arquivo — `Option` preserva cascade, `Background` discrimina fill.
    pub background: Option<Background>,
    pub padding: Option<f32>,
    pub margin: Option<f32>,
    /// `display`. Default CSS = `Block`. Ver regra `Option<T>` no topo do arquivo.
    pub display: Option<DisplayMode>,
    pub border: Option<BorderStyle>,
    pub border_radius: Option<f32>,
    /// `width`. Default CSS = `Auto`. Ver regra `Option<T>` no topo do arquivo.
    pub width: Option<WidthValue>,
    /// `height`. Default CSS = `Auto`. Ver regra `Option<T>` no topo do arquivo.
    pub height: Option<WidthValue>,
    pub min_width: Option<WidthValue>,
    pub max_width: Option<WidthValue>,
    pub min_height: Option<WidthValue>,
    pub max_height: Option<WidthValue>,
    pub gap: Option<f32>,
    /// `cursor: pointer`. `Some(true)` ou `None` (não declarado).
    /// Ver regra `Option<T>` no topo do arquivo — `bool` direto não permite `:hover`
    /// resetar pra `cursor: default`.
    pub cursor_pointer: Option<bool>,
    /// `flex: 1` / `flex-grow: 1` — `Some(true)` cresce, `None` = não declarado.
    /// Ver regra `Option<T>` no topo do arquivo.
    pub flex_grow: Option<bool>,
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
    /// `opacity: 0..1` — multiplicador aplicado ao alpha de TODAS as cores
    /// (color, background, border) no paint. `None` = não declarado (= 1.0 opaco).
    /// Compõe com o alpha da própria cor: se `background: #80808080` (alpha 128)
    /// e `opacity: 0.5`, o alpha final = 128 * 0.5 = 64.
    pub opacity: Option<f32>,
    /// `white-space: nowrap` declarado. `None` = não declarado, `Some(true)` = nowrap,
    /// `Some(false)` = `normal` (default; relevante quando wrap for implementado).
    pub white_space_nowrap: Option<bool>,
    /// `text-overflow: ellipsis` declarado.
    pub text_overflow_ellipsis: Option<bool>,
    /// Lista de `box-shadow`. `None` = não declarado; `Some(vec![])` = `box-shadow: none`
    /// explícito (cancela sombra herdada). Sintaxe CSS: vírgula-separado,
    /// primeira sombra fica na frente do stack visual.
    /// Ver regra `Option<T>` no topo do arquivo — `Vec` direto não distingue
    /// "não declarado" de "explicitamente vazio", quebrando cascade pra `none`.
    pub box_shadows: Option<Vec<BoxShadow>>,
    /// `flex-direction`. Default CSS = `Row`. Ver regra `Option<T>` no topo do arquivo.
    pub flex_direction: Option<FlexDirection>,
    /// `align-items` — alinhamento cruzado dos filhos. Default CSS = `Stretch`.
    /// Ver regra `Option<T>` no topo do arquivo.
    pub align_items: Option<AlignItems>,
    /// `justify-content` — distribuição no eixo principal. Default CSS = `FlexStart`.
    /// Ver regra `Option<T>` no topo do arquivo.
    pub justify_content: Option<JustifyContent>,
    /// `overflow`: o que acontece quando o conteúdo excede o rect do container.
    /// `None` = não declarado (= Visible default CSS). Ver regra `Option<T>`.
    pub overflow: Option<OverflowKind>,
    /// `transform: rotate(...)`. `None` = não declarado (= identity, sem rotação).
    /// Ver regra `Option<T>` no topo do arquivo.
    pub transform: Option<Transform2d>,
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
