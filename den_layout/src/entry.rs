//! Entradas de layout usadas pela tabela flat do Den.
//!
//! Cada `<tag>` do HTML vira uma `LayoutEntry`. Ela guarda as propriedades
//! resolvidas do box model CSS (width/height/padding/border/margin/gap) pra
//! que as funções de regra (`width::resolve`, `height::resolve`, etc.)
//! saibam tudo que precisam só olhando pra entry e pro pai.

use crate::{DimensionRule, DisplayMode, PositionKind};

/// Uma entrada na flat list de layout.
#[derive(Debug, Clone, Default)]
pub struct LayoutEntry {
    /// Índice do pai na lista (`None` só pro body).
    /// O índice deste entry é sua posição no Vec — não precisa de campo separado.
    pub parent: Option<usize>,
    /// Índices dos filhos diretos.
    pub children: Vec<usize>,
    /// Regra de largura declarada no SCSS.
    pub width_rule: DimensionRule,
    /// Regra de altura declarada no SCSS.
    pub height_rule: DimensionRule,
    /// `min-width` declarado no SCSS (None = 0).
    pub min_width: Option<DimensionRule>,
    /// `max-width` declarado no SCSS (None = infinito).
    pub max_width: Option<DimensionRule>,
    /// `min-height` declarado no SCSS (None = 0).
    pub min_height: Option<DimensionRule>,
    /// `max-height` declarado no SCSS (None = infinito).
    pub max_height: Option<DimensionRule>,
    /// Display mode — determina como distribui espaço pros filhos.
    pub display: DisplayMode,
    /// Padding uniforme em CSS pixels (entre conteúdo e border).
    pub padding: f32,
    /// Larguras de borda por lado em CSS pixels: `[top, right, bottom, left]`.
    /// `[0; 4]` = sem borda. Cada slot independente — `border-left-width: 0`
    /// zera só o slot 3 sem afetar os outros.
    pub border_widths: [f32; 4],
    /// Margin uniforme em CSS pixels (fora da border).
    pub margin: f32,
    /// Gap entre filhos diretos em CSS pixels.
    pub gap: f32,
    /// Peso de flex-grow. 0 = não cresce.
    pub flex_grow: f32,
    /// Largura do CONTEÚDO próprio (texto/input) em CSS pixels. Sem padding, sem border.
    /// Preenchido em runtime pelo painter via medição real da fonte.
    pub intrinsic_width: f32,
    /// Altura do CONTEÚDO próprio em CSS pixels. Sem padding, sem border.
    pub intrinsic_height: f32,
    /// Esquema de posicionamento CSS — afeta se o elemento entra no flow normal.
    pub position: PositionKind,
    /// Offset vertical do topo do containing block. `None` = não declarado (deixa
    /// o engine decidir, normalmente ancorando pelo `bottom` se setado).
    pub top: Option<DimensionRule>,
    /// Offset horizontal da borda esquerda do containing block. `None` = não declarado.
    pub left: Option<DimensionRule>,
    /// Offset horizontal da borda direita do containing block. `None` = não declarado.
    pub right: Option<DimensionRule>,
    /// Offset vertical do bottom do containing block. `None` = não declarado.
    pub bottom: Option<DimensionRule>,
    /// Ordem de paint entre positioned siblings. `None` = 0 (default CSS `auto`).
    pub z_index: Option<i32>,
}

impl LayoutEntry {
    /// Borda do topo (`border-top-width`).
    pub fn border_top(&self) -> f32 {
        self.border_widths[0]
    }
    /// Borda da direita.
    pub fn border_right(&self) -> f32 {
        self.border_widths[1]
    }
    /// Borda de baixo.
    pub fn border_bottom(&self) -> f32 {
        self.border_widths[2]
    }
    /// Borda da esquerda.
    pub fn border_left(&self) -> f32 {
        self.border_widths[3]
    }
    /// Soma das bordas horizontais (left + right) — overhead horizontal do box model.
    pub fn border_x_extent(&self) -> f32 {
        self.border_widths[3] + self.border_widths[1]
    }
    /// Soma das bordas verticais (top + bottom) — overhead vertical.
    pub fn border_y_extent(&self) -> f32 {
        self.border_widths[0] + self.border_widths[2]
    }
}
