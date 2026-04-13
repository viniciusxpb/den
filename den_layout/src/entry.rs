//! Entradas de layout usadas pela tabela flat do Den.

use crate::{DimensionRule, DisplayMode};

/// Uma entrada na flat list de layout.
///
/// Cada elemento HTML parseado gera uma `LayoutEntry`.
#[derive(Debug, Clone)]
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
    /// Display mode — determina como distribui espaço pros filhos.
    pub display: DisplayMode,
    /// Padding uniforme em CSS pixels.
    pub padding: f32,
    /// Margin uniforme em CSS pixels.
    pub margin: f32,
    /// Gap entre filhos diretos em CSS pixels.
    pub gap: f32,
    /// Peso de flex-grow. 0 = não cresce.
    pub flex_grow: f32,
    /// Largura mínima estimada do conteúdo próprio em CSS pixels.
    pub intrinsic_width: f32,
    /// Altura mínima estimada do conteúdo próprio em CSS pixels.
    pub intrinsic_height: f32,
}
