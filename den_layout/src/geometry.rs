//! Tipos geométricos calculados pelo motor de layout Den.

/// Caixa calculada para um elemento.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct LayoutRect {
    /// Posição horizontal em CSS pixels.
    pub x: f32,
    /// Posição vertical em CSS pixels.
    pub y: f32,
    /// Largura em CSS pixels.
    pub width: f32,
    /// Altura em CSS pixels.
    pub height: f32,
}
