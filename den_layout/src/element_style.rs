//! Estruturas de estilo expostas para handlers e runtime Den.

/// Propriedades visuais de um elemento Den, extraídas em compile time.
///
/// Passado como argumento quando o handler usa a keyword `style`.
#[derive(Debug, Clone, Default)]
pub struct DenElementStyle {
    /// Cor do texto em RGB.
    pub color: Option<(u8, u8, u8)>,
    /// Cor de fundo em RGB.
    pub background: Option<(u8, u8, u8)>,
    /// Tamanho de fonte em CSS pixels.
    pub font_size: Option<f32>,
    /// Padding uniforme em CSS pixels.
    pub padding: Option<f32>,
    /// Margin uniforme em CSS pixels.
    pub margin: Option<f32>,
    /// Raio de borda em CSS pixels.
    pub border_radius: Option<f32>,
    /// Largura da borda em CSS pixels.
    pub border_width: Option<f32>,
    /// Cor da borda em RGB.
    pub border_color: Option<(u8, u8, u8)>,
    /// Largura fixa em CSS pixels.
    pub width_px: Option<f32>,
    /// Largura percentual já normalizada entre 0 e 1.
    pub width_percent: Option<f32>,
    /// Indica se o elemento usa display flex.
    pub is_flex: bool,
}
