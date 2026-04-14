//! Modos de display usados pelo motor de layout Den.

/// Display mode do elemento.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum DisplayMode {
    /// Layout vertical em bloco.
    #[default]
    Block,
    /// Layout horizontal com distribuição flex.
    Flex,
    /// Reservado para o futuro motor de grid.
    Grid,
}
