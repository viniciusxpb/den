//! Modos de display usados pelo motor de layout Den.

/// Display mode do elemento.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DisplayMode {
    /// Layout vertical em bloco.
    Block,
    /// Layout horizontal com distribuição flex.
    Flex,
    /// Reservado para o futuro motor de grid.
    Grid,
}
