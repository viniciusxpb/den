//! Modos de display + propriedades de flex usadas pelo motor de layout Den.

/// Display mode do elemento.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum DisplayMode {
    /// Layout vertical em bloco.
    #[default]
    Block,
    /// Layout flex (eixo principal definido por [`FlexDirection`]).
    Flex,
    /// Reservado para o futuro motor de grid.
    Grid,
}

/// Eixo principal de um container flex. `Row` = horizontal (default CSS),
/// `Column` = vertical.
///
/// **ESPELHO** de `den_macros::types::FlexDirection`. Variantes adicionadas
/// aqui exigem atualização lá + no `flex_direction_tokens` do codegen.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum FlexDirection {
    #[default]
    Row,
    Column,
}

impl FlexDirection {
    /// Retorna `true` se o eixo principal é horizontal (Row).
    pub fn is_row(self) -> bool {
        matches!(self, FlexDirection::Row)
    }
}

/// Alinhamento dos filhos no eixo CRUZADO de um flex container.
///
/// **ESPELHO** de `den_macros::types::AlignItems`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum AlignItems {
    #[default]
    Stretch,
    FlexStart,
    Center,
    FlexEnd,
}

/// Distribuição dos filhos no eixo PRINCIPAL.
///
/// **ESPELHO** de `den_macros::types::JustifyContent`.
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
