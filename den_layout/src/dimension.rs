//! Enum compartilhada entre `width` e `height` — como uma dimensão foi declarada no SCSS.
//!
//! Os resolvers específicos vivem em `width.rs` e `height.rs`.

/// Como uma dimensão foi declarada no SCSS.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum DimensionRule {
    /// Sem width/height no SCSS.
    ///
    /// - Com filhos: segue o contexto do pai.
    /// - Sem filhos: encaixa no pai para largura, altura natural para altura.
    #[default]
    Auto,
    /// Valor fixo em pixels: `width: 200px` ou `height: 200px`.
    Px(f32),
    /// Percentagem do pai: `width: 50%` ou `height: 50%`.
    Percent(f32),
}

/// Esquema de posicionamento CSS `position: ...`.
///
/// - `Static` (default): flow normal, offsets ignorados.
/// - `Relative`: flow normal, mas estabelece containing block pros filhos absolute.
/// - `Absolute`: fora de flow, posicionado contra o nearest positioned ancestor.
/// - `Fixed`: fora de flow, posicionado contra o viewport (body).
///
/// **ESPELHO**: este enum é gêmeo de `den_macros::types::PositionKind`. Adicionar
/// variante aqui exige atualizar o lado macro E o `position_tokens` que faz a
/// tradução. Ver doc do tipo no macro pra contexto sobre a duplicação.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PositionKind {
    #[default]
    Static,
    Relative,
    Absolute,
    Fixed,
}

impl PositionKind {
    pub fn is_positioned(self) -> bool {
        !matches!(self, PositionKind::Static)
    }

    pub fn is_out_of_flow(self) -> bool {
        matches!(self, PositionKind::Absolute | PositionKind::Fixed)
    }
}
