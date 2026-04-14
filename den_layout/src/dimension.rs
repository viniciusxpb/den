//! Enum compartilhada entre `width` e `height` — como uma dimensão foi declarada no SCSS.
//!
//! Os resolvers específicos vivem em `width.rs` e `height.rs`.

/// Como uma dimensão foi declarada no SCSS.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DimensionRule {
    /// Sem width/height no SCSS.
    ///
    /// - Com filhos: segue o contexto do pai.
    /// - Sem filhos: encaixa no pai para largura, altura natural para altura.
    Auto,
    /// Valor fixo em pixels: `width: 200px` ou `height: 200px`.
    Px(f32),
    /// Percentagem do pai: `width: 50%` ou `height: 50%`.
    Percent(f32),
}
