//! Regras de dimensão usadas pelo motor de layout Den.

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

/// Resolve uma largura a partir da regra e do conteúdo disponível no pai.
pub(crate) fn resolve_width(rule: DimensionRule, parent_content_width: f32) -> f32 {
    match rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content_width * pct,
        DimensionRule::Auto => parent_content_width,
    }
}

/// Resolve uma altura a partir da regra e do conteúdo disponível no pai.
pub(crate) fn resolve_height(rule: DimensionRule, parent_content_height: f32) -> f32 {
    match rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content_height * pct,
        DimensionRule::Auto => 0.0,
    }
}
