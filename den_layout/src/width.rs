//! Regra `width` — resolve largura do elemento a partir da declaração SCSS
//! e do contexto de conteúdo do pai.
//!
//! Ponto único de mudança pra lógica de largura. `table.rs` chama `resolve`
//! quando processa cada filho dentro de block/flex.

use crate::DimensionRule;

/// Resolve a largura efetiva em CSS pixels.
///
/// - `Px(n)`: sempre `n`, ignora o pai.
/// - `Percent(p)`: `p * parent_content_width` (p é decimal, 0.5 = 50%).
/// - `Auto`: preenche todo o espaço de conteúdo disponível do pai.
pub(crate) fn resolve(rule: DimensionRule, parent_content_width: f32) -> f32 {
    match rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content_width * pct,
        DimensionRule::Auto => parent_content_width,
    }
}
