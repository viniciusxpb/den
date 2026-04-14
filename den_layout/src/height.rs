//! Regra `height` — resolve altura do elemento a partir da declaração SCSS
//! e do contexto de altura do pai.
//!
//! Ponto único de mudança pra lógica de altura. Inclui o cálculo de "altura
//! de conteúdo disponível do pai" pra filhos com `height: N%`.

use crate::{DimensionRule, spacing};

/// Resolve a altura efetiva em CSS pixels.
///
/// - `Px(n)`: sempre `n`, ignora o pai.
/// - `Percent(p)`: `p * parent_content_height` (0 se o pai não tem altura fixa).
/// - `Auto`: retorna 0 — a altura real é preenchida bottom-up depois que
///   os filhos desse elemento forem posicionados.
pub(crate) fn resolve(rule: DimensionRule, parent_content_height: f32) -> f32 {
    match rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content_height * pct,
        DimensionRule::Auto => 0.0,
    }
}

/// Altura de conteúdo do pai disponível pros filhos (pra resolver `height: N%`).
///
/// Contrato:
/// - **Body** (raiz, sempre `is_body = true`): viewport inteiro menos padding.
/// - **Parent `height: Px`/`Percent`**: `parent_rect.height` já foi resolvida
///   pelo avô quando este pai foi posicionado → retorna essa altura menos padding.
/// - **Parent `height: Auto`**: retorna 0. Filhos com `height: %` caem pra 0
///   (CSS compat: `%` contra pai auto resolve pra auto/0 sem multi-pass layout).
pub(crate) fn parent_content_height_for(
    parent_rule: DimensionRule,
    is_body: bool,
    parent_rect_height: f32,
    parent_padding: f32,
) -> f32 {
    let available = (parent_rect_height - spacing::uniform_padding_extent(parent_padding)).max(0.0);
    if is_body {
        return available;
    }
    match parent_rule {
        DimensionRule::Px(_) | DimensionRule::Percent(_) => available,
        DimensionRule::Auto => 0.0,
    }
}
