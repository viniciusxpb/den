//! Regra `width` — resolve a largura final (border box) de uma `LayoutEntry`.
//!
//! Ponto único de mudança pra tudo que afeta largura no box model CSS:
//! - `width`, `min-width`, `max-width`
//! - `padding` (left + right)
//! - `border-width` (left + right)
//! - `intrinsic_width` (conteúdo próprio, texto)
//!
//! Margin **não** entra aqui.
//!
//! **Box model**: `rect.width = content + padding*2 + border*2`, sempre.

use crate::{DimensionRule, LayoutEntry};

/// Soma horizontal ocupada por padding + border (esquerda + direita).
fn edge_extent(entry: &LayoutEntry) -> f32 {
    (entry.padding + entry.border_width) * 2.0
}

/// Resolve a largura efetiva (border box) em CSS pixels.
///
/// - `Px(n)`: exatamente `n`.
/// - `Percent(p)`: `p * parent_content_width`.
/// - `Auto`: preenche todo o espaço de conteúdo disponível do pai (ou usa
///   intrínseco no contexto flex via `auto_leaf`).
///
/// Aplica clamp de `min-width` / `max-width` no resultado.
pub(crate) fn resolve(entry: &LayoutEntry, parent_content_width: f32) -> f32 {
    let base = match entry.width_rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content_width * pct,
        DimensionRule::Auto => parent_content_width,
    };
    clamp(base, entry, parent_content_width)
}

/// Largura `Auto` baseada no conteúdo próprio (usado em flex, onde children
/// Auto sem flex-grow empacotam no tamanho do conteúdo mais bordas).
pub(crate) fn resolve_auto_leaf(entry: &LayoutEntry) -> f32 {
    entry.intrinsic_width + edge_extent(entry)
}

/// Aplica os limites `min-width` / `max-width` (ambos em border-box).
fn clamp(value: f32, entry: &LayoutEntry, parent_content_width: f32) -> f32 {
    let mut v = value;
    if let Some(min) = entry.min_width {
        v = v.max(dimension_value(min, parent_content_width));
    }
    if let Some(max) = entry.max_width {
        v = v.min(dimension_value(max, parent_content_width));
    }
    v
}

fn dimension_value(rule: DimensionRule, parent_content: f32) -> f32 {
    match rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content * pct,
        DimensionRule::Auto => 0.0,
    }
}
