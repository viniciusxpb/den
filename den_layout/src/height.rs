//! Regra `height` — resolve a altura final (border box) de uma `LayoutEntry`.
//!
//! Ponto único de mudança pra tudo que afeta altura no box model CSS:
//! - `height`, `min-height`, `max-height`
//! - `padding` (top + bottom)
//! - `border-width` (top + bottom)
//! - `intrinsic_height` (conteúdo próprio, texto)
//!
//! Margin **não** entra aqui: é espaço FORA da border box, tratado como
//! posicionamento no `table.rs`.
//!
//! **Box model**: `rect.height = content + padding*2 + border*2`, sempre.
//! `intrinsic_height` é conteúdo puro; esta função adiciona as bordas.

use crate::{DimensionRule, LayoutEntry};

/// Soma vertical ocupada por padding + border (topo + base).
/// É o overhead que a altura da border-box tem em cima do conteúdo.
fn edge_extent(entry: &LayoutEntry) -> f32 {
    (entry.padding + entry.border_width) * 2.0
}

/// Resolve a altura efetiva (border box) em CSS pixels.
///
/// - `Px(n)`: exatamente `n` — valor já inclui padding+border (modelo border-box).
/// - `Percent(p)`: `p * parent_content_height`.
/// - `Auto`: conteúdo + padding*2 + border*2 (pra nó folha).
///   Containers com filhos SOBRESCREVEM esse valor depois que os filhos
///   forem posicionados (ver `table.rs::layout_block_children` final).
///
/// Aplica clamp de `min-height` / `max-height` no resultado.
pub(crate) fn resolve(entry: &LayoutEntry, parent_content_height: f32) -> f32 {
    let base = match entry.height_rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content_height * pct,
        DimensionRule::Auto => resolve_auto_leaf(entry),
    };
    clamp(base, entry, parent_content_height)
}

/// Altura `Auto` pra nó folha (sem filhos): box model sobre o conteúdo intrínseco.
/// Usado também quando algum bloco tem filhos mas queremos saber o "mínimo natural".
pub(crate) fn resolve_auto_leaf(entry: &LayoutEntry) -> f32 {
    entry.intrinsic_height + edge_extent(entry)
}

/// Altura de CONTEÚDO do pai disponível pros filhos (pra resolver `height: N%`).
/// Equivale à rect.height do pai menos padding + border (duas bordas).
pub(crate) fn parent_content_height_for(
    parent_rule: DimensionRule,
    is_body: bool,
    parent_rect_height: f32,
    parent_padding: f32,
    parent_border_width: f32,
) -> f32 {
    let overhead = (parent_padding + parent_border_width) * 2.0;
    let available = (parent_rect_height - overhead).max(0.0);
    if is_body {
        return available;
    }
    match parent_rule {
        DimensionRule::Px(_) | DimensionRule::Percent(_) => available,
        DimensionRule::Auto => 0.0,
    }
}

/// Aplica os limites `min-height` / `max-height` (ambos em border-box).
fn clamp(value: f32, entry: &LayoutEntry, parent_content_height: f32) -> f32 {
    let mut v = value;
    if let Some(min) = entry.min_height {
        v = v.max(dimension_value(min, parent_content_height));
    }
    if let Some(max) = entry.max_height {
        v = v.min(dimension_value(max, parent_content_height));
    }
    v
}

/// Resolve uma `DimensionRule` como valor concreto pra comparação em clamp.
/// `Auto` vira `0` (min) ou `f32::INFINITY` (max) — caller decide via `saturating`.
fn dimension_value(rule: DimensionRule, parent_content: f32) -> f32 {
    match rule {
        DimensionRule::Px(px) => px,
        DimensionRule::Percent(pct) => parent_content * pct,
        DimensionRule::Auto => 0.0,
    }
}
