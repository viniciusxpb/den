//! Funções auxiliares para distribuição flex no motor Den.

use crate::DimensionRule;

/// Calcula o espaço total ocupado por gaps entre filhos flex.
pub(crate) fn gap_total(gap: f32, child_count: usize) -> f32 {
    gap * child_count.saturating_sub(1) as f32
}

/// Distribui a largura final de um filho em contexto flex.
pub(crate) fn distribute_flex_width(
    width_rule: DimensionRule,
    flex_grow: f32,
    fixed_width: f32,
    remaining_width: f32,
    grow_total: f32,
) -> f32 {
    if flex_grow > 0.0 && width_rule == DimensionRule::Auto {
        if grow_total > 0.0 {
            remaining_width * (flex_grow / grow_total)
        } else {
            0.0
        }
    } else {
        fixed_width
    }
}
