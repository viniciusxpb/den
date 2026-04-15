//! Pass de layout flex: filhos distribuídos horizontalmente.
//!
//! Auto-children sem `flex-grow` empacotam no tamanho intrínseco; com `flex-grow`
//! dividem o espaço remanescente proporcionalmente. Filhos `position: absolute|fixed`
//! ficam de fora desta pass — vão pra [`super::positioned`].
//!
//! Note: importamos `crate::flex` (helpers de cálculo) com path absoluto pra
//! evitar ambiguidade com o nome desta submódulo.

use super::{LayoutTable, content_axis};
use crate::{BODY_INDEX, DimensionRule, LayoutRect, height, margin, width};

impl LayoutTable {
    /// Resolve filhos em fluxo horizontal flex.
    pub(super) fn layout_flex_children(&mut self, parent_idx: usize) {
        let parent_rect = self.rects[parent_idx];
        let padding = self.entries[parent_idx].padding;
        let border = self.entries[parent_idx].border_width;
        let edge = padding + border;
        let gap = self.entries[parent_idx].gap;
        let content_x = parent_rect.x + edge;
        let content_width = content_axis(parent_rect.width, edge);
        let parent_content_height = height::parent_content_height_for(
            self.entries[parent_idx].height_rule,
            parent_idx == BODY_INDEX,
            parent_rect.height,
            padding,
            border,
        );
        let all_children = self.entries[parent_idx].children.clone();
        let in_flow: Vec<usize> = all_children
            .iter()
            .copied()
            .filter(|&c| !self.entries[c].position.is_out_of_flow())
            .collect();
        if in_flow.is_empty() {
            return;
        }

        let gap_total = crate::flex::gap_total(gap, in_flow.len());
        let margin_total: f32 = in_flow
            .iter()
            .map(|&child_idx| margin::uniform_extent(self.entries[child_idx].margin))
            .sum();
        let mut fixed_total = 0.0;
        let mut grow_total = 0.0;

        for &child_idx in &in_flow {
            let grow = self.entries[child_idx].flex_grow;
            let child = &self.entries[child_idx];
            if grow > 0.0 && child.width_rule == DimensionRule::Auto {
                grow_total += grow;
            } else if child.width_rule == DimensionRule::Auto {
                // Auto sem flex-grow empacota no tamanho da border-box: content + padding + border.
                fixed_total += width::resolve_auto_leaf(child);
            } else {
                fixed_total += width::resolve(child, content_width);
            }
        }

        let remaining = (content_width - fixed_total - margin_total - gap_total).max(0.0);
        let mut cursor_x = content_x;
        let content_y = parent_rect.y + edge;
        let mut max_height = 0.0f32;

        for (pos, child_idx) in in_flow.iter().copied().enumerate() {
            let margin = self.entries[child_idx].margin;
            let grow = self.entries[child_idx].flex_grow;
            let child = &self.entries[child_idx];
            let fixed_width = if child.width_rule == DimensionRule::Auto && grow == 0.0 {
                width::resolve_auto_leaf(child)
            } else {
                width::resolve(child, content_width)
            };
            let resolved_width = crate::flex::distribute_flex_width(
                child.width_rule,
                grow,
                fixed_width,
                remaining,
                grow_total,
            );
            let resolved_height = height::resolve(child, parent_content_height);
            self.sizes[child_idx] = Some(resolved_width);
            self.rects[child_idx] = LayoutRect {
                x: cursor_x + margin,
                y: content_y + margin,
                width: resolved_width,
                height: resolved_height,
            };
            self.layout_children(child_idx);
            max_height =
                max_height.max(self.rects[child_idx].height + margin::uniform_extent(margin));
            cursor_x += margin::uniform_extent(margin) + resolved_width;
            if pos + 1 < in_flow.len() {
                cursor_x += gap;
            }
        }

        if self.entries[parent_idx].height_rule == DimensionRule::Auto && parent_idx != BODY_INDEX {
            // Altura natural = max child height + padding*2 + border*2.
            self.rects[parent_idx].height = max_height + edge * 2.0;
        }
    }
}
