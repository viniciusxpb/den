//! Pass de layout block: filhos empilhados verticalmente.
//!
//! Margens uniformes são sempre reservadas; o motor ainda não implementa
//! colapso de margin entre blocos como browsers fazem. Filhos com
//! `position: absolute|fixed` são excluídos do flow normal e tratados
//! depois pela [`super::positioned`] pass.

use super::{LayoutTable, content_axis};
use crate::{BODY_INDEX, DimensionRule, LayoutRect, height, margin, width};

impl LayoutTable {
    /// Resolve filhos em fluxo vertical de bloco.
    pub(super) fn layout_block_children(&mut self, parent_idx: usize) {
        let parent_rect = self.rects[parent_idx];
        let padding = self.entries[parent_idx].padding;
        let border = self.entries[parent_idx].border_width;
        let gap = self.entries[parent_idx].gap;
        // Conteúdo começa DEPOIS do padding + border (por lado).
        let edge = padding + border;
        let content_x = parent_rect.x + edge;
        let content_width = content_axis(parent_rect.width, edge);
        let parent_content_height = height::parent_content_height_for(
            self.entries[parent_idx].height_rule,
            parent_idx == BODY_INDEX,
            parent_rect.height,
            padding,
            border,
        );
        let mut cursor_y = parent_rect.y + edge;
        let all_children = self.entries[parent_idx].children.clone();
        // Filtra positioned do flow normal — eles são tratados em layout_positioned.
        let in_flow: Vec<usize> = all_children
            .iter()
            .copied()
            .filter(|&c| !self.entries[c].position.is_out_of_flow())
            .collect();

        if in_flow.is_empty() {
            if self.entries[parent_idx].height_rule == DimensionRule::Auto
                && parent_idx != BODY_INDEX
            {
                self.rects[parent_idx].height = self.rects[parent_idx]
                    .height
                    .max(height::resolve_auto_leaf(&self.entries[parent_idx]));
            }
            return;
        }

        for (pos, child_idx) in in_flow.iter().copied().enumerate() {
            let margin = self.entries[child_idx].margin;
            let child_width_context = margin::child_content_width(content_width, margin);
            let resolved_width = width::resolve(&self.entries[child_idx], child_width_context);
            let resolved_height = height::resolve(&self.entries[child_idx], parent_content_height);
            self.sizes[child_idx] = Some(resolved_width);
            self.rects[child_idx] = LayoutRect {
                x: content_x + margin,
                y: cursor_y + margin,
                width: resolved_width,
                height: resolved_height,
            };
            self.layout_children(child_idx);
            cursor_y += margin::uniform_extent(margin) + self.rects[child_idx].height;
            if pos + 1 < in_flow.len() {
                cursor_y += gap;
            }
        }

        if self.entries[parent_idx].height_rule == DimensionRule::Auto {
            // Altura natural do container = children + padding*2 + border*2.
            let content_height = cursor_y - parent_rect.y + edge;
            if parent_idx == BODY_INDEX {
                // Body cresce com o conteúdo mas nunca encolhe abaixo do viewport.
                self.rects[parent_idx].height = parent_rect.height.max(content_height);
            } else {
                self.rects[parent_idx].height = content_height;
            }
        }
    }
}
