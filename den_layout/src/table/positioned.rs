//! Pass de layout pra elementos `position: absolute|fixed`.
//!
//! Roda DEPOIS do flow normal (block/flex). Cada positioned tem seu próprio
//! containing block, derivado do nearest positioned ancestor (ou do body
//! pra `fixed`). Não toma espaço dos siblings in-flow nem afeta a altura
//! auto do pai.

use super::{LayoutTable, resolve_offset};
use crate::{BODY_INDEX, DimensionRule, LayoutRect, PositionKind, height, width};

impl LayoutTable {
    /// Encontra o containing block (CB) pra um elemento `position: absolute|fixed`.
    ///
    /// - `Fixed` → body (index 0), sempre.
    /// - `Absolute` → walk up pro primeiro ancestor com `position != Static`. Se
    ///   ninguém for positioned no caminho, cai no body (que é Relative por padrão —
    ///   ver `RenderTree::to_layout_entries`).
    pub(super) fn containing_block_index(&self, idx: usize) -> usize {
        if matches!(self.entries[idx].position, PositionKind::Fixed) {
            return BODY_INDEX;
        }
        let mut cursor = self.entries[idx].parent;
        while let Some(p) = cursor {
            if self.entries[p].position.is_positioned() {
                return p;
            }
            cursor = self.entries[p].parent;
        }
        BODY_INDEX
    }

    /// Layout de um elemento out-of-flow (absolute/fixed) contra seu containing block.
    ///
    /// Seguindo a spec CSS: CB pra absolute = padding box do ancestor positioned; pra
    /// fixed = viewport (body). Offsets `top/left/right/bottom` são resolvidos contra
    /// as dimensões do CB; percent offsets usam width (left/right) ou height (top/bottom)
    /// do CB.
    ///
    /// Regras de width/height com offsets:
    /// - `left` + `right` ambos setados e `width: auto` → stretch entre as duas bordas.
    /// - Caso contrário: usa `width_rule` normal e ancora pela borda fornecida.
    /// - `right` sem `left` ancora pela direita: `x = cb_right - right - width`.
    ///
    /// **TODO (MVP)**: `margin` é ignorado em positioned elements. Spec CSS aplica
    /// margin entre o offset e a borda do elemento (`left: 10` + `margin-left: 5`
    /// → content x = 15). Pra adicionar: somar margin no `x`/`y` finais e subtrair
    /// margin total do width quando em modo stretch.
    pub(super) fn layout_positioned(&mut self, idx: usize) {
        let cb_idx = self.containing_block_index(idx);
        let cb_rect = self.rects[cb_idx];
        let cb_border = self.entries[cb_idx].border_width;
        // Padding box do CB (spec CSS): rect menos border (padding fica DENTRO).
        let cb_x = cb_rect.x + cb_border;
        let cb_y = cb_rect.y + cb_border;
        let cb_w = (cb_rect.width - cb_border * 2.0).max(0.0);
        let cb_h = (cb_rect.height - cb_border * 2.0).max(0.0);

        let top = self.entries[idx].top.map(|r| resolve_offset(r, cb_h));
        let left = self.entries[idx].left.map(|r| resolve_offset(r, cb_w));
        let right = self.entries[idx].right.map(|r| resolve_offset(r, cb_w));
        let bottom = self.entries[idx].bottom.map(|r| resolve_offset(r, cb_h));

        // Width: left+right ambos setados e width é Auto → stretch. Caso contrário resolve normal.
        let resolved_width = match (left, right, self.entries[idx].width_rule) {
            (Some(l), Some(r), DimensionRule::Auto) => (cb_w - l - r).max(0.0),
            _ => {
                let entry = &self.entries[idx];
                if entry.width_rule == DimensionRule::Auto {
                    // auto sem stretch → shrink-to-fit (MVP: intrinsic ou cb_w)
                    width::resolve_auto_leaf(entry).min(cb_w)
                } else {
                    width::resolve(entry, cb_w)
                }
            }
        };
        let resolved_height = match (top, bottom, self.entries[idx].height_rule) {
            (Some(t), Some(b), DimensionRule::Auto) => (cb_h - t - b).max(0.0),
            _ => height::resolve(&self.entries[idx], cb_h),
        };

        // Posição final com base nos anchors disponíveis.
        let x = match (left, right) {
            (Some(l), _) => cb_x + l,
            (None, Some(r)) => cb_x + cb_w - r - resolved_width,
            // Sem left nem right: CSS usa "static position" (onde o elemento estaria
            // no flow). Simplificação: encosta no topo-esquerda do CB.
            (None, None) => cb_x,
        };
        let y = match (top, bottom) {
            (Some(t), _) => cb_y + t,
            (None, Some(b)) => cb_y + cb_h - b - resolved_height,
            (None, None) => cb_y,
        };

        self.sizes[idx] = Some(resolved_width);
        self.rects[idx] = LayoutRect {
            x,
            y,
            width: resolved_width,
            height: resolved_height,
        };
        // Recursão: filhos do positioned (podem ser positioned eles mesmos).
        self.layout_children(idx);
    }
}
