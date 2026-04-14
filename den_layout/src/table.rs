//! Tabela flat que calcula retângulos de layout para a árvore Den.

use crate::{
    BODY_INDEX, DimensionRule, DisplayMode, LayoutEntry, LayoutRect, config, flex, height, margin,
    width,
};
use std::sync::OnceLock;

/// Tabela de layout que resolve retângulos em CSS pixels.
///
/// Criada uma vez pelo código gerado e reutilizada todo frame.
pub struct LayoutTable {
    /// Flat list de todos os elementos, index 0 = body.
    pub entries: Vec<LayoutEntry>,
    /// Larguras resolvidas. `None` = ainda não calculada neste passe.
    /// Resetado no início de cada `resolve()`.
    pub sizes: Vec<Option<f32>>,
    /// Retângulos calculados em CSS pixels.
    pub rects: Vec<LayoutRect>,
    /// Máximo de iterações reservado para algoritmos futuros.
    pub max_passes: usize,
}

impl LayoutTable {
    /// Cria uma tabela com entries já montadas pelo codegen.
    pub fn new(entries: Vec<LayoutEntry>) -> Self {
        let len = entries.len();
        Self {
            entries,
            sizes: vec![None; len],
            rects: vec![LayoutRect::default(); len],
            max_passes: config::DEFAULT_MAX_PASSES,
        }
    }

    /// Resolve todas as larguras a partir da largura disponível.
    pub fn resolve(&mut self, available_width: f32) {
        self.resolve_in_viewport(available_width, 0.0);
    }

    /// Resolve layout a partir do viewport disponível, em CSS pixels.
    pub fn resolve_in_viewport(&mut self, available_width: f32, available_height: f32) {
        for size in &mut self.sizes {
            *size = None;
        }
        for rect in &mut self.rects {
            *rect = LayoutRect::default();
        }

        self.sizes[BODY_INDEX] = Some(available_width);
        self.rects[BODY_INDEX] = LayoutRect {
            x: 0.0,
            y: 0.0,
            width: available_width,
            height: available_height,
        };

        self.layout_children(BODY_INDEX);
    }

    /// Compatibilidade com o codegen atual. Flex já é resolvido por `layout_children`.
    pub fn distribute_flex(&mut self) {}

    /// Resolve os filhos de um elemento conforme seu display mode.
    fn layout_children(&mut self, parent_idx: usize) {
        let display = self.entries[parent_idx].display;
        match display {
            // PENDING: `Grid` existe no tipo público, mas ainda usa fluxo block.
            DisplayMode::Block | DisplayMode::Grid => self.layout_block_children(parent_idx),
            DisplayMode::Flex => self.layout_flex_children(parent_idx),
        }
    }

    /// Resolve filhos em fluxo vertical de bloco.
    ///
    /// Margens uniformes são sempre reservadas; o motor ainda não implementa
    /// colapso de margin entre blocos como browsers fazem.
    fn layout_block_children(&mut self, parent_idx: usize) {
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
        let children = self.entries[parent_idx].children.clone();

        if children.is_empty() {
            if self.entries[parent_idx].height_rule == DimensionRule::Auto
                && parent_idx != BODY_INDEX
            {
                self.rects[parent_idx].height = self.rects[parent_idx]
                    .height
                    .max(height::resolve_auto_leaf(&self.entries[parent_idx]));
            }
            return;
        }

        for (pos, child_idx) in children.iter().copied().enumerate() {
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
            if pos + 1 < children.len() {
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

    /// Resolve filhos em fluxo horizontal flex.
    fn layout_flex_children(&mut self, parent_idx: usize) {
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
        let children = self.entries[parent_idx].children.clone();
        if children.is_empty() {
            return;
        }

        let gap_total = flex::gap_total(gap, children.len());
        let margin_total: f32 = children
            .iter()
            .map(|&child_idx| margin::uniform_extent(self.entries[child_idx].margin))
            .sum();
        let mut fixed_total = 0.0;
        let mut grow_total = 0.0;

        for &child_idx in &children {
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

        for (pos, child_idx) in children.iter().copied().enumerate() {
            let margin = self.entries[child_idx].margin;
            let grow = self.entries[child_idx].flex_grow;
            let child = &self.entries[child_idx];
            let fixed_width = if child.width_rule == DimensionRule::Auto && grow == 0.0 {
                width::resolve_auto_leaf(child)
            } else {
                width::resolve(child, content_width)
            };
            let resolved_width = flex::distribute_flex_width(
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
            max_height = max_height
                .max(self.rects[child_idx].height + margin::uniform_extent(margin));
            cursor_x += margin::uniform_extent(margin) + resolved_width;
            if pos + 1 < children.len() {
                cursor_x += gap;
            }
        }

        if self.entries[parent_idx].height_rule == DimensionRule::Auto && parent_idx != BODY_INDEX {
            // Altura natural = max child height + padding*2 + border*2.
            self.rects[parent_idx].height = max_height + edge * 2.0;
        }
    }


    /// Emite no stderr um snapshot da tabela resolvida.
    pub fn debug_dump(&self, template_path: &str, labels: &[&str]) {
        eprintln!(
            "DenLayout[{template_path}]: entries={} labels={}",
            self.entries.len(),
            labels.len()
        );
        for (idx, entry) in self.entries.iter().enumerate() {
            let label = labels.get(idx).copied().unwrap_or("<missing-label>");
            let rect = self.rects.get(idx).copied().unwrap_or_default();
            let size = self.sizes.get(idx).copied().flatten();
            eprintln!(
                "  [{idx}] {label} parent={:?} children={:?} display={:?} width={:?} height={:?} min_w={:?} max_w={:?} min_h={:?} max_h={:?} padding={} border={} margin={} gap={} flex_grow={} intrinsic_width={} intrinsic_height={} size={:?} rect=({}, {}, {}, {})",
                entry.parent,
                entry.children,
                entry.display,
                entry.width_rule,
                entry.height_rule,
                entry.min_width,
                entry.max_width,
                entry.min_height,
                entry.max_height,
                entry.padding,
                entry.border_width,
                entry.margin,
                entry.gap,
                entry.flex_grow,
                entry.intrinsic_width,
                entry.intrinsic_height,
                size,
                rect.x,
                rect.y,
                rect.width,
                rect.height,
            );
        }
    }
}

/// Retorna se o dump de layout está habilitado no ambiente.
/// Largura/altura de conteúdo disponível dentro de um rect: tira padding + border (2 lados).
fn content_axis(outer: f32, edge: f32) -> f32 {
    (outer - edge * 2.0).max(0.0)
}

pub fn layout_debug_enabled() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();

    *CACHED.get_or_init(|| match std::env::var(config::LAYOUT_DEBUG_ENV) {
        Ok(value) => value == config::DEBUG_ON || value.eq_ignore_ascii_case("true"),
        Err(std::env::VarError::NotPresent) => false,
        Err(err) => {
            eprintln!("Den: falha ao ler {}: {err}", config::LAYOUT_DEBUG_ENV);
            false
        }
    })
}

#[cfg(test)]
mod tests {
    use super::LayoutTable;
    use crate::{DimensionRule, DisplayMode, LayoutEntry};

    /// Constrói uma `LayoutTable` sem depender do codegen.
    fn make_table(mut entries: Vec<LayoutEntry>) -> LayoutTable {
        for i in 1..entries.len() {
            let parent = entries[i].parent.expect("non-body must have parent");
            entries[parent].children.push(i);
        }
        LayoutTable::new(entries)
    }

    /// Cria a entry raiz invisível.
    fn body() -> LayoutEntry {
        LayoutEntry::default()
    }

    /// Cria uma entry block com parent e regra de largura.
    fn entry(parent: usize, rule: DimensionRule) -> LayoutEntry {
        LayoutEntry {
            parent: Some(parent),
            width_rule: rule,
            ..LayoutEntry::default()
        }
    }

    /// Cria uma entry flex com parent e regra de largura.
    fn flex_entry(parent: usize, rule: DimensionRule) -> LayoutEntry {
        LayoutEntry {
            parent: Some(parent),
            width_rule: rule,
            display: DisplayMode::Flex,
            ..LayoutEntry::default()
        }
    }

    /// Cria uma entry auto com flex-grow.
    fn flex_grow_entry(parent: usize) -> LayoutEntry {
        LayoutEntry {
            flex_grow: 1.0,
            ..entry(parent, DimensionRule::Auto)
        }
    }

    /// Cria uma entry com padding uniforme.
    fn padded_entry(parent: usize, rule: DimensionRule, padding: f32) -> LayoutEntry {
        LayoutEntry {
            padding,
            ..entry(parent, rule)
        }
    }

    /// Cria uma entry com margin uniforme.
    fn margined_entry(parent: usize, rule: DimensionRule, margin: f32) -> LayoutEntry {
        LayoutEntry {
            margin,
            ..entry(parent, rule)
        }
    }

    #[test]
    fn body_receives_available_width() {
        let mut table = make_table(vec![body()]);
        table.resolve(800.0);
        assert_eq!(table.sizes[0], Some(800.0));
    }

    #[test]
    fn fixed_px_ignores_parent() {
        let mut table = make_table(vec![body(), entry(0, DimensionRule::Px(200.0))]);
        table.resolve(800.0);
        assert_eq!(table.sizes[1], Some(200.0));
    }

    #[test]
    fn percent_resolves_from_parent() {
        let mut table = make_table(vec![body(), entry(0, DimensionRule::Percent(0.5))]);
        table.resolve(800.0);
        assert_eq!(table.sizes[1], Some(400.0));
    }

    #[test]
    fn percent_100_equals_parent() {
        let mut table = make_table(vec![body(), entry(0, DimensionRule::Percent(1.0))]);
        table.resolve(600.0);
        assert_eq!(table.sizes[1], Some(600.0));
    }

    #[test]
    fn auto_leaf_fills_parent() {
        let mut table = make_table(vec![body(), entry(0, DimensionRule::Auto)]);
        table.resolve(800.0);
        assert_eq!(table.sizes[1], Some(800.0));
    }

    #[test]
    fn auto_block_container_fills_parent_context() {
        let mut table = make_table(vec![
            body(),
            entry(0, DimensionRule::Auto),
            entry(1, DimensionRule::Px(300.0)),
        ]);
        table.resolve(800.0);
        assert_eq!(table.sizes[2], Some(300.0));
        assert_eq!(table.sizes[1], Some(800.0));
    }

    #[test]
    fn flex_distributes_equally_between_auto_children() {
        let mut table = make_table(vec![
            body(),
            flex_entry(0, DimensionRule::Percent(1.0)),
            flex_grow_entry(1),
            flex_grow_entry(1),
            flex_grow_entry(1),
        ]);
        table.resolve(600.0);
        table.distribute_flex();
        assert_eq!(table.sizes[1], Some(600.0));
        assert_eq!(table.sizes[2], Some(200.0));
        assert_eq!(table.sizes[3], Some(200.0));
        assert_eq!(table.sizes[4], Some(200.0));
    }

    #[test]
    fn flex_fixed_child_takes_priority_auto_gets_remainder() {
        let mut table = make_table(vec![
            body(),
            flex_entry(0, DimensionRule::Percent(1.0)),
            entry(1, DimensionRule::Px(200.0)),
            flex_grow_entry(1),
        ]);
        table.resolve(600.0);
        table.distribute_flex();
        assert_eq!(table.sizes[2], Some(200.0));
        assert_eq!(table.sizes[3], Some(400.0));
    }

    #[test]
    fn auto_flex_child_uses_intrinsic_width_without_grow() {
        let mut table = make_table(vec![
            body(),
            flex_entry(0, DimensionRule::Px(600.0)),
            LayoutEntry {
                intrinsic_width: 72.0,
                ..entry(1, DimensionRule::Auto)
            },
            flex_grow_entry(1),
        ]);
        table.resolve(800.0);
        assert_eq!(table.sizes[2], Some(72.0));
        assert_eq!(table.sizes[3], Some(528.0));
        assert_eq!(table.rects[3].x, 72.0);
    }

    #[test]
    fn percent_of_fixed_parent_not_of_body() {
        let mut table = make_table(vec![
            body(),
            entry(0, DimensionRule::Px(600.0)),
            entry(1, DimensionRule::Percent(0.5)),
        ]);
        table.resolve(800.0);
        assert_eq!(table.sizes[1], Some(600.0));
        assert_eq!(table.sizes[2], Some(300.0));
    }

    #[test]
    fn percent_child_uses_parent_content_width() {
        let mut table = make_table(vec![
            body(),
            padded_entry(0, DimensionRule::Px(600.0), 20.0),
            entry(1, DimensionRule::Percent(1.0)),
        ]);
        table.resolve(800.0);
        assert_eq!(table.sizes[1], Some(600.0));
        assert_eq!(table.sizes[2], Some(560.0));
    }

    #[test]
    fn intrinsic_height_contributes_to_auto_block_height() {
        let mut table = make_table(vec![
            body(),
            padded_entry(0, DimensionRule::Px(600.0), 10.0),
            LayoutEntry {
                intrinsic_height: 18.0,
                ..entry(1, DimensionRule::Auto)
            },
        ]);
        table.resolve(800.0);
        assert_eq!(table.rects[2].height, 18.0);
        assert_eq!(table.rects[1].height, 38.0);
    }

    #[test]
    fn margin_reduces_block_child_available_width() {
        let mut table = make_table(vec![body(), margined_entry(0, DimensionRule::Auto, 20.0)]);
        table.resolve(600.0);
        assert_eq!(table.sizes[1], Some(560.0));
        assert_eq!(table.rects[1].x, 20.0);
        assert_eq!(table.rects[1].y, 20.0);
    }

    #[test]
    fn block_margins_do_not_collapse() {
        let mut table = make_table(vec![
            body(),
            padded_entry(0, DimensionRule::Px(600.0), 10.0),
            LayoutEntry {
                height_rule: DimensionRule::Px(20.0),
                margin: 5.0,
                ..entry(1, DimensionRule::Auto)
            },
            LayoutEntry {
                height_rule: DimensionRule::Px(20.0),
                margin: 5.0,
                ..entry(1, DimensionRule::Auto)
            },
        ]);
        table.resolve(800.0);
        assert_eq!(table.rects[2].y, 15.0);
        assert_eq!(table.rects[3].y, 45.0);
        assert_eq!(table.rects[1].height, 80.0);
    }

    #[test]
    fn margin_is_reserved_before_flex_grow_distribution() {
        let mut table = make_table(vec![
            body(),
            flex_entry(0, DimensionRule::Px(600.0)),
            LayoutEntry {
                margin: 10.0,
                ..flex_grow_entry(1)
            },
            LayoutEntry {
                margin: 10.0,
                ..flex_grow_entry(1)
            },
        ]);
        table.resolve(800.0);
        assert_eq!(table.sizes[2], Some(280.0));
        assert_eq!(table.sizes[3], Some(280.0));
        assert_eq!(table.rects[2].x, 10.0);
        assert_eq!(table.rects[3].x, 310.0);
    }

    #[test]
    fn resize_recalculates_all() {
        let mut table = make_table(vec![
            body(),
            entry(0, DimensionRule::Percent(1.0)),
            entry(0, DimensionRule::Px(200.0)),
        ]);
        table.resolve(800.0);
        assert_eq!(table.sizes[1], Some(800.0));
        assert_eq!(table.sizes[2], Some(200.0));

        table.resolve(1200.0);
        assert_eq!(table.sizes[1], Some(1200.0));
        assert_eq!(table.sizes[2], Some(200.0));
    }

    #[test]
    fn percent_in_flex_is_treated_as_fixed() {
        let mut table = make_table(vec![
            body(),
            flex_entry(0, DimensionRule::Px(600.0)),
            entry(1, DimensionRule::Percent(0.75)),
            flex_grow_entry(1),
        ]);
        table.resolve(800.0);
        table.distribute_flex();
        assert_eq!(table.sizes[2], Some(450.0));
        assert_eq!(table.sizes[3], Some(150.0));
    }

    #[test]
    fn nested_flex_distributes_correctly() {
        let mut table = make_table(vec![
            body(),
            flex_entry(0, DimensionRule::Percent(1.0)),
            LayoutEntry {
                display: DisplayMode::Flex,
                ..flex_grow_entry(1)
            },
            flex_grow_entry(2),
            flex_grow_entry(2),
            flex_grow_entry(1),
        ]);

        table.resolve(800.0);
        table.distribute_flex();

        assert_eq!(table.sizes[1], Some(800.0));
        assert_eq!(table.sizes[2], Some(400.0));
        assert_eq!(table.sizes[5], Some(400.0));
        assert_eq!(table.sizes[3], Some(200.0));
        assert_eq!(table.sizes[4], Some(200.0));
    }

    #[test]
    fn auto_does_not_exceed_parent() {
        let mut table = make_table(vec![
            body(),
            entry(0, DimensionRule::Auto),
            entry(1, DimensionRule::Px(700.0)),
        ]);
        table.resolve(600.0);
        assert_eq!(table.sizes[2], Some(700.0));
        assert_eq!(table.sizes[1], Some(600.0));
    }
}
