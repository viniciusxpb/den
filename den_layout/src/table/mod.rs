//! Tabela flat que calcula retângulos de layout para a árvore Den.
//!
//! Estrutura:
//! - este `mod.rs` — struct `LayoutTable`, entry points (`new`, `resolve`,
//!   `resolve_in_viewport`), dispatcher `layout_children`, debug, e o helper
//!   `layout_debug_enabled`.
//! - [`mod@block`] — pass de layout vertical (display: block).
//! - [`mod@flex`] — pass de layout horizontal (display: flex).
//! - [`mod@positioned`] — segunda pass pra `position: absolute|fixed`,
//!   com lookup de containing block.
//!
//! Cada submódulo adiciona métodos a `impl LayoutTable` via blocos `impl`
//! distribuídos — Rust permite. A ordem de execução por parent é sempre:
//! "in-flow children primeiro (block ou flex) → positioned children depois".

mod block;
mod flex;
mod positioned;

use crate::{BODY_INDEX, DimensionRule, DisplayMode, LayoutEntry, LayoutRect, config};
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
    ///
    /// Pipeline por parent:
    /// 1. In-flow children: layout block ou flex normalmente (positioned são ignorados).
    /// 2. Positioned children (`absolute`/`fixed`): após o flow normal, via `layout_positioned`.
    ///
    /// Positioned não afetam `auto-height` do pai nem tomam espaço dos siblings in-flow
    /// (comportamento CSS spec).
    pub(super) fn layout_children(&mut self, parent_idx: usize) {
        let display = self.entries[parent_idx].display;
        match display {
            // PENDING: `Grid` existe no tipo público, mas ainda usa fluxo block.
            DisplayMode::Block | DisplayMode::Grid => self.layout_block_children(parent_idx),
            DisplayMode::Flex => self.layout_flex_children(parent_idx),
        }

        // Segunda pass: filhos positioned. Absolute usa o containing block do nearest
        // positioned ancestor; fixed usa sempre o body.
        let children = self.entries[parent_idx].children.clone();
        for child_idx in children {
            if self.entries[child_idx].position.is_out_of_flow() {
                self.layout_positioned(child_idx);
            }
        }
    }

    /// Emite no stderr um snapshot da tabela resolvida.
    ///
    /// Inclui propriedades de positioning (`position`/`top`/`left`/`right`/`bottom`/`z_index`)
    /// pra que `DEN_DEBUG_LAYOUT=1` permita verificar se elementos absolute/fixed
    /// foram colocados no rect correto pelo CB lookup.
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
            // Só imprime campos de positioning quando RELEVANTES (não-default),
            // pra dump não virar barulho em elementos comuns (a maioria é Static).
            let pos_str = if entry.position != crate::PositionKind::Static {
                format!(
                    " position={:?} top={:?} left={:?} right={:?} bottom={:?} z={:?}",
                    entry.position,
                    entry.top,
                    entry.left,
                    entry.right,
                    entry.bottom,
                    entry.z_index,
                )
            } else {
                String::new()
            };
            let bw = entry.border_widths;
            eprintln!(
                "  [{idx}] {label} parent={:?} children={:?} display={:?} width={:?} height={:?} min_w={:?} max_w={:?} min_h={:?} max_h={:?} padding={} border=[{}, {}, {}, {}] margin={} gap={} flex_grow={} intrinsic_width={} intrinsic_height={} size={:?} rect=({}, {}, {}, {}){pos_str}",
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
                bw[0],
                bw[1],
                bw[2],
                bw[3],
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

/// Resolve um offset (`top`/`left`/`right`/`bottom`) contra a extensão (width ou height)
/// do containing block. `Auto` aqui é fallback defensivo — o parser converte `auto`
/// pra `None` no `Option<DimensionRule>`, então não deveria chegar até aqui. Caso
/// algum codegen futuro emita `Auto`, tratamos como 0 pra não falhar.
pub(super) fn resolve_offset(rule: DimensionRule, cb_extent: f32) -> f32 {
    match rule {
        DimensionRule::Px(v) => v,
        DimensionRule::Percent(p) => p * cb_extent,
        DimensionRule::Auto => 0.0,
    }
}

/// Retorna se o dump de layout está habilitado no ambiente.
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

    // ---- position: absolute / relative / fixed ----

    fn positioned(parent: usize, kind: crate::PositionKind, w: f32, h: f32) -> LayoutEntry {
        LayoutEntry {
            parent: Some(parent),
            position: kind,
            width_rule: DimensionRule::Px(w),
            height_rule: DimensionRule::Px(h),
            ..LayoutEntry::default()
        }
    }

    /// Helper: faz body ser positioned (Relative) pra simular comportamento do
    /// `to_layout_entries` runtime.
    fn body_relative() -> LayoutEntry {
        LayoutEntry {
            position: crate::PositionKind::Relative,
            ..LayoutEntry::default()
        }
    }

    #[test]
    fn absolute_child_uses_top_left_offsets_from_body() {
        // Sem positioned ancestor explícito → body (Relative) é o CB.
        let mut child = positioned(0, crate::PositionKind::Absolute, 100.0, 50.0);
        child.top = Some(DimensionRule::Px(20.0));
        child.left = Some(DimensionRule::Px(40.0));
        let mut table = make_table(vec![body_relative(), child]);
        table.resolve_in_viewport(800.0, 600.0);
        assert_eq!(table.rects[1].x, 40.0);
        assert_eq!(table.rects[1].y, 20.0);
        assert_eq!(table.rects[1].width, 100.0);
        assert_eq!(table.rects[1].height, 50.0);
    }

    #[test]
    fn absolute_uses_nearest_positioned_ancestor_as_containing_block() {
        // body > relative_parent (200x100 at 50,30) > absolute_child (top:10 left:5)
        // CB do absolute = relative_parent. Posição final = (50+5, 30+10).
        let mut entries = vec![body_relative()];
        entries.push(LayoutEntry {
            parent: Some(0),
            position: crate::PositionKind::Relative,
            width_rule: DimensionRule::Px(200.0),
            height_rule: DimensionRule::Px(100.0),
            margin: 0.0,
            ..LayoutEntry::default()
        });
        // Move o relative pro deslocamento esperado via ordering — simples: ele será
        // o primeiro filho do body, então cai em (0,0). Ajusta o teste pra (0,0).
        let mut absolute_child = positioned(1, crate::PositionKind::Absolute, 30.0, 20.0);
        absolute_child.top = Some(DimensionRule::Px(10.0));
        absolute_child.left = Some(DimensionRule::Px(5.0));
        entries.push(absolute_child);
        let mut table = make_table(entries);
        table.resolve_in_viewport(800.0, 600.0);
        // CB = relative_parent at (0,0) with 200x100; offsets top:10 left:5.
        assert_eq!(table.rects[2].x, 5.0);
        assert_eq!(table.rects[2].y, 10.0);
    }

    #[test]
    fn absolute_skips_static_ancestors_for_containing_block() {
        // body > static_div (full width) > absolute_child
        // Static ancestor é IGNORADO; CB cai no body.
        let mut entries = vec![body_relative()];
        entries.push(LayoutEntry {
            parent: Some(0),
            width_rule: DimensionRule::Percent(1.0),
            height_rule: DimensionRule::Px(200.0),
            ..LayoutEntry::default()
        });
        let mut absolute_child = positioned(1, crate::PositionKind::Absolute, 50.0, 50.0);
        absolute_child.top = Some(DimensionRule::Px(15.0));
        absolute_child.right = Some(DimensionRule::Px(25.0));
        entries.push(absolute_child);
        let mut table = make_table(entries);
        table.resolve_in_viewport(800.0, 600.0);
        // CB = body (800x600). right:25 + width:50 → x = 800 - 25 - 50 = 725.
        assert_eq!(table.rects[2].x, 725.0);
        assert_eq!(table.rects[2].y, 15.0);
    }

    #[test]
    fn absolute_left_and_right_stretches_to_fill() {
        // CB = body (1000 wide). left:50 right:50, width:auto → width = 1000-50-50 = 900.
        let mut child = positioned(0, crate::PositionKind::Absolute, 0.0, 30.0);
        child.width_rule = DimensionRule::Auto;
        child.left = Some(DimensionRule::Px(50.0));
        child.right = Some(DimensionRule::Px(50.0));
        let mut table = make_table(vec![body_relative(), child]);
        table.resolve_in_viewport(1000.0, 400.0);
        assert_eq!(table.rects[1].x, 50.0);
        assert_eq!(table.rects[1].width, 900.0);
    }

    #[test]
    fn fixed_always_uses_body_as_containing_block() {
        // body > relative_parent > fixed_child. CB do fixed = body, NÃO o relative.
        let mut entries = vec![body_relative()];
        entries.push(LayoutEntry {
            parent: Some(0),
            position: crate::PositionKind::Relative,
            width_rule: DimensionRule::Px(200.0),
            height_rule: DimensionRule::Px(100.0),
            ..LayoutEntry::default()
        });
        let mut fixed_child = positioned(1, crate::PositionKind::Fixed, 80.0, 40.0);
        fixed_child.top = Some(DimensionRule::Px(10.0));
        fixed_child.left = Some(DimensionRule::Px(20.0));
        entries.push(fixed_child);
        let mut table = make_table(entries);
        table.resolve_in_viewport(800.0, 600.0);
        // Fixed ignora o relative; pin no body → posição absoluta no viewport.
        assert_eq!(table.rects[2].x, 20.0);
        assert_eq!(table.rects[2].y, 10.0);
    }

    #[test]
    fn absolute_does_not_affect_parent_auto_height() {
        // Parent block com 1 filho in-flow (height 50) + 1 absolute (height 200).
        // Auto height do parent só conta o in-flow → 50.
        let mut entries = vec![body_relative()];
        entries.push(LayoutEntry {
            parent: Some(0),
            position: crate::PositionKind::Relative,
            width_rule: DimensionRule::Percent(1.0),
            ..LayoutEntry::default()
        });
        entries.push(LayoutEntry {
            height_rule: DimensionRule::Px(50.0),
            ..entry(1, DimensionRule::Auto)
        });
        let mut absolute_child = positioned(1, crate::PositionKind::Absolute, 100.0, 200.0);
        absolute_child.top = Some(DimensionRule::Px(0.0));
        absolute_child.left = Some(DimensionRule::Px(0.0));
        entries.push(absolute_child);
        let mut table = make_table(entries);
        table.resolve_in_viewport(800.0, 600.0);
        // Parent (idx 1) auto-height = só o in-flow child = 50.
        assert_eq!(table.rects[1].height, 50.0);
    }

    #[test]
    fn absolute_percent_offset_resolves_against_containing_block() {
        // CB body 800x600. left:25%, top:50% → x=200, y=300.
        let mut child = positioned(0, crate::PositionKind::Absolute, 100.0, 50.0);
        child.left = Some(DimensionRule::Percent(0.25));
        child.top = Some(DimensionRule::Percent(0.5));
        let mut table = make_table(vec![body_relative(), child]);
        table.resolve_in_viewport(800.0, 600.0);
        assert_eq!(table.rects[1].x, 200.0);
        assert_eq!(table.rects[1].y, 300.0);
    }
}
