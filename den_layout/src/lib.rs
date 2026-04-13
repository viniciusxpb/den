//! Den Layout System — resolução de layout em runtime.
//!
//! Flat list em ordem DFS, com o `body` invisível no índice 0. O pai define o
//! algoritmo de layout dos filhos: block, flex e futuramente grid. Roda a cada
//! frame porque egui é immediate mode e a janela pode mudar de tamanho.

mod router;
mod state;

pub use router::{DenPage, DenRouter};
pub use state::{DenDebugState, DenInputState, DenNodeId, DenRouteState};

/// Índice do body na lista. Sempre 0.
pub const BODY_INDEX: usize = 0;

/// Propriedades visuais de um elemento Den, extraídas em compile time.
/// Passado como argumento quando o handler usa a keyword `style`.
#[derive(Debug, Clone, Default)]
pub struct DenElementStyle {
    pub color: Option<(u8, u8, u8)>,
    pub background: Option<(u8, u8, u8)>,
    pub font_size: Option<f32>,
    pub padding: Option<f32>,
    pub border_radius: Option<f32>,
    pub border_width: Option<f32>,
    pub border_color: Option<(u8, u8, u8)>,
    pub width_px: Option<f32>,
    pub width_percent: Option<f32>,
    pub is_flex: bool,
}

/// Como a largura foi declarada no SCSS.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WidthRule {
    /// Sem width no SCSS.
    /// - Com filhos: abraça o maior filho (block) ou soma (flex)
    /// - Sem filhos: encaixa no pai (100%)
    Auto,
    /// Valor fixo em pixels: `width: 200px` → `Px(200.0)`
    Px(f32),
    /// Percentagem do pai: `width: 50%` → `Percent(0.5)`
    Percent(f32),
}

/// Caixa calculada para um elemento.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct LayoutRect {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

/// Display mode do elemento.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DisplayMode {
    Block,
    Flex,
    Grid,
}

/// Uma entrada na flat list de layout.
/// Cada elemento HTML parseado gera uma LayoutEntry.
#[derive(Debug, Clone)]
pub struct LayoutEntry {
    /// Índice do pai na lista (`None` só pro body).
    /// O índice deste entry é sua posição no Vec — não precisa de campo separado.
    pub parent: Option<usize>,
    /// Índices dos filhos diretos.
    pub children: Vec<usize>,
    /// Regra de largura declarada no SCSS.
    pub width_rule: WidthRule,
    /// Regra de altura declarada no SCSS.
    pub height_rule: WidthRule,
    /// Display mode — determina como distribui espaço pros filhos.
    pub display: DisplayMode,
    /// Padding uniforme em CSS pixels.
    pub padding: f32,
    /// Gap entre filhos diretos em CSS pixels.
    pub gap: f32,
    /// Peso de flex-grow. 0 = não cresce.
    pub flex_grow: f32,
}

/// Tabela de layout que resolve larguras iterativamente.
/// Criada uma vez, reutilizada todo frame.
pub struct LayoutTable {
    /// Flat list de todos os elementos, index 0 = body.
    pub entries: Vec<LayoutEntry>,
    /// Larguras resolvidas. `None` = ainda não resolvido neste frame.
    /// Resetado no início de cada `resolve()`.
    pub sizes: Vec<Option<f32>>,
    /// Retângulos calculados em CSS pixels.
    pub rects: Vec<LayoutRect>,
    /// Máximo de iterações pra evitar loop infinito (na prática 2-3 bastam).
    pub max_passes: usize,
}

impl LayoutTable {
    /// Cria uma tabela vazia com apenas o body.
    /// Use `from_entries` depois de construir as entries via codegen.
    pub fn new(entries: Vec<LayoutEntry>) -> Self {
        let len = entries.len();
        Self {
            entries,
            sizes: vec![None; len],
            rects: vec![LayoutRect::default(); len],
            max_passes: 5,
        }
    }

    /// Resolve todas as larguras. Chamado todo frame antes do render.
    /// `available_width` vem de `ui.available_width()`.
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

    fn layout_children(&mut self, parent_idx: usize) {
        let display = self.entries[parent_idx].display;
        match display {
            DisplayMode::Block | DisplayMode::Grid => self.layout_block_children(parent_idx),
            DisplayMode::Flex => self.layout_flex_children(parent_idx),
        }
    }

    fn layout_block_children(&mut self, parent_idx: usize) {
        let parent_rect = self.rects[parent_idx];
        let padding = self.entries[parent_idx].padding;
        let gap = self.entries[parent_idx].gap;
        let content_x = parent_rect.x + padding;
        let content_width = (parent_rect.width - padding * 2.0).max(0.0);
        let mut cursor_y = parent_rect.y + padding;
        let children = self.entries[parent_idx].children.clone();

        for (pos, child_idx) in children.iter().copied().enumerate() {
            let width = self.resolve_child_width(child_idx, content_width);
            let height = self.resolve_child_height(child_idx, 0.0);
            self.sizes[child_idx] = Some(width);
            self.rects[child_idx] = LayoutRect {
                x: content_x,
                y: cursor_y,
                width,
                height,
            };
            self.layout_children(child_idx);
            cursor_y += self.rects[child_idx].height;
            if pos + 1 < children.len() {
                cursor_y += gap;
            }
        }

        if self.entries[parent_idx].height_rule == WidthRule::Auto && parent_idx != BODY_INDEX {
            self.rects[parent_idx].height = cursor_y - parent_rect.y + padding;
        }
    }

    fn layout_flex_children(&mut self, parent_idx: usize) {
        let parent_rect = self.rects[parent_idx];
        let padding = self.entries[parent_idx].padding;
        let gap = self.entries[parent_idx].gap;
        let content_x = parent_rect.x + padding;
        let content_width = (parent_rect.width - padding * 2.0).max(0.0);
        let children = self.entries[parent_idx].children.clone();
        if children.is_empty() {
            return;
        }

        let gap_total = gap * children.len().saturating_sub(1) as f32;
        let mut fixed_total = 0.0;
        let mut grow_total = 0.0;

        for &child_idx in &children {
            let grow = self.entries[child_idx].flex_grow;
            if grow > 0.0 && self.entries[child_idx].width_rule == WidthRule::Auto {
                grow_total += grow;
            } else if self.entries[child_idx].width_rule == WidthRule::Auto {
                // Auto sem flex-grow é content-sized no render egui. O layout
                // ainda não mede conteúdo, então não desconta do espaço flex.
            } else {
                fixed_total += self.resolve_child_width(child_idx, content_width);
            }
        }

        let remaining = (content_width - fixed_total - gap_total).max(0.0);
        let mut cursor_x = content_x;
        let content_y = parent_rect.y + padding;
        let mut max_height = 0.0f32;

        for (pos, child_idx) in children.iter().copied().enumerate() {
            let grow = self.entries[child_idx].flex_grow;
            let width = if grow > 0.0 && self.entries[child_idx].width_rule == WidthRule::Auto {
                if grow_total > 0.0 {
                    remaining * (grow / grow_total)
                } else {
                    0.0
                }
            } else if self.entries[child_idx].width_rule == WidthRule::Auto {
                0.0
            } else {
                self.resolve_child_width(child_idx, content_width)
            };
            let height = self.resolve_child_height(child_idx, 0.0);
            self.sizes[child_idx] =
                if self.entries[child_idx].width_rule == WidthRule::Auto && grow <= 0.0 {
                    None
                } else {
                    Some(width)
                };
            self.rects[child_idx] = LayoutRect {
                x: cursor_x,
                y: content_y,
                width,
                height,
            };
            self.layout_children(child_idx);
            max_height = max_height.max(self.rects[child_idx].height);
            cursor_x += width;
            if pos + 1 < children.len() {
                cursor_x += gap;
            }
        }

        if self.entries[parent_idx].height_rule == WidthRule::Auto && parent_idx != BODY_INDEX {
            self.rects[parent_idx].height = max_height + padding * 2.0;
        }
    }

    fn resolve_child_width(&self, child_idx: usize, parent_content_width: f32) -> f32 {
        match self.entries[child_idx].width_rule {
            WidthRule::Px(px) => px,
            WidthRule::Percent(pct) => parent_content_width * pct,
            WidthRule::Auto => parent_content_width,
        }
    }

    fn resolve_child_height(&self, child_idx: usize, parent_content_height: f32) -> f32 {
        match self.entries[child_idx].height_rule {
            WidthRule::Px(px) => px,
            WidthRule::Percent(pct) => parent_content_height * pct,
            WidthRule::Auto => 0.0,
        }
    }
}

// ============================================================================
// Testes
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Constrói uma LayoutTable a partir de entries sem children preenchidos,
    /// e popula os children a partir dos parents (mesmo que o codegen gerado faz).
    fn make_table(mut entries: Vec<LayoutEntry>) -> LayoutTable {
        for i in 1..entries.len() {
            let parent = entries[i].parent.expect("non-body must have parent");
            entries[parent].children.push(i);
        }
        LayoutTable::new(entries)
    }

    fn body() -> LayoutEntry {
        LayoutEntry {
            parent: None,
            children: vec![],
            width_rule: WidthRule::Auto,
            height_rule: WidthRule::Auto,
            display: DisplayMode::Block,
            padding: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
        }
    }

    /// Helper: cria entry block com parent e regra de largura.
    /// O index é a posição no Vec passado pra make_table.
    fn entry(parent: usize, rule: WidthRule) -> LayoutEntry {
        LayoutEntry {
            parent: Some(parent),
            children: vec![],
            width_rule: rule,
            height_rule: WidthRule::Auto,
            display: DisplayMode::Block,
            padding: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
        }
    }

    fn flex_entry(parent: usize, rule: WidthRule) -> LayoutEntry {
        LayoutEntry {
            parent: Some(parent),
            children: vec![],
            width_rule: rule,
            height_rule: WidthRule::Auto,
            display: DisplayMode::Flex,
            padding: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
        }
    }

    fn flex_grow_entry(parent: usize) -> LayoutEntry {
        LayoutEntry {
            flex_grow: 1.0,
            ..entry(parent, WidthRule::Auto)
        }
    }

    fn padded_entry(parent: usize, rule: WidthRule, padding: f32) -> LayoutEntry {
        LayoutEntry {
            padding,
            ..entry(parent, rule)
        }
    }

    #[test]
    fn body_receives_available_width() {
        let mut t = make_table(vec![body()]);
        t.resolve(800.0);
        assert_eq!(t.sizes[0], Some(800.0));
    }

    #[test]
    fn fixed_px_ignores_parent() {
        let mut t = make_table(vec![body(), entry(0, WidthRule::Px(200.0))]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(200.0));
    }

    #[test]
    fn percent_resolves_from_parent() {
        let mut t = make_table(vec![body(), entry(0, WidthRule::Percent(0.5))]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(400.0));
    }

    #[test]
    fn percent_100_equals_parent() {
        let mut t = make_table(vec![body(), entry(0, WidthRule::Percent(1.0))]);
        t.resolve(600.0);
        assert_eq!(t.sizes[1], Some(600.0));
    }

    #[test]
    fn auto_leaf_fills_parent() {
        let mut t = make_table(vec![body(), entry(0, WidthRule::Auto)]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(800.0));
    }

    #[test]
    fn auto_block_container_fills_parent_context() {
        // Block auto segue o contexto do pai, como uma div CSS normal.
        let mut t = make_table(vec![
            body(),
            entry(0, WidthRule::Auto), // container block auto
            entry(1, WidthRule::Px(300.0)),
        ]);
        t.resolve(800.0);
        assert_eq!(t.sizes[2], Some(300.0));
        assert_eq!(t.sizes[1], Some(800.0));
    }

    #[test]
    fn flex_distributes_equally_between_auto_children() {
        // pai flex 600px, 3 filhos auto → cada um 200px após distribute_flex
        let mut t = make_table(vec![
            body(),
            flex_entry(0, WidthRule::Percent(1.0)), // flex container 100%
            flex_grow_entry(1),
            flex_grow_entry(1),
            flex_grow_entry(1),
        ]);
        t.resolve(600.0);
        t.distribute_flex();
        assert_eq!(t.sizes[1], Some(600.0));
        assert_eq!(t.sizes[2], Some(200.0));
        assert_eq!(t.sizes[3], Some(200.0));
        assert_eq!(t.sizes[4], Some(200.0));
    }

    #[test]
    fn flex_fixed_child_takes_priority_auto_gets_remainder() {
        // flex 600px, filho fixo 200px + filho auto → auto fica 400px
        let mut t = make_table(vec![
            body(),
            flex_entry(0, WidthRule::Percent(1.0)),
            entry(1, WidthRule::Px(200.0)),
            flex_grow_entry(1),
        ]);
        t.resolve(600.0);
        t.distribute_flex();
        assert_eq!(t.sizes[2], Some(200.0));
        assert_eq!(t.sizes[3], Some(400.0));
    }

    #[test]
    fn percent_of_fixed_parent_not_of_body() {
        // Percentagem resolve do PAI DIRETO, não do body.
        // filho=50% de pai=600px → 300px, independente do body ser 800px
        let mut t = make_table(vec![
            body(),
            entry(0, WidthRule::Px(600.0)), // pai fixo 600px
            entry(1, WidthRule::Percent(0.5)),
        ]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(600.0));
        assert_eq!(t.sizes[2], Some(300.0)); // 50% de 600, NÃO de 800
    }

    #[test]
    fn percent_child_uses_parent_content_width() {
        // pai 600px com padding 20 → content width 560.
        // filho 100% deve usar 560, não a largura externa do pai.
        let mut t = make_table(vec![
            body(),
            padded_entry(0, WidthRule::Px(600.0), 20.0),
            entry(1, WidthRule::Percent(1.0)),
        ]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(600.0));
        assert_eq!(t.sizes[2], Some(560.0));
    }

    #[test]
    fn resize_recalculates_all() {
        // mesmo template, janela muda de 800 pra 1200
        let mut t = make_table(vec![
            body(),
            entry(0, WidthRule::Percent(1.0)),
            entry(0, WidthRule::Px(200.0)),
        ]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(800.0));
        assert_eq!(t.sizes[2], Some(200.0));

        t.resolve(1200.0);
        assert_eq!(t.sizes[1], Some(1200.0));
        assert_eq!(t.sizes[2], Some(200.0)); // fixo não muda
    }

    #[test]
    fn percent_in_flex_is_treated_as_fixed() {
        // flex 600px, filho 75% + filho auto
        // resolve(): filho[2] = 75% de 600 = 450
        // distribute_flex(): Percent é fixo → remaining = 600 - 450 = 150 → auto fica 150
        let mut t = make_table(vec![
            body(),
            flex_entry(0, WidthRule::Px(600.0)),
            entry(1, WidthRule::Percent(0.75)),
            flex_grow_entry(1),
        ]);
        t.resolve(800.0);
        t.distribute_flex();
        assert_eq!(t.sizes[2], Some(450.0)); // resolve() mantido, não sobrescrito
        assert_eq!(t.sizes[3], Some(150.0)); // 600 - 450 = 150
    }

    #[test]
    fn nested_flex_distributes_correctly() {
        // body 800 → flex L1 (100%) → 2 filhos auto (400 cada)
        //   → filho[2] é flex com 2 netos auto (200 cada)
        // distribute_flex processa em ordem de índice (top-down):
        // L1(1) → seta filhos[2,5] = 400 cada
        // L2(2) → agora tem 400, seta filhos[3,4] = 200 cada
        let mut t = make_table(vec![
            body(),                                 // 0
            flex_entry(0, WidthRule::Percent(1.0)), // 1: L1 flex
            LayoutEntry {
                display: DisplayMode::Flex,
                ..flex_grow_entry(1)
            }, // 2: L2 flex (filho do L1)
            flex_grow_entry(2),                     // 3: neto A
            flex_grow_entry(2),                     // 4: neto B
            flex_grow_entry(1),                     // 5: L2 simples (irmão)
        ]);

        t.resolve(800.0);
        t.distribute_flex();

        assert_eq!(t.sizes[1], Some(800.0)); // L1
        assert_eq!(t.sizes[2], Some(400.0)); // metade do L1
        assert_eq!(t.sizes[5], Some(400.0)); // metade do L1
        assert_eq!(t.sizes[3], Some(200.0)); // metade do L2 (400/2)
        assert_eq!(t.sizes[4], Some(200.0));
    }

    #[test]
    fn auto_does_not_exceed_parent() {
        // container auto (block) preenche o pai; filho fixo pode exceder.
        let mut t = make_table(vec![
            body(),
            entry(0, WidthRule::Auto),      // container auto
            entry(1, WidthRule::Px(700.0)), // filho maior que o pai
        ]);
        t.resolve(600.0);
        assert_eq!(t.sizes[2], Some(700.0)); // filho não é capped (só o container é)
        assert_eq!(t.sizes[1], Some(600.0)); // container segue o pai
    }
}
