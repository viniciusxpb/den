//! Den Layout System — resolução iterativa de larguras em runtime.
//!
//! Flat list, múltiplas passadas, cada passada resolve o que pode
//! usando o que a anterior salvou. Roda a cada frame (immediate mode).
//!
//! ESCOPO: só largura. Altura, posição vertical, margin, gap são
//! problemas separados pra depois.

/// Índice do body na lista. Sempre 0.
pub const BODY_INDEX: usize = 0;

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

/// Display mode do elemento.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DisplayMode {
    Block,
    Flex,
}

/// Uma entrada na flat list de layout.
/// Cada elemento HTML parseado gera uma LayoutEntry.
#[derive(Debug, Clone)]
pub struct LayoutEntry {
    /// Índice deste elemento na lista (0 = body).
    pub index: usize,
    /// Índice do pai na lista (`None` só pro body).
    pub parent: Option<usize>,
    /// Índices dos filhos diretos.
    pub children: Vec<usize>,
    /// Nível de profundidade (body = 0, L1 = 1, ...).
    pub depth: usize,
    /// Regra de largura declarada no SCSS.
    pub width_rule: WidthRule,
    /// Display mode — determina como distribui espaço pros filhos.
    pub display: DisplayMode,
}

/// Tabela de layout que resolve larguras iterativamente.
/// Criada uma vez, reutilizada todo frame.
pub struct LayoutTable {
    /// Flat list de todos os elementos, index 0 = body.
    pub entries: Vec<LayoutEntry>,
    /// Larguras resolvidas. `None` = ainda não resolvido neste frame.
    /// Resetado no início de cada `resolve()`.
    pub sizes: Vec<Option<f32>>,
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
            max_passes: 5,
        }
    }

    /// Resolve todas as larguras. Chamado todo frame antes do render.
    /// `available_width` vem de `ui.available_width()`.
    pub fn resolve(&mut self, available_width: f32) {
        // Reset
        for size in &mut self.sizes {
            *size = None;
        }

        // Body sempre = largura disponível do egui
        self.sizes[BODY_INDEX] = Some(available_width);

        // Loop até ponto fixo ou max_passes
        for _ in 0..self.max_passes {
            if !self.run_one_pass() {
                break;
            }
        }
    }

    /// Uma passada de resolução. Retorna `true` se algum valor mudou.
    fn run_one_pass(&mut self) -> bool {
        let mut changed = false;

        for i in 0..self.entries.len() {
            if self.sizes[i].is_some() {
                continue;
            }

            // Px e Percent não precisam dos children — clone só no branch Auto.
            match self.entries[i].width_rule {
                WidthRule::Px(px) => {
                    self.sizes[i] = Some(px);
                    changed = true;
                }

                WidthRule::Percent(pct) => {
                    let parent_width = self.entries[i].parent
                        .and_then(|p| self.sizes[p]);
                    if let Some(pw) = parent_width {
                        self.sizes[i] = Some(pw * pct);
                        changed = true;
                    }
                }

                WidthRule::Auto => {
                    let parent = self.entries[i].parent;
                    let display = self.entries[i].display;
                    // Clone só aqui, onde é necessário pra iterar com borrow split
                    let children = self.entries[i].children.clone();

                    if children.is_empty() {
                        // Folha sem filhos: encaixa no pai
                        if let Some(parent_width) = parent.and_then(|p| self.sizes[p]) {
                            self.sizes[i] = Some(parent_width);
                            changed = true;
                        }
                    } else {
                        // Tem filhos: abraça se todos resolvidos
                        let all_resolved = children.iter().all(|&c| self.sizes[c].is_some());
                        if all_resolved {
                            let total = self.calculate_children_width(&children, display);
                            // Auto não cresce além do pai
                            let capped = parent
                                .and_then(|p| self.sizes[p])
                                .map_or(total, |pw| total.min(pw));
                            self.sizes[i] = Some(capped);
                            changed = true;
                        }
                        // Se filhos não resolvidos, espera próxima passada
                    }
                }
            }
        }

        changed
    }

    /// Pós-processamento: distribui espaço restante entre filhos `Auto`
    /// em containers flex. Chamado depois de `resolve()`.
    ///
    /// Ordem importa: o loop é top-down (índice menor = mais próximo da raiz),
    /// então containers flex aninhados são processados na ordem correta —
    /// pai antes de filho. Isso é garantido pela construção da flat list
    /// (DFS pré-ordem) em `from_den_nodes`.
    ///
    /// `Px` e `Percent` são tratados como **largura fixa** neste contexto:
    /// `resolve()` já calculou seus valores, `distribute_flex` só aloca o resto.
    /// Só filhos `Auto` recebem espaço distribuído.
    pub fn distribute_flex(&mut self) {
        for i in 0..self.entries.len() {
            if self.entries[i].display != DisplayMode::Flex { continue; }
            if self.entries[i].children.is_empty() { continue; }
            let Some(parent_width) = self.sizes[i] else { continue; };

            // Clone após os continues — não aloca se o elemento for ignorado
            let children = self.entries[i].children.clone();

            // Px e Percent já foram resolvidos por resolve() → tratados como fixos.
            // Só Auto entra na distribuição de espaço restante.
            let mut fixed_total: f32 = 0.0;
            let mut flex_children: Vec<usize> = Vec::new();

            for &child_idx in &children {
                match self.entries[child_idx].width_rule {
                    WidthRule::Auto => flex_children.push(child_idx),
                    // Px e Percent: usa o tamanho já resolvido como fixo
                    _ => fixed_total += self.sizes[child_idx].unwrap_or(0.0),
                }
            }

            if flex_children.is_empty() { continue; }

            // Espaço restante dividido igualmente entre filhos Auto
            let remaining = (parent_width - fixed_total).max(0.0);
            let per_child = remaining / flex_children.len() as f32;

            for &child_idx in &flex_children {
                self.sizes[child_idx] = Some(per_child);
            }
        }
    }

    /// Largura total dos filhos baseado no display mode do pai.
    fn calculate_children_width(&self, children: &[usize], display: DisplayMode) -> f32 {
        let widths: Vec<f32> = children.iter().filter_map(|&c| self.sizes[c]).collect();
        if widths.is_empty() {
            return 0.0;
        }
        match display {
            // Flex: lado a lado → soma
            DisplayMode::Flex => widths.iter().sum(),
            // Block: empilhados → o maior
            DisplayMode::Block => widths.iter().cloned().fold(0.0_f32, f32::max),
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
            let parent = entries[i].parent.unwrap_or(0);
            let child = entries[i].index;
            entries[parent].children.push(child);
        }
        LayoutTable::new(entries)
    }

    fn body() -> LayoutEntry {
        LayoutEntry {
            index: 0, parent: None, children: vec![], depth: 0,
            width_rule: WidthRule::Auto, display: DisplayMode::Block,
        }
    }

    fn entry(index: usize, parent: usize, rule: WidthRule) -> LayoutEntry {
        LayoutEntry {
            index, parent: Some(parent), children: vec![], depth: 1,
            width_rule: rule, display: DisplayMode::Block,
        }
    }

    fn flex_entry(index: usize, parent: usize, rule: WidthRule) -> LayoutEntry {
        LayoutEntry {
            index, parent: Some(parent), children: vec![], depth: 1,
            width_rule: rule, display: DisplayMode::Flex,
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
        let mut t = make_table(vec![body(), entry(1, 0, WidthRule::Px(200.0))]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(200.0));
    }

    #[test]
    fn percent_resolves_from_parent() {
        let mut t = make_table(vec![body(), entry(1, 0, WidthRule::Percent(0.5))]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(400.0));
    }

    #[test]
    fn percent_100_equals_parent() {
        let mut t = make_table(vec![body(), entry(1, 0, WidthRule::Percent(1.0))]);
        t.resolve(600.0);
        assert_eq!(t.sizes[1], Some(600.0));
    }

    #[test]
    fn auto_leaf_fills_parent() {
        let mut t = make_table(vec![body(), entry(1, 0, WidthRule::Auto)]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(800.0));
    }

    #[test]
    fn auto_block_container_embraces_largest_child() {
        // pai auto (block), filho fixo 300px → pai fica 300px (capped ao body 800)
        let mut t = make_table(vec![
            body(),
            entry(1, 0, WidthRule::Auto),   // container block auto
            entry(2, 1, WidthRule::Px(300.0)),
        ]);
        t.resolve(800.0);
        assert_eq!(t.sizes[2], Some(300.0));
        assert_eq!(t.sizes[1], Some(300.0));
    }

    #[test]
    fn flex_distributes_equally_between_auto_children() {
        // pai flex 600px, 3 filhos auto → cada um 200px após distribute_flex
        let mut t = make_table(vec![
            body(),
            flex_entry(1, 0, WidthRule::Percent(1.0)),  // flex container 100%
            entry(2, 1, WidthRule::Auto),
            entry(3, 1, WidthRule::Auto),
            entry(4, 1, WidthRule::Auto),
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
            flex_entry(1, 0, WidthRule::Percent(1.0)),
            entry(2, 1, WidthRule::Px(200.0)),
            entry(3, 1, WidthRule::Auto),
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
            entry(1, 0, WidthRule::Px(600.0)),  // pai fixo 600px
            LayoutEntry {
                index: 2, parent: Some(1), children: vec![], depth: 2,
                width_rule: WidthRule::Percent(0.5), display: DisplayMode::Block,
            },
        ]);
        t.resolve(800.0);
        assert_eq!(t.sizes[1], Some(600.0));
        assert_eq!(t.sizes[2], Some(300.0)); // 50% de 600, NÃO de 800
    }

    #[test]
    fn resize_recalculates_all() {
        // mesmo template, janela muda de 800 pra 1200
        let mut t = make_table(vec![
            body(),
            entry(1, 0, WidthRule::Percent(1.0)),
            entry(2, 0, WidthRule::Px(200.0)),
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
            flex_entry(1, 0, WidthRule::Px(600.0)),
            entry(2, 1, WidthRule::Percent(0.75)),
            entry(3, 1, WidthRule::Auto),
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
            body(),
            flex_entry(1, 0, WidthRule::Percent(1.0)),  // L1 flex
            flex_entry(2, 1, WidthRule::Auto),           // L2 flex (filho do L1)
            entry(3, 2, WidthRule::Auto),                // neto A
            entry(4, 2, WidthRule::Auto),                // neto B
            entry(5, 1, WidthRule::Auto),                // L2 simples (irmão)
        ]);
        t.entries[2].depth = 2;
        t.entries[3].depth = 3;
        t.entries[4].depth = 3;
        t.entries[5].depth = 2;

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
        // container auto (block) com um filho maior que o pai → capped no pai
        // block mode usa MAX dos filhos: 700 > 600 → capped em 600
        let mut t = make_table(vec![
            body(),
            entry(1, 0, WidthRule::Auto),       // container auto
            entry(2, 1, WidthRule::Px(700.0)),  // filho maior que o pai
        ]);
        t.resolve(600.0);
        assert_eq!(t.sizes[2], Some(700.0)); // filho não é capped (só o container é)
        assert_eq!(t.sizes[1], Some(600.0)); // container capped no pai
    }
}
