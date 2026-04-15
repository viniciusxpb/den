//! Render tree — árvore de elementos com valores resolvidos.
//!
//! Construída a cada frame pelo código gerado pelo `den_template!`. Alimenta
//! a `LayoutTable` via `to_layout_entries()` e é percorrida pelo painter pra
//! desenhar o conteúdo no backend ativo (egui hoje).

use crate::{DenNodeId, DimensionRule, DisplayMode, LayoutEntry};

/// RGB sem canal alpha — formato canônico do Den pra cores.
pub type Rgb = (u8, u8, u8);

/// Transformação textual declarada via `text-transform`.
///
/// SYNC: espelho de `den_macros::types::TextTransform`; manter variantes sincronizadas.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum TextTransform {
    /// Sem transformação.
    #[default]
    None,
    /// Converte texto para maiúsculas.
    Uppercase,
    /// Converte texto para minúsculas.
    Lowercase,
    /// Capitaliza a primeira letra de cada palavra.
    Capitalize,
}

/// Alinhamento horizontal de texto dentro da caixa do nó.
///
/// SYNC: espelho de `den_macros::types::TextAlign`; manter variantes sincronizadas.
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub enum TextAlign {
    /// Alinha à esquerda.
    #[default]
    Left,
    /// Centraliza.
    Center,
    /// Alinha à direita.
    Right,
}

/// Estilo visual resolvido de um nó pronto pro painter.
#[derive(Debug, Clone, PartialEq)]
pub struct PaintStyle {
    /// Cor do texto (quando houver).
    pub color: Option<Rgb>,
    /// Cor de fundo (preenchimento).
    pub background: Option<Rgb>,
    /// Cor da borda.
    pub border_color: Option<Rgb>,
    /// Largura da borda em CSS pixels. 0 = sem borda.
    pub border_width: f32,
    /// Raio dos cantos em CSS pixels.
    pub border_radius: f32,
    /// Tamanho da fonte em CSS pixels (quando o nó renderizar texto).
    pub font_size: f32,
    /// Pilha CSS de fontes emitida pelo codegen como literal estático.
    ///
    /// O `DenVisual` usa `String` durante parse/resolve, mas `PaintStyle` é o
    /// produto final gerado pelo macro, então o codegen consegue emitir
    /// `Some("Inter, sans-serif")` como `&'static str` sem alocação em runtime.
    pub font_family: Option<&'static str>,
    /// Peso CSS da fonte (`400`, `700`, etc.). Armazenado para seleção futura de faces.
    pub font_weight: u16,
    /// Se `font-style` pediu itálico/oblíquo.
    pub font_italic: bool,
    /// Altura de linha absoluta em CSS pixels. `0` = usar métrica da fonte.
    pub line_height: f32,
    /// Altura de linha multiplicadora. `0` = não declarada.
    pub line_height_factor: f32,
    /// Espaço extra entre letras em CSS pixels.
    pub letter_spacing: f32,
    /// Transformação textual aplicada antes de medir/pintar.
    pub text_transform: TextTransform,
    /// Alinhamento horizontal do texto dentro do rect.
    pub text_align: TextAlign,
    /// Desenha sublinhado.
    pub underline: bool,
    /// Desenha tachado.
    pub strikethrough: bool,
    /// Trocar o cursor pra pointer quando o mouse tá em cima.
    pub cursor_pointer: bool,
}

impl Default for PaintStyle {
    fn default() -> Self {
        Self {
            color: None,
            background: None,
            border_color: None,
            border_width: 0.0,
            border_radius: 0.0,
            font_size: 0.0,
            font_family: None,
            font_weight: 400,
            font_italic: false,
            line_height: 0.0,
            line_height_factor: 0.0,
            letter_spacing: 0.0,
            text_transform: TextTransform::None,
            text_align: TextAlign::Left,
            underline: false,
            strikethrough: false,
            cursor_pointer: false,
        }
    }
}

/// Conteúdo pintado por um nó.
#[derive(Debug, Clone)]
pub enum RenderKind {
    /// Só retângulo + borda + filhos. Sem texto.
    Container,
    /// Retângulo + texto já interpolado.
    Text {
        /// String final (já passou por `format!` / interpolação).
        content: String,
        /// Se deve usar tamanho/peso de heading.
        heading: bool,
    },
    /// Input pintado: retângulo, valor vindo da `DenRouteState`, caret.
    Input {
        /// Identificador estável usado como chave em `DenInputState`/`focus`.
        node_id: DenNodeId,
        /// Texto exibido quando o valor está vazio.
        placeholder: Option<String>,
    },
}

/// Dados que alimentam uma `LayoutEntry` na tabela de layout.
/// Caixa externa (margin) envolve caixa da borda (border) envolve caixa de padding
/// envolve caixa de conteúdo. O box model CSS canônico.
#[derive(Debug, Clone)]
pub struct LayoutIntent {
    pub width_rule: DimensionRule,
    pub height_rule: DimensionRule,
    /// Limite mínimo aplicado depois do `width_rule` resolver.
    pub min_width: Option<DimensionRule>,
    /// Limite máximo aplicado depois do `width_rule` resolver.
    pub max_width: Option<DimensionRule>,
    pub min_height: Option<DimensionRule>,
    pub max_height: Option<DimensionRule>,
    pub display: DisplayMode,
    pub padding: f32,
    pub border_width: f32,
    pub margin: f32,
    pub gap: f32,
    pub flex_grow: f32,
    /// Largura do CONTEÚDO próprio (texto/input), sem padding e sem border.
    pub intrinsic_width: f32,
    /// Altura do CONTEÚDO próprio, sem padding e sem border.
    pub intrinsic_height: f32,
    /// Esquema de posicionamento — afeta se o elemento entra no flow normal.
    pub position: crate::PositionKind,
    /// Offset vertical do topo do containing block (só aplicado se positioned).
    pub top: Option<DimensionRule>,
    /// Offset horizontal da borda esquerda do containing block.
    pub left: Option<DimensionRule>,
    /// Offset horizontal da borda direita do containing block.
    pub right: Option<DimensionRule>,
    /// Offset vertical do bottom do containing block.
    pub bottom: Option<DimensionRule>,
    /// Ordem de paint entre positioned siblings. `None` = 0 (default CSS `auto`).
    pub z_index: Option<i32>,
}

impl Default for LayoutIntent {
    fn default() -> Self {
        Self {
            width_rule: DimensionRule::Auto,
            height_rule: DimensionRule::Auto,
            min_width: None,
            max_width: None,
            min_height: None,
            max_height: None,
            display: DisplayMode::Block,
            padding: 0.0,
            border_width: 0.0,
            margin: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
            intrinsic_width: 0.0,
            intrinsic_height: 0.0,
            position: crate::PositionKind::Static,
            top: None,
            left: None,
            right: None,
            bottom: None,
            z_index: None,
        }
    }
}

/// Intenção de interação do nó — índices em tabelas mantidas pelo macro.
#[derive(Debug, Clone, Copy, Default)]
pub struct Interact {
    /// Índice do handler de click na tabela do template. `None` = não clicável.
    pub click_handler: Option<u32>,
    /// Índice na tabela de slots de navegação (`goto`).
    pub goto_slot: Option<u32>,
    /// Forçar `CursorIcon::PointingHand` no hover mesmo sem `hover_style`.
    pub pointer_on_hover: bool,
}

impl Interact {
    /// Retorna se o nó reage a cliques.
    pub fn is_clickable(&self) -> bool {
        self.click_handler.is_some() || self.goto_slot.is_some()
    }
}

/// Nó da render tree.
#[derive(Debug, Clone)]
pub struct RenderNode {
    /// Identificador estável entre frames (salt por índice de loop quando em `<for>`).
    pub node_id: DenNodeId,
    /// Índice deste nó na `LayoutTable` gerada (== `nodes.len()` no momento do push + 1 pro body).
    pub layout_index: usize,
    /// Conteúdo pintado.
    pub kind: RenderKind,
    /// Estilo base.
    pub style: PaintStyle,
    /// Estilo aplicado quando o nó está em hover (já resolvido, não é delta).
    pub hover_style: Option<PaintStyle>,
    /// Intenção de interação.
    pub interact: Interact,
    /// Dados de layout alimentados pra `LayoutEntry`.
    pub layout: LayoutIntent,
    /// Índices em `RenderTree::nodes` dos filhos diretos.
    pub children: Vec<usize>,
}

impl RenderNode {
    /// Construtor mínimo com valores default.
    pub fn new(node_id: DenNodeId, layout_index: usize, kind: RenderKind) -> Self {
        Self {
            node_id,
            layout_index,
            kind,
            style: PaintStyle::default(),
            hover_style: None,
            interact: Interact::default(),
            layout: LayoutIntent::default(),
            children: Vec::new(),
        }
    }
}

/// Árvore de render construída a cada frame pelo macro.
///
/// Os nós são armazenados flat; `roots` contém os índices dos filhos diretos
/// do body (que é entrada 0 da `LayoutTable`, sem `RenderNode` correspondente).
#[derive(Debug, Default)]
pub struct RenderTree {
    /// Nós em ordem DFS de push.
    pub nodes: Vec<RenderNode>,
    /// Índices em `nodes` dos roots (filhos diretos do body invisível).
    pub roots: Vec<usize>,
    /// Estilo do seletor `body` do SCSS (background, border, font padrão).
    /// `None` quando o SCSS não declara `body { ... }`.
    pub body_style: Option<PaintStyle>,
}

impl RenderTree {
    /// Cria uma tree vazia.
    pub fn new() -> Self {
        Self::default()
    }

    /// Empurra um nó e retorna seu índice em `nodes`.
    ///
    /// O caller é responsável por popular `children` do pai e, se for root,
    /// adicionar o índice retornado em `self.roots`.
    pub fn push(&mut self, node: RenderNode) -> usize {
        let idx = self.nodes.len();
        self.nodes.push(node);
        idx
    }

    /// Converte a render tree numa `Vec<LayoutEntry>` pronta pra `LayoutTable`.
    ///
    /// A entrada `0` é sempre o body invisível (parent=None, display=Block, tudo zero).
    /// Cada `RenderNode` vira uma entrada em `RenderNode::layout_index`.
    pub fn to_layout_entries(&self) -> Vec<LayoutEntry> {
        let mut entries = Vec::with_capacity(self.nodes.len() + 1);

        // Body em 0. Body é sempre `position: relative` implicitamente pra servir de
        // containing block default pros `position: absolute` sem positioned ancestor.
        entries.push(LayoutEntry {
            parent: None,
            children: self
                .roots
                .iter()
                .map(|&r| self.nodes[r].layout_index)
                .collect(),
            width_rule: DimensionRule::Auto,
            height_rule: DimensionRule::Auto,
            min_width: None,
            max_width: None,
            min_height: None,
            max_height: None,
            display: DisplayMode::Block,
            padding: 0.0,
            border_width: 0.0,
            margin: 0.0,
            gap: 0.0,
            flex_grow: 0.0,
            intrinsic_width: 0.0,
            intrinsic_height: 0.0,
            position: crate::PositionKind::Relative,
            top: None,
            left: None,
            right: None,
            bottom: None,
            z_index: None,
        });

        // Prepara entradas por nó (sem parent ainda).
        for node in &self.nodes {
            let l = &node.layout;
            entries.push(LayoutEntry {
                parent: None,
                children: node
                    .children
                    .iter()
                    .map(|&c| self.nodes[c].layout_index)
                    .collect(),
                width_rule: l.width_rule,
                height_rule: l.height_rule,
                min_width: l.min_width,
                max_width: l.max_width,
                min_height: l.min_height,
                max_height: l.max_height,
                display: l.display,
                padding: l.padding,
                border_width: l.border_width,
                margin: l.margin,
                gap: l.gap,
                flex_grow: l.flex_grow,
                intrinsic_width: l.intrinsic_width,
                intrinsic_height: l.intrinsic_height,
                position: l.position,
                top: l.top,
                left: l.left,
                right: l.right,
                bottom: l.bottom,
                z_index: l.z_index,
            });
        }

        // Popula parent: roots apontam pra body (0), demais pro layout_index do pai.
        for &root_idx in &self.roots {
            let li = self.nodes[root_idx].layout_index;
            entries[li].parent = Some(0);
        }
        for (parent_pos, parent_node) in self.nodes.iter().enumerate() {
            for &child_pos in &parent_node.children {
                let _ = parent_pos;
                let child_li = self.nodes[child_pos].layout_index;
                let parent_li = parent_node.layout_index;
                entries[child_li].parent = Some(parent_li);
            }
        }

        entries
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn simple_tree() -> RenderTree {
        // body
        // └── root (layout_index=1)
        //     ├── child_a (layout_index=2)
        //     └── child_b (layout_index=3)
        let mut tree = RenderTree::new();

        let mut root = RenderNode::new(DenNodeId::new(1), 1, RenderKind::Container);
        root.layout.padding = 8.0;
        let root_idx = tree.push(root);
        tree.roots.push(root_idx);

        let child_a = RenderNode::new(
            DenNodeId::new(2),
            2,
            RenderKind::Text {
                content: "A".into(),
                heading: false,
            },
        );
        let a_idx = tree.push(child_a);
        tree.nodes[root_idx].children.push(a_idx);

        let child_b = RenderNode::new(
            DenNodeId::new(3),
            3,
            RenderKind::Text {
                content: "B".into(),
                heading: false,
            },
        );
        let b_idx = tree.push(child_b);
        tree.nodes[root_idx].children.push(b_idx);

        tree
    }

    #[test]
    fn to_layout_entries_sets_parent_and_children() {
        let tree = simple_tree();
        let entries = tree.to_layout_entries();

        assert_eq!(entries.len(), 4, "body + 3 nós");

        // Body
        assert!(entries[0].parent.is_none());
        assert_eq!(entries[0].children, vec![1]);

        // Root filho do body.
        assert_eq!(entries[1].parent, Some(0));
        assert_eq!(entries[1].children, vec![2, 3]);
        assert_eq!(entries[1].padding, 8.0);

        // Filhos apontam pro root.
        assert_eq!(entries[2].parent, Some(1));
        assert!(entries[2].children.is_empty());
        assert_eq!(entries[3].parent, Some(1));
        assert!(entries[3].children.is_empty());
    }

    #[test]
    fn to_layout_entries_compatible_with_layout_table() {
        use crate::LayoutTable;
        let tree = simple_tree();
        let entries = tree.to_layout_entries();
        let mut table = LayoutTable::new(entries);
        table.resolve_in_viewport(400.0, 300.0);

        // Body ocupa todo o viewport.
        assert_eq!(table.rects[0].width, 400.0);
        // Root é Auto dentro do body → preenche largura.
        assert_eq!(table.rects[1].width, 400.0);
        // Filhos recebem content_width do root (400 - padding*2 = 384).
        assert_eq!(table.rects[2].width, 384.0);
        assert_eq!(table.rects[3].width, 384.0);
    }

    #[test]
    fn empty_tree_yields_only_body() {
        let tree = RenderTree::new();
        let entries = tree.to_layout_entries();
        assert_eq!(entries.len(), 1);
        assert!(entries[0].parent.is_none());
        assert!(entries[0].children.is_empty());
    }
}
