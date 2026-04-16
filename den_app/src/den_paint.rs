//! Adapter egui do Den — ponte única entre `RenderTree`/`LayoutTable` e o backend.
//!
//! ## Arquitetura (fluxo unidirecional separado)
//!
//! ```text
//!                        render_frame()
//!                              │
//!         ┌────────────────────┼────────────────────┐
//!         │                    │                    │
//!    layout_pass          event_pass           paint_pass
//!     (pura, sem           (egui source)       (egui sink)
//!      toque em egui)           │                    │
//!                               ▼                    ▼
//!                       Vec<PaintEvent>        draws on egui
//!                           │
//!                           ▼
//!                     Dispatcher (no
//!                     código gerado)
//! ```
//!
//! - **`layout_pass`**: mede texto, monta `LayoutTable.entries`, resolve rects.
//!   Pura — não toca `egui::Painter`. Pode rodar em qualquer lugar.
//! - **`event_pass`**: fonte de eventos do egui. Chama `ui.interact()` em cada nó
//!   pra hit-test, gerencia foco/caret/teclado de inputs, atualiza `hover` no
//!   `DenRouteState`, e coleta `Vec<PaintEvent>`. Não desenha nada.
//! - **`paint_pass`**: consome `hover`/`focus` do state (populados pelo event_pass
//!   no mesmo frame) e desenha. Não chama `ui.interact()`, não emite eventos.
//!
//! A **ordem matters**: event_pass roda ANTES do paint_pass. Assim o frame N
//! já reage ao hover/focus do frame N (não do N-1), mantendo a UI responsiva
//! sem o atraso de 1 frame do modelo "paint → coletar → próximo frame".
//!
//! Os `PaintEvent`s retornados são despachados pelo código gerado pelo
//! `den_template!` pros handlers (`(click)`, `goto`, `InputChanged` pra two-way
//! binding).

use crate::paint_config::{
    INPUT_TEXT_PADDING_X, INPUT_TEXT_PADDING_Y, MIN_BORDER_WIDTH_PX, MIN_FONT_SIZE_PX,
    MIN_INSET_SHADOW_SPREAD_PX, SHADOW_BLUR_ALPHA_DECAY, SHADOW_BLUR_SAMPLES,
};
use den_layout::{
    DenNodeId, DenRouteState, LayoutRect, LayoutTable, PaintStyle, RenderKind, RenderTree, Rgb,
    TextAlign, TextTransform,
};
use eframe::egui::{
    self, Align, Color32, FontId, Id, Pos2, Rect, Sense, Stroke, StrokeKind, Ui, Vec2,
    text::{FontFamily, Galley, LayoutJob, TextFormat},
};
use std::sync::Arc;

/// Galley medida mais seu retângulo intrínseco. É a "caixa invisível" que o
/// layout usa em vez de depender do texto cru.
struct TextBox {
    /// Galley do egui já shapeada e pronta para pintura.
    galley: Arc<Galley>,
    /// Largura intrínseca do texto medido, em pontos/px do egui para a escala atual.
    width: f32,
    /// Altura intrínseca do texto medido, em pontos/px do egui para a escala atual.
    height: f32,
}

/// Eventos emitidos pelo painter — despachados pelo código gerado.
#[derive(Debug, Clone)]
pub enum PaintEvent {
    /// Usuário clicou num nó com `(click)="handler(...)"`.
    Click { handler: u32 },
    /// Usuário clicou num nó com `goto="PageName"`.
    Goto { slot: u32 },
    /// Texto de um input mudou (digitou/apagou).
    InputChanged { node_id: DenNodeId, value: String },
}

/// Orchestrator: roda os 3 passes em ordem e devolve os eventos coletados.
///
/// O código gerado pelo `den_template!` chama esta função e depois faz
/// dispatch dos eventos retornados pros handlers do usuário. Isto é o único
/// ponto de entrada público do painter.
///
/// Ordem dos passes:
/// 1. `layout_pass` — mede texto + resolve rects (pura)
/// 2. `event_pass` — hit-test + foco/teclado + coleta eventos
/// 3. Paint do body (seletor `body {}` do SCSS)
/// 4. `paint_pass` — desenha nós reutilizando hover/focus do event_pass
pub fn paint_tree(
    ui: &mut Ui,
    scale: f32,
    tree: &mut RenderTree,
    layout: &mut LayoutTable,
    state: &mut DenRouteState,
) -> Vec<PaintEvent> {
    render_frame(ui, scale, tree, layout, state)
}

/// Mesma coisa que `paint_tree`, nome novo reflete a arquitetura de 3 passes.
/// Mantemos `paint_tree` como alias porque o codegen do macro emite esse nome.
pub fn render_frame(
    ui: &mut Ui,
    scale: f32,
    tree: &mut RenderTree,
    layout: &mut LayoutTable,
    state: &mut DenRouteState,
) -> Vec<PaintEvent> {
    layout_pass(ui, scale, tree, layout);

    let origin = ui.min_rect().min;
    let body_rect = scaled_rect(
        layout.rects.first().copied().unwrap_or_default(),
        origin,
        scale,
    );
    ui.allocate_rect(body_rect, Sense::hover());

    // Event pass PRIMEIRO — assim paint_pass do mesmo frame já usa o hover/focus
    // recém-atualizado (sem atraso de 1 frame).
    state.hover_mut().clear();
    let events = event_pass(ui, scale, origin, tree, layout, state);

    // Pinta o body (seletor `body` no SCSS) antes dos nós. Equivalente ao
    // <body> do browser — define background, border, etc. do viewport inteiro.
    if let Some(body_style) = &tree.body_style {
        let body_painter = ui.painter_at(body_rect);
        paint_background(&body_painter, body_rect, body_style, scale);
        paint_border(&body_painter, body_rect, body_style, scale);
    }

    paint_pass(ui, scale, origin, tree, layout, state);

    events
}

/// **Pass 1 — layout** (função pura, NÃO chama `egui::Painter`).
///
/// Mede o texto intrínseco via `ui.fonts_mut` (única interação com egui aqui,
/// mas é só consulta de métricas — não desenha), popula `LayoutTable.entries`
/// a partir da `RenderTree`, e resolve rects no viewport atual.
///
/// Depois dessa função, `layout.rects[i]` tem x/y/width/height resolvidos em
/// CSS pixels pra todo nó. `event_pass` e `paint_pass` só leem daí.
fn layout_pass(ui: &Ui, scale: f32, tree: &mut RenderTree, layout: &mut LayoutTable) {
    // Medição intrínseca substitui a estimativa compile-time do codegen.
    measure_tree_text(ui, tree);

    layout.entries = tree.to_layout_entries();
    let len = layout.entries.len();
    if layout.sizes.len() != len {
        layout.sizes.clear();
        layout.sizes.resize(len, None);
        layout.rects.clear();
        layout.rects.resize(len, LayoutRect::default());
    }

    let viewport_w = ui.available_width() / scale;
    let viewport_h = ui.available_height() / scale;
    layout.resolve_in_viewport(viewport_w, viewport_h);
}

/// **Pass 2 — eventos** (egui source). Não desenha nada.
///
/// Walk DFS idêntico ao paint_pass: chama `ui.interact()` em cada nó pra
/// hit-test, atualiza `state.hover`/`state.focus`/`state.cursor` (inputs),
/// processa eventos de teclado quando há input focado, e coleta
/// `Vec<PaintEvent>` pra dispatch.
fn event_pass(
    ui: &mut Ui,
    scale: f32,
    origin: Pos2,
    tree: &RenderTree,
    layout: &LayoutTable,
    state: &mut DenRouteState,
) -> Vec<PaintEvent> {
    let mut events = Vec::new();
    for &root_idx in &tree.roots {
        collect_events_node(
            ui,
            scale,
            origin,
            tree,
            layout,
            state,
            root_idx,
            None,
            &mut events,
        );
    }
    events
}

/// **Pass 3 — paint** (egui sink). Não chama `ui.interact()`, não emite eventos.
///
/// Lê `state.hover`/`state.focus` populados pelo `event_pass` pra escolher o
/// estilo ativo (base vs hover) e desenha cada nó: drop-shadow → background →
/// inset-shadow → conteúdo (text/input visual) → border.
fn paint_pass(
    ui: &mut Ui,
    scale: f32,
    origin: Pos2,
    tree: &RenderTree,
    layout: &LayoutTable,
    state: &DenRouteState,
) {
    for &root_idx in &tree.roots {
        paint_node(ui, scale, origin, tree, layout, state, root_idx, None);
    }
}

/// Mede o CONTEÚDO (texto puro) de cada nó e atualiza `LayoutIntent::intrinsic_{width,height}`.
///
/// Contrato: `intrinsic_*` = dimensão do conteúdo SEM padding e SEM border.
/// O layout engine (`height::resolve`, `width::resolve`) é quem adiciona
/// padding + border em volta. Assim o box model CSS fica centralizado lá e
/// nada é aplicado duas vezes.
fn measure_tree_text(ui: &Ui, tree: &mut RenderTree) {
    for node in &mut tree.nodes {
        match &node.kind {
            RenderKind::Text { content, heading } => {
                if content.is_empty() {
                    node.layout.intrinsic_width = 0.0;
                    node.layout.intrinsic_height = 0.0;
                    continue;
                }
                let text = apply_text_transform(content, node.style.text_transform);
                let text_box =
                    layout_text_box(ui, &text, *heading, &node.style, 1.0, Color32::WHITE);
                node.layout.intrinsic_width = text_box.width;
                node.layout.intrinsic_height = text_box.height;
            }
            RenderKind::Input { .. } => {
                let text_box = layout_text_box(ui, "M", false, &node.style, 1.0, Color32::WHITE);
                node.layout.intrinsic_height = text_box.height;
                // Se o codegen não pré-preencheu intrinsic_width (via DEFAULT_INPUT_WIDTH),
                // garante um mínimo razoável pro input ser clicável.
                if node.layout.intrinsic_width < 40.0 {
                    node.layout.intrinsic_width = 40.0;
                }
            }
            RenderKind::Container => {
                // Containers não têm conteúdo próprio; a altura/largura vem dos filhos.
            }
        }
    }
}

/// Desenha um único nó da `RenderTree` (sem chamar `ui.interact()` nem emitir
/// eventos). Lê `state.hover`/`state.focus` populados pelo `event_pass`.
///
/// `parent_clip` é o rect máximo em que este nó pode pintar, vindo do
/// `overflow: hidden` de um ancestral. `None` = sem clipping extra (usa clip
/// do `ui` como limite natural).
#[allow(clippy::too_many_arguments)]
fn paint_node(
    ui: &mut Ui,
    scale: f32,
    origin: Pos2,
    tree: &RenderTree,
    layout: &LayoutTable,
    state: &DenRouteState,
    node_idx: usize,
    parent_clip: Option<Rect>,
) {
    let node = &tree.nodes[node_idx];
    let rect = scaled_rect(layout.rects[node.layout_index], origin, scale);

    let hovering = state.hover().contains(&node.node_id);
    let active_style: &PaintStyle = if hovering {
        node.hover_style.as_ref().unwrap_or(&node.style)
    } else {
        &node.style
    };

    // Clip efetivo: interseção do próprio rect com o clip do ancestral (se houver).
    // Assim `overflow: hidden` no pai faz filhos que extrapolam sumirem visualmente.
    let self_clip = match parent_clip {
        Some(pc) => rect.intersect(pc),
        None => rect,
    };
    // Clip pra sombras (extendem pra fora do rect): usa o clip do ancestral se
    // houver, senão o clip do ui. Assim drop shadows respeitam overflow do pai
    // mas não são cortadas no próprio rect do nó.
    let shadow_clip = parent_clip.unwrap_or_else(|| ui.clip_rect());

    paint_drop_shadows(
        &ui.painter().with_clip_rect(shadow_clip),
        rect,
        active_style,
        scale,
    );

    // Rotação: background/border/texto rotacionados via Mesh + `TextShape::angle`,
    // todos em volta do centro do rect (CSS default `transform-origin: 50% 50%`).
    //
    // Quando rotacionado, o painter NÃO pode ser `painter_at(self_clip)` porque
    // os cantos rotacionados extrapolam o rect axis-aligned e seriam cortados
    // (resultaria em octógono em vez de quadrado girado). Usamos `shadow_clip`
    // (clip do ancestral ou da `ui`) pra permitir que os cantos saiam do rect
    // original. A aparência continua correta porque o Mesh rotacionado só
    // desenha dentro dos 4 corners calculados.
    //
    // `border-radius` é ignorado quando rotated (Mesh não tem corner arcs).
    // Input rotacionado: caret/focus tratados como axis-aligned — follow-up.
    //
    // `PaintStyle::transform` só é `Some` quando não-identity (ver `transform_tokens`).
    let has_rotation = active_style.transform.is_some();

    // Painter pra conteúdo: axis-aligned cuts conteúdo que extrapola (ok);
    // rotated path usa o clip mais largo pra cantos não serem cortados.
    let painter = if has_rotation {
        ui.painter().with_clip_rect(shadow_clip)
    } else {
        ui.painter_at(self_clip)
    };

    if let Some(transform) = active_style.transform {
        paint_rotated_rect(&painter, rect, active_style, transform, scale);
    } else {
        paint_background(&painter, rect, active_style, scale);
        paint_inset_shadows(&painter, rect, active_style, scale);
    }

    match &node.kind {
        RenderKind::Container => {
            // Fundo + borda já pintados acima.
        }
        RenderKind::Text { content, heading } => {
            paint_text(ui, &painter, rect, content, *heading, active_style, scale);
        }
        RenderKind::Input {
            node_id,
            placeholder,
        } => {
            paint_input_visual(
                ui,
                &painter,
                rect,
                *node_id,
                placeholder.as_deref(),
                active_style,
                scale,
                state,
            );
        }
    }

    if !has_rotation {
        paint_border(&painter, rect, active_style, scale);
    }

    // Propaga clip pra filhos: se este nó tem `overflow: hidden`, filhos são
    // limitados ao `self_clip`; senão herda o `parent_clip` do ancestral.
    let child_clip = if active_style.overflow_hidden {
        Some(self_clip)
    } else {
        parent_clip
    };

    // Mesma ordem do event_pass: in-flow primeiro (tree order), depois positioned
    // ordenados por z-index ascendente (default 0; ties por tree order). Cobre os
    // casos comuns de overlay (ports sobre node, modal sobre canvas) sem implementar
    // stacking contexts completos.
    for child_idx in paint_order(tree, &node.children) {
        paint_node(ui, scale, origin, tree, layout, state, child_idx, child_clip);
    }
}

/// Pinta o background + border de um rect rotacionado via Mesh 2D.
///
/// Aplica em qualquer `RenderKind`. O conteúdo (texto/input) continua sendo
/// pintado axis-aligned POR CIMA — resulta num rect rotacionado com texto
/// flat em cima. Suficiente pra wires/badges; rotação de texto via
/// `TextShape::angle` é follow-up.
///
/// **Limitações MVP**:
/// - `border-radius` é ignorado (Mesh retangular sem corner arcs).
/// - `box-shadow` drop ainda é pintado axis-aligned atrás do rect original —
///   não acompanha a rotação. Visualmente errado em casos extremos, mas wires
///   do ndnm (caso principal de uso) não têm sombra, então OK pra MVP.
/// - Children continuam paintados axis-aligned (rotação NÃO propaga).
fn paint_rotated_rect(
    painter: &egui::Painter,
    rect: Rect,
    style: &PaintStyle,
    transform: den_layout::Transform2d,
    scale: f32,
) {
    let (sin, cos) = transform.rotation_rad.sin_cos();
    let center = rect.center();
    let corners = [
        rotate_around(rect.left_top(), center, sin, cos),
        rotate_around(rect.right_top(), center, sin, cos),
        rotate_around(rect.right_bottom(), center, sin, cos),
        rotate_around(rect.left_bottom(), center, sin, cos),
    ];

    // Fill (background) via Mesh de 2 triangles.
    if let Some(bg) = style.background {
        let fill = rgb_to_color(bg, style.opacity);
        let mut mesh = egui::epaint::Mesh::default();
        for corner in &corners {
            mesh.colored_vertex(*corner, fill);
        }
        mesh.add_triangle(0, 1, 2);
        mesh.add_triangle(0, 2, 3);
        painter.add(egui::Shape::mesh(mesh));
    }

    // Border: 4 line_segments rotacionados. Usa maior largura declarada
    // (aprox. uniforme) — rotated + per-side widths é combinação rara.
    let max_border_width = style.border_widths.iter().copied().fold(0.0f32, f32::max);
    if max_border_width > 0.0 && let Some(color) = style.border_color {
        let stroke_width = (max_border_width * scale).max(MIN_BORDER_WIDTH_PX);
        let stroke = Stroke::new(stroke_width, rgb_to_color(color, style.opacity));
        for i in 0..4 {
            painter.line_segment([corners[i], corners[(i + 1) % 4]], stroke);
        }
    }
}

/// Rotaciona um ponto em volta de um centro dado `sin`/`cos` do ângulo
/// (pré-calculados pra chamar 4× por rect sem redundância).
fn rotate_around(point: Pos2, center: Pos2, sin: f32, cos: f32) -> Pos2 {
    let dx = point.x - center.x;
    let dy = point.y - center.y;
    Pos2::new(
        center.x + dx * cos - dy * sin,
        center.y + dx * sin + dy * cos,
    )
}

/// Walk de eventos DFS: chama `ui.interact()` em cada nó, atualiza hover/focus,
/// processa teclado pra inputs focados, e coleta `PaintEvent`s pra dispatch.
///
/// **Não desenha**. A ordem DFS é IDÊNTICA à do `paint_node` pra garantir que
/// egui interprete o z-order corretamente (chamadas posteriores a `ui.interact`
/// sobrepõem as anteriores, o que bate com filhos sobre pais visualmente).
///
/// `parent_clip` é propagado pelos `overflow: hidden` ancestrais. Hit-test é
/// feito na interseção do rect próprio com o clip — clicks fora da área
/// visível do ancestral não registram.
#[allow(clippy::too_many_arguments)]
fn collect_events_node(
    ui: &mut Ui,
    scale: f32,
    origin: Pos2,
    tree: &RenderTree,
    layout: &LayoutTable,
    state: &mut DenRouteState,
    node_idx: usize,
    parent_clip: Option<Rect>,
    events: &mut Vec<PaintEvent>,
) {
    let node = &tree.nodes[node_idx];
    let rect = scaled_rect(layout.rects[node.layout_index], origin, scale);

    // Hit-test acontece só dentro da área visível (overflow: hidden de ancestrais).
    let hit_rect = match parent_clip {
        Some(pc) => rect.intersect(pc),
        None => rect,
    };

    let sense = if node.interact.is_clickable() {
        Sense::click()
    } else {
        Sense::hover()
    };
    let id = Id::new(node.node_id.raw());
    let resp = ui.interact(hit_rect, id, sense);

    let hovering = resp.hovered();
    if hovering {
        state.hover_mut().insert(node.node_id);
    }

    // Cursor pointer precisa ler estilo ativo. Usa o mesmo critério do paint_node
    // (hover → hover_style.cursor_pointer OU interact.pointer_on_hover).
    if hovering {
        let active_style: &PaintStyle = node.hover_style.as_ref().unwrap_or(&node.style);
        if active_style.cursor_pointer || node.interact.pointer_on_hover {
            ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
        }
    }

    // Inputs: foco, teclado, InputChanged.
    if let RenderKind::Input { node_id, .. } = &node.kind {
        handle_input_events(ui, *node_id, &resp, state, events);
    }

    // Propaga clip pra filhos igual ao paint_node (overflow: hidden do pai).
    let active_style: &PaintStyle = if hovering {
        node.hover_style.as_ref().unwrap_or(&node.style)
    } else {
        &node.style
    };
    let child_clip = if active_style.overflow_hidden {
        Some(hit_rect)
    } else {
        parent_clip
    };

    // Recursão na MESMA ordem do paint_node — garante que hit-test de filhos
    // (chamadas mais tardias de ui.interact) sobrescreva a do pai quando
    // sobrepostos. Consistente com CSS pointer-events default.
    for child_idx in paint_order(tree, &node.children) {
        collect_events_node(
            ui,
            scale,
            origin,
            tree,
            layout,
            state,
            child_idx,
            child_clip,
            events,
        );
    }

    // Click events coletados DEPOIS dos filhos — se o clique "pertence" a um
    // filho, o resp.clicked() do filho retorna true; o do pai false. Egui
    // resolve isso via interact order automaticamente.
    if resp.clicked() {
        if let Some(h) = node.interact.click_handler {
            events.push(PaintEvent::Click { handler: h });
        } else if let Some(g) = node.interact.goto_slot {
            events.push(PaintEvent::Goto { slot: g });
        }
    }
}

/// Converte um `LayoutRect` (CSS px) pra `Rect` do egui em espaço de tela.
fn scaled_rect(r: LayoutRect, origin: Pos2, scale: f32) -> Rect {
    Rect::from_min_size(
        origin + Vec2::new(r.x * scale, r.y * scale),
        Vec2::new(r.width * scale, r.height * scale),
    )
}

/// Devolve os índices de filhos na ordem de paint:
/// 1. In-flow (não-positioned + relative) na ordem da tree.
/// 2. Positioned (`absolute`/`fixed`) ordenados por `z-index` ascendente; ties por tree order.
///
/// Mantém ordem da tree como tiebreak pra paint determinístico.
fn paint_order(tree: &RenderTree, children: &[usize]) -> Vec<usize> {
    let mut in_flow: Vec<usize> = Vec::with_capacity(children.len());
    let mut positioned: Vec<(i32, usize, usize)> = Vec::new(); // (z, tree_pos, idx)
    for (tree_pos, &child_idx) in children.iter().enumerate() {
        let pos = tree.nodes[child_idx].layout.position;
        if pos.is_out_of_flow() {
            let z = tree.nodes[child_idx].layout.z_index.unwrap_or(0);
            positioned.push((z, tree_pos, child_idx));
        } else {
            in_flow.push(child_idx);
        }
    }
    positioned.sort_by_key(|&(z, tree_pos, _)| (z, tree_pos));
    in_flow.extend(positioned.into_iter().map(|(_, _, idx)| idx));
    in_flow
}

/// Pinta o fundo do nó, respeitando border_radius.
fn paint_background(painter: &egui::Painter, rect: Rect, style: &PaintStyle, scale: f32) {
    let Some(bg) = style.background else {
        return;
    };
    let radius = style.border_radius * scale;
    painter.rect_filled(rect, radius, rgb_to_color(bg, style.opacity));
}

/// Pinta as sombras `box-shadow` externas (drop) do nó. Vão atrás do background.
///
/// Ordem CSS: a primeira sombra do `Vec` fica na FRENTE do stack visual; a
/// última fica no FUNDO. Pintamos da última pra primeira.
fn paint_drop_shadows(painter: &egui::Painter, rect: Rect, style: &PaintStyle, scale: f32) {
    for shadow in style.box_shadows.iter().rev() {
        if shadow.inset {
            continue;
        }
        paint_shadow_layer(painter, rect, style, shadow, scale);
    }
}

/// Pinta as sombras `box-shadow inset` (internas). Vão DEPOIS do background mas
/// antes do conteúdo.
///
/// MVP: aproximação por borda interna — usa `rect_stroke` com o `spread` como
/// largura da linha (mínimo `MIN_INSET_SHADOW_SPREAD_PX`), assim o desenho fica
/// nas bordas internas e NÃO ocupa o miolo do nó (rect_filled cobriria texto).
/// `blur` é ignorado nesta variante; gradiente direcional real saindo das
/// bordas pra dentro fica como melhoria futura — ndnm.scss não usa inset.
///
/// **Limitação intencional**: `spread` negativo é tratado como `0`. CSS spec diz
/// que negativo encolhe a sombra inset (cobre menos área). Como nesta aproximação
/// stub a sombra É um stroke nas bordas internas, "encolher" não tem semântica
/// visual coerente — desenhar com spread negativo expandiria o `inner` rect pra
/// FORA do nó e o stroke "inset" sairia das bordas externas. Documentado aqui;
/// quando o blur direcional real for implementado, o spread negativo passa a
/// fazer sentido (reduz a extensão do gradiente).
fn paint_inset_shadows(painter: &egui::Painter, rect: Rect, style: &PaintStyle, scale: f32) {
    for shadow in style.box_shadows.iter().rev() {
        if !shadow.inset {
            continue;
        }
        let radius = style.border_radius * scale;
        // `.max(0.0)` é deliberado — ver doc da função sobre spread negativo.
        let inner = rect
            .translate(egui::vec2(shadow.offset_x * scale, shadow.offset_y * scale))
            .shrink(shadow.spread.max(0.0) * scale);
        if inner.width() <= 0.0 || inner.height() <= 0.0 {
            continue;
        }
        // `spread` vira largura do stroke interno; piso `MIN_INSET_SHADOW_SPREAD_PX`
        // pra ser visível mesmo quando spread declarado for zero.
        let stroke_width =
            (shadow.spread.max(MIN_INSET_SHADOW_SPREAD_PX) * scale).max(MIN_BORDER_WIDTH_PX);
        painter.rect_stroke(
            inner,
            radius,
            egui::Stroke::new(stroke_width, rgb_to_color(shadow.color, style.opacity)),
            egui::epaint::StrokeKind::Inside,
        );
    }
}

/// Pinta uma única sombra drop (não-inset). Simula blur com `SHADOW_BLUR_SAMPLES`
/// retângulos concêntricos de alpha decrescente — egui não tem blur shader.
///
/// Resultado: rect denso no centro (a "sombra dura"), fade gradual nas bordas.
fn paint_shadow_layer(
    painter: &egui::Painter,
    rect: Rect,
    style: &PaintStyle,
    shadow: &den_layout::BoxShadow,
    scale: f32,
) {
    let radius = style.border_radius * scale;
    let base_rect = rect
        .translate(egui::vec2(shadow.offset_x * scale, shadow.offset_y * scale))
        .expand(shadow.spread * scale);

    // blur=0 → uma única camada nítida.
    if shadow.blur <= 0.0 {
        painter.rect_filled(base_rect, radius, rgb_to_color(shadow.color, style.opacity));
        return;
    }

    let scaled_blur = shadow.blur * scale;
    let layers = SHADOW_BLUR_SAMPLES.max(1);
    let step = scaled_blur / layers as f32;

    // Camadas pintadas OUTSIDE-IN: a externa cai primeiro (alpha mínimo),
    // depois cada interna por cima reforça o centro. O resultado visual:
    // borda esmaecida, núcleo opaco.
    for i in (0..layers).rev() {
        let expand = i as f32 * step;
        let layer_rect = base_rect.expand(expand);
        let alpha_factor = (1.0 - i as f32 * SHADOW_BLUR_ALPHA_DECAY).clamp(0.0, 1.0);
        let layer_color = scale_color_alpha(shadow.color, alpha_factor * style.opacity);
        painter.rect_filled(layer_rect, radius, layer_color);
    }
}

/// Multiplica o alpha de uma cor RGBA por um fator (0..=1) e devolve `Color32`.
/// Usado pelas camadas de blur do `box-shadow`.
fn scale_color_alpha(rgb: Rgb, factor: f32) -> Color32 {
    let (r, g, b, a) = rgb;
    let scaled = (a as f32 * factor.clamp(0.0, 1.0)).round().clamp(0.0, 255.0) as u8;
    Color32::from_rgba_unmultiplied(r, g, b, scaled)
}

/// Pinta a borda do nó por cima do conteúdo.
///
/// Se as 4 sides têm a mesma largura, usa `rect_stroke` (preserva
/// `border_radius` corretamente). Se assimétricas, desenha 4 line_segments
/// independentes — sides com largura `0` viram no-op.
fn paint_border(painter: &egui::Painter, rect: Rect, style: &PaintStyle, scale: f32) {
    let widths = style.border_widths;
    if widths.iter().all(|w| *w <= 0.0) {
        return;
    }
    let Some(color) = style.border_color else {
        return;
    };
    let stroke_color = rgb_to_color(color, style.opacity);

    // Caso uniforme: preserva o radius via rect_stroke.
    if widths[0] == widths[1] && widths[1] == widths[2] && widths[2] == widths[3] {
        let width = (widths[0] * scale).max(MIN_BORDER_WIDTH_PX);
        let radius = style.border_radius * scale;
        painter.rect_stroke(
            rect,
            radius,
            Stroke::new(width, stroke_color),
            StrokeKind::Inside,
        );
        return;
    }

    // Caso assimétrico: 4 line_segments. Border-radius é ignorado aqui
    // (cantos arredondados com per-side widths exigem mesh rendering).
    let scaled = [
        widths[0] * scale, // top
        widths[1] * scale, // right
        widths[2] * scale, // bottom
        widths[3] * scale, // left
    ];
    let min_w = MIN_BORDER_WIDTH_PX;
    let paint_side = |a: Pos2, b: Pos2, w: f32| {
        if w > 0.0 {
            painter.line_segment([a, b], Stroke::new(w.max(min_w), stroke_color));
        }
    };
    let top_y = rect.min.y + scaled[0] * 0.5;
    let bot_y = rect.max.y - scaled[2] * 0.5;
    let left_x = rect.min.x + scaled[3] * 0.5;
    let right_x = rect.max.x - scaled[1] * 0.5;
    paint_side(
        Pos2::new(rect.min.x, top_y),
        Pos2::new(rect.max.x, top_y),
        scaled[0],
    );
    paint_side(
        Pos2::new(right_x, rect.min.y),
        Pos2::new(right_x, rect.max.y),
        scaled[1],
    );
    paint_side(
        Pos2::new(rect.min.x, bot_y),
        Pos2::new(rect.max.x, bot_y),
        scaled[2],
    );
    paint_side(
        Pos2::new(left_x, rect.min.y),
        Pos2::new(left_x, rect.max.y),
        scaled[3],
    );
}

/// Pinta texto dentro do rect usando a caixa textual já medida pelo egui.
///
/// Respeita `style.transform`: quando há rotação, usa `TextShape::angle` do
/// egui pra rotacionar os glifos em volta do centro do rect (CSS default
/// `transform-origin: 50% 50%`), mantendo o texto coeso com o background
/// rotacionado pelo `paint_rotated_rect`.
fn paint_text(
    ui: &Ui,
    painter: &egui::Painter,
    rect: Rect,
    content: &str,
    heading: bool,
    style: &PaintStyle,
    scale: f32,
) {
    if content.is_empty() {
        return;
    }
    let color = style
        .color
        .map(|c| rgb_to_color(c, style.opacity))
        .unwrap_or(Color32::from_gray(220));
    let content = apply_text_transform(content, style.text_transform);
    let mut text_box = layout_text_box(ui, &content, heading, style, scale, color);
    // text-overflow: ellipsis — trunca e adiciona "…" quando não cabe no rect.
    if style.text_overflow_ellipsis && text_box.width > rect.width() {
        let truncated = fit_with_ellipsis(&content, rect.width(), |s| {
            layout_text_box(ui, s, heading, style, scale, color).width
        });
        text_box = layout_text_box(ui, &truncated, heading, style, scale, color);
    }
    let x = aligned_text_x(rect, text_box.width, style.text_align);
    let natural_pos = Pos2::new(x, rect.min.y);

    match style.transform {
        None => {
            painter.galley(natural_pos, text_box.galley, color);
        }
        Some(transform) => {
            // Rotaciona o ponto de ancoragem (natural_pos) em volta do centro do
            // rect. Como `TextShape::angle` rotaciona o galley em volta do próprio
            // `pos`, o resultado combinado é: texto girando em volta do centro do
            // rect (igual ao background rotacionado em `paint_rotated_rect`).
            let (sin, cos) = transform.rotation_rad.sin_cos();
            let pivot = rect.center();
            let rotated_pos = rotate_around(natural_pos, pivot, sin, cos);
            painter.add(
                egui::epaint::TextShape::new(rotated_pos, text_box.galley, color)
                    .with_angle(transform.rotation_rad),
            );
        }
    }
}

/// Trunca `text` por chars (do fim) até `text + "…"` caber em `max_width`.
/// `measure(s)` retorna a largura pintada de `s` no contexto atual.
/// Returns "…" se nem um único char cabe junto da elipse, ou se o input já é vazio.
fn fit_with_ellipsis(text: &str, max_width: f32, mut measure: impl FnMut(&str) -> f32) -> String {
    const ELLIPSIS: char = '…';
    if measure(text) <= max_width {
        return text.to_string();
    }
    let chars: Vec<char> = text.chars().collect();
    let mut buf = String::with_capacity(text.len() + ELLIPSIS.len_utf8());
    // Busca o maior prefixo que cabe quando "…" é apendado.
    for cut in (0..chars.len()).rev() {
        buf.clear();
        buf.extend(chars[..cut].iter());
        buf.push(ELLIPSIS);
        if measure(&buf) <= max_width {
            return buf;
        }
    }
    ELLIPSIS.to_string()
}

/// **Event pass pra inputs**: gerencia foco (click-to-focus, click-elsewhere-to-blur),
/// processa eventos de teclado quando focado, atualiza `state.inputs`/`state.cursor`,
/// e emite `PaintEvent::InputChanged` quando o valor muda.
///
/// Não desenha nada — o lado visual (texto + caret) vive em `paint_input_visual`,
/// que só lê o state atualizado aqui.
fn handle_input_events(
    ui: &Ui,
    node_id: DenNodeId,
    resp: &egui::Response,
    state: &mut DenRouteState,
    events: &mut Vec<PaintEvent>,
) {
    // Foco: click dentro foca; click fora quando já focado desfoca.
    if resp.clicked() {
        state.set_focus(Some(node_id));
        let len = state.inputs().get(node_id).map(|s| s.len()).unwrap_or(0);
        state.set_cursor(node_id, len);
    } else if resp.clicked_elsewhere() && state.focus() == Some(node_id) {
        state.set_focus(None);
        state.clear_cursor(node_id);
    }

    let focused = state.focus() == Some(node_id);

    // Valor corrente (source of truth é o route state).
    let value = state
        .inputs()
        .get(node_id)
        .map(|s| s.to_string())
        .unwrap_or_default();

    let mut cursor = clamp_char_boundary(
        &value,
        state
            .cursor_of(node_id)
            .unwrap_or(value.len())
            .min(value.len()),
    );

    let mut new_value = value.clone();
    let mut changed = false;
    if focused {
        let key_events = ui.ctx().input(|i| i.events.clone());
        for ev in key_events {
            match ev {
                egui::Event::Text(text) => {
                    let filtered: String = text.chars().filter(|c| !c.is_control()).collect();
                    if !filtered.is_empty() {
                        new_value.insert_str(cursor, &filtered);
                        cursor += filtered.len();
                        changed = true;
                    }
                }
                egui::Event::Key {
                    key: egui::Key::Backspace,
                    pressed: true,
                    ..
                } => {
                    if cursor > 0 {
                        let prev = prev_char_boundary(&new_value, cursor);
                        new_value.replace_range(prev..cursor, "");
                        cursor = prev;
                        changed = true;
                    }
                }
                egui::Event::Key {
                    key: egui::Key::Delete,
                    pressed: true,
                    ..
                } => {
                    if cursor < new_value.len() {
                        let next = next_char_boundary(&new_value, cursor);
                        new_value.replace_range(cursor..next, "");
                        changed = true;
                    }
                }
                egui::Event::Key {
                    key: egui::Key::ArrowLeft,
                    pressed: true,
                    ..
                } => {
                    cursor = prev_char_boundary(&new_value, cursor);
                }
                egui::Event::Key {
                    key: egui::Key::ArrowRight,
                    pressed: true,
                    ..
                } => {
                    cursor = next_char_boundary(&new_value, cursor);
                }
                egui::Event::Key {
                    key: egui::Key::Home,
                    pressed: true,
                    ..
                } => {
                    cursor = 0;
                }
                egui::Event::Key {
                    key: egui::Key::End,
                    pressed: true,
                    ..
                } => {
                    cursor = new_value.len();
                }
                egui::Event::Key {
                    key: egui::Key::Escape | egui::Key::Enter,
                    pressed: true,
                    ..
                } => {
                    state.set_focus(None);
                    state.clear_cursor(node_id);
                }
                _ => {}
            }
        }
    }

    cursor = clamp_char_boundary(&new_value, cursor.min(new_value.len()));
    state.set_cursor(node_id, cursor);

    if changed {
        state.inputs_mut().set(node_id, new_value.clone());
        events.push(PaintEvent::InputChanged {
            node_id,
            value: new_value,
        });
    }

    // Caret pisca continuamente quando focado → solicita repaint.
    if focused {
        ui.ctx().request_repaint();
    }
}

/// **Paint pass pra inputs**: desenha o texto (valor ou placeholder) e, se
/// focado, o caret piscante. Lê tudo de `state` — não muta nada, não emite
/// eventos. A lógica de teclado/foco fica em `handle_input_events`.
#[allow(clippy::too_many_arguments)]
fn paint_input_visual(
    ui: &Ui,
    painter: &egui::Painter,
    rect: Rect,
    node_id: DenNodeId,
    placeholder: Option<&str>,
    style: &PaintStyle,
    scale: f32,
    state: &DenRouteState,
) {
    let focused = state.focus() == Some(node_id);
    let display_value = state
        .inputs()
        .get(node_id)
        .map(|s| s.to_string())
        .unwrap_or_default();

    let (display_text, is_placeholder) = if display_value.is_empty() {
        (placeholder.unwrap_or("").to_string(), true)
    } else {
        (display_value.clone(), false)
    };

    let text_color = if is_placeholder {
        Color32::from_gray(130)
    } else {
        style
            .color
            .map(|c| rgb_to_color(c, style.opacity))
            .unwrap_or(Color32::from_gray(220))
    };
    let text_pos = rect.min + Vec2::new(INPUT_TEXT_PADDING_X * scale, INPUT_TEXT_PADDING_Y * scale);

    if !display_text.is_empty() {
        let painted_text = apply_text_transform(&display_text, style.text_transform);
        let text_box = layout_text_box(ui, &painted_text, false, style, scale, text_color);
        painter.galley(text_pos, text_box.galley, text_color);
    }

    if focused {
        let show_caret = ui.ctx().input(|i| (i.time % 1.0) < 0.5);
        if show_caret {
            let caret_color = style
                .color
                .map(|c| rgb_to_color(c, style.opacity))
                .unwrap_or(Color32::from_gray(220));
            let cursor_pos = state
                .cursor_of(node_id)
                .unwrap_or(display_value.len())
                .min(display_value.len());
            let cursor_safe = clamp_char_boundary(&display_value, cursor_pos);
            let pre = &display_value[..cursor_safe];
            let painted_pre = apply_text_transform(pre, style.text_transform);
            let pre_box = layout_text_box(ui, &painted_pre, false, style, scale, caret_color);
            let caret_x = text_pos.x + pre_box.width;
            let top = rect.min.y + INPUT_TEXT_PADDING_Y * scale;
            let bot = rect.max.y - INPUT_TEXT_PADDING_Y * scale;
            painter.line_segment(
                [Pos2::new(caret_x, top), Pos2::new(caret_x, bot)],
                Stroke::new((1.0 * scale).max(1.0), caret_color),
            );
        }
    }
}

fn layout_text_box(
    ui: &Ui,
    text: &str,
    heading: bool,
    style: &PaintStyle,
    scale: f32,
    color: Color32,
) -> TextBox {
    ui.fonts_mut(|fonts| {
        let available_families = fonts.families();
        let format = text_format_for_style(style, heading, scale, color, &available_families);
        let mut job = LayoutJob::single_section(text.to_string(), format);
        // Den ainda não faz wrap de texto; medir em uma linha replica o contrato atual.
        job.break_on_newline = false;
        job.halign = Align::LEFT;

        let galley = fonts.layout_job(job);
        TextBox {
            width: galley.rect.width(),
            height: galley.rect.height(),
            galley,
        }
    })
}

/// Constrói o formato textual do egui a partir do `PaintStyle` resolvido.
fn text_format_for_style(
    style: &PaintStyle,
    heading: bool,
    scale: f32,
    color: Color32,
    available_families: &[FontFamily],
) -> TextFormat {
    let base = if style.font_size > 0.0 {
        style.font_size
    } else if heading {
        20.0
    } else {
        14.0
    };
    let size = (base * scale).max(MIN_FONT_SIZE_PX);
    let decoration = Stroke::new((1.0 * scale).max(1.0), color);

    TextFormat {
        font_id: FontId::new(size, font_family_for_style(style, available_families)),
        extra_letter_spacing: style.letter_spacing * scale,
        line_height: line_height_for_style(style, base, scale),
        color,
        italics: style.font_italic,
        underline: if style.underline {
            decoration
        } else {
            Stroke::NONE
        },
        strikethrough: if style.strikethrough {
            decoration
        } else {
            Stroke::NONE
        },
        ..Default::default()
    }
}

/// Resolve `line-height` absoluto ou multiplicador para pontos na escala atual.
fn line_height_for_style(style: &PaintStyle, base_font_size: f32, scale: f32) -> Option<f32> {
    if style.line_height > 0.0 {
        Some(style.line_height * scale)
    } else if style.line_height_factor > 0.0 {
        Some(base_font_size * style.line_height_factor * scale)
    } else {
        None
    }
}

/// Escolhe a primeira família CSS disponível no egui, com fallback para genéricas.
fn font_family_for_style(style: &PaintStyle, available_families: &[FontFamily]) -> FontFamily {
    let Some(stack) = style.font_family else {
        return FontFamily::Proportional;
    };

    for requested in css_font_family_stack(stack) {
        let normalized = requested.to_ascii_lowercase();
        match normalized.as_str() {
            "monospace" | "ui-monospace" => return FontFamily::Monospace,
            "serif" | "sans-serif" | "system-ui" | "ui-sans-serif" | "cursive" | "fantasy" => {
                return FontFamily::Proportional;
            }
            _ => {
                if let Some(found) = find_registered_font_family(&requested, available_families) {
                    return found;
                }
            }
        }
    }

    FontFamily::Proportional
}

/// Busca família registrada pelo app usando comparação case-insensitive.
fn find_registered_font_family(
    requested: &str,
    available_families: &[FontFamily],
) -> Option<FontFamily> {
    available_families.iter().find_map(|family| {
        if let FontFamily::Name(name) = family
            && name.eq_ignore_ascii_case(requested)
        {
            return Some(family.clone());
        }
        None
    })
}

/// Divide uma pilha CSS de fontes, respeitando nomes entre aspas.
fn css_font_family_stack(stack: &str) -> Vec<String> {
    let mut families = Vec::new();
    let mut current = String::new();
    let mut quote: Option<char> = None;

    for ch in stack.chars() {
        match quote {
            Some(q) => {
                if ch == q {
                    quote = None;
                } else {
                    current.push(ch);
                }
            }
            None => match ch {
                '"' | '\'' => quote = Some(ch),
                ',' => {
                    push_css_font_family(&mut families, &current);
                    current.clear();
                }
                _ => current.push(ch),
            },
        }
    }
    push_css_font_family(&mut families, &current);
    families
}

/// Adiciona uma família não vazia à pilha já normalizada.
fn push_css_font_family(families: &mut Vec<String>, family: &str) {
    let family = family.trim();
    if !family.is_empty() {
        families.push(family.to_string());
    }
}

/// Aplica `text-transform` antes de medir e pintar o texto.
fn apply_text_transform(text: &str, transform: TextTransform) -> String {
    match transform {
        TextTransform::None => text.to_string(),
        TextTransform::Uppercase => text.to_uppercase(),
        TextTransform::Lowercase => text.to_lowercase(),
        TextTransform::Capitalize => capitalize_text(text),
    }
}

/// Capitaliza a primeira letra de cada palavra, preservando o resto do texto.
fn capitalize_text(text: &str) -> String {
    let mut result = String::new();
    let mut start_of_word = true;
    for ch in text.chars() {
        if ch.is_alphanumeric() {
            if start_of_word {
                result.extend(ch.to_uppercase());
                start_of_word = false;
            } else {
                result.push(ch);
            }
        } else {
            result.push(ch);
            start_of_word = true;
        }
    }
    result
}

/// Calcula a posição X do texto dentro do retângulo do nó.
fn aligned_text_x(rect: Rect, text_width: f32, align: TextAlign) -> f32 {
    match align {
        TextAlign::Left => rect.min.x,
        TextAlign::Center => rect.min.x + (rect.width() - text_width) / 2.0,
        TextAlign::Right => rect.max.x - text_width,
    }
}

/// Ajusta um offset para o boundary UTF-8 anterior, se necessário.
fn clamp_char_boundary(s: &str, offset: usize) -> usize {
    let mut i = offset.min(s.len());
    while i > 0 && !s.is_char_boundary(i) {
        i -= 1;
    }
    i
}

/// Retorna o byte offset do caractere anterior ao offset dado (UTF-8 safe).
fn prev_char_boundary(s: &str, offset: usize) -> usize {
    if offset == 0 {
        return 0;
    }
    let mut i = offset - 1;
    while i > 0 && !s.is_char_boundary(i) {
        i -= 1;
    }
    i
}

/// Retorna o byte offset do próximo caractere após o offset dado (UTF-8 safe).
fn next_char_boundary(s: &str, offset: usize) -> usize {
    let len = s.len();
    if offset >= len {
        return len;
    }
    let mut i = offset + 1;
    while i < len && !s.is_char_boundary(i) {
        i += 1;
    }
    i
}

/// Conversão RGBA → Color32 respeitando `opacity` do `PaintStyle`.
///
/// Alpha final = `rgb.a * opacity` (clamped 0..=255). `opacity = 1.0` preserva
/// o alpha original da cor; `opacity = 0.0` zera tudo (invisível).
fn rgb_to_color(rgb: Rgb, opacity: f32) -> Color32 {
    let (r, g, b, a) = rgb;
    let effective_a = (a as f32 * opacity.clamp(0.0, 1.0)).round().clamp(0.0, 255.0) as u8;
    Color32::from_rgba_unmultiplied(r, g, b, effective_a)
}

#[cfg(test)]
mod tests {
    use super::{
        apply_text_transform, clamp_char_boundary, css_font_family_stack, fit_with_ellipsis,
        next_char_boundary, paint_order, prev_char_boundary, rgb_to_color,
    };
    use den_layout::TextTransform;
    use eframe::egui::Color32;

    #[test]
    fn rgb_to_color_multiplies_alpha_by_opacity() {
        // Cor opaca a 50% opacity → alpha 128.
        let c = rgb_to_color((255, 0, 0, 255), 0.5);
        assert_eq!(c, Color32::from_rgba_unmultiplied(255, 0, 0, 128));
        // Cor já translúcida (alpha 100) a 50% → 50.
        let c = rgb_to_color((0, 0, 0, 100), 0.5);
        assert_eq!(c, Color32::from_rgba_unmultiplied(0, 0, 0, 50));
        // opacity = 1.0 preserva alpha original.
        let c = rgb_to_color((10, 20, 30, 200), 1.0);
        assert_eq!(c, Color32::from_rgba_unmultiplied(10, 20, 30, 200));
        // opacity > 1 é clampado em 1.
        let c = rgb_to_color((10, 20, 30, 255), 5.0);
        assert_eq!(c.a(), 255);
    }

    #[test]
    fn fit_with_ellipsis_returns_input_when_already_fits() {
        // Fake measure: 1 unit per char.
        let measure = |s: &str| s.chars().count() as f32;
        assert_eq!(fit_with_ellipsis("abc", 10.0, measure), "abc");
        assert_eq!(fit_with_ellipsis("abc", 3.0, measure), "abc");
    }

    #[test]
    fn fit_with_ellipsis_truncates_when_needed() {
        // 1 unit per char. "…" também conta como 1.
        let measure = |s: &str| s.chars().count() as f32;
        // "abcdef" tem 6 chars; max_width=4 → "abc…" (3 + 1 elipse = 4).
        assert_eq!(fit_with_ellipsis("abcdef", 4.0, measure), "abc…");
        // max_width=1 → só a elipse.
        assert_eq!(fit_with_ellipsis("abcdef", 1.0, measure), "…");
    }

    #[test]
    fn clamps_cursor_to_previous_utf8_boundary() {
        let value = "açb";

        assert_eq!(clamp_char_boundary(value, 2), 1);
        assert_eq!(clamp_char_boundary(value, value.len() + 8), value.len());
    }

    #[test]
    fn moves_cursor_across_utf8_chars() {
        let value = "açb";

        assert_eq!(next_char_boundary(value, 1), 3);
        assert_eq!(prev_char_boundary(value, 3), 1);
    }

    #[test]
    fn splits_css_font_family_stack_with_quoted_names() {
        assert_eq!(
            css_font_family_stack(r#""Inter Tight", Arial, sans-serif"#),
            vec!["Inter Tight", "Arial", "sans-serif"]
        );
    }

    #[test]
    fn applies_text_transform_before_measurement() {
        assert_eq!(
            apply_text_transform("olá den", TextTransform::Uppercase),
            "OLÁ DEN"
        );
        assert_eq!(
            apply_text_transform("olá den", TextTransform::Capitalize),
            "Olá Den"
        );
    }

    // ---- paint_order: in-flow first, then positioned by z-index ----

    use den_layout::{
        DenNodeId, Interact, LayoutIntent, PaintStyle, PositionKind, RenderKind, RenderNode,
        RenderTree,
    };

    fn child(idx: u64, position: PositionKind, z_index: Option<i32>) -> RenderNode {
        let mut node = RenderNode::new(DenNodeId::new(idx), idx as usize, RenderKind::Container);
        node.layout = LayoutIntent {
            position,
            z_index,
            ..LayoutIntent::default()
        };
        node.style = PaintStyle::default();
        node.interact = Interact::default();
        node
    }

    fn tree_with_children(children: Vec<RenderNode>) -> RenderTree {
        let mut tree = RenderTree::new();
        for c in children {
            tree.push(c);
        }
        tree
    }

    #[test]
    fn paint_order_keeps_inflow_before_positioned() {
        // tree-order: [absolute, static, absolute, static]
        // expected:    [static, static, absolute, absolute] (preservando ordem da tree em cada grupo)
        let tree = tree_with_children(vec![
            child(1, PositionKind::Absolute, None),
            child(2, PositionKind::Static, None),
            child(3, PositionKind::Absolute, None),
            child(4, PositionKind::Static, None),
        ]);
        let order = paint_order(&tree, &[0, 1, 2, 3]);
        assert_eq!(order, vec![1, 3, 0, 2]);
    }

    #[test]
    fn paint_order_sorts_positioned_by_z_index_ascending() {
        // tree-order: z=5, z=1, z=3 → expected paint: z=1, z=3, z=5
        let tree = tree_with_children(vec![
            child(1, PositionKind::Absolute, Some(5)),
            child(2, PositionKind::Absolute, Some(1)),
            child(3, PositionKind::Absolute, Some(3)),
        ]);
        let order = paint_order(&tree, &[0, 1, 2]);
        assert_eq!(order, vec![1, 2, 0]);
    }

    #[test]
    fn paint_order_default_z_index_is_zero() {
        // z=None equivale a z=0; ties por tree order.
        let tree = tree_with_children(vec![
            child(1, PositionKind::Absolute, Some(-1)),
            child(2, PositionKind::Absolute, None), // z=0
            child(3, PositionKind::Absolute, Some(1)),
        ]);
        let order = paint_order(&tree, &[0, 1, 2]);
        assert_eq!(order, vec![0, 1, 2]);
    }

    #[test]
    fn paint_order_z_index_ties_break_by_tree_order() {
        // dois com z=2: ordem da tree (idx 0, idx 2) vence.
        let tree = tree_with_children(vec![
            child(1, PositionKind::Absolute, Some(2)),
            child(2, PositionKind::Absolute, Some(5)),
            child(3, PositionKind::Absolute, Some(2)),
        ]);
        let order = paint_order(&tree, &[0, 1, 2]);
        assert_eq!(order, vec![0, 2, 1]);
    }

    #[test]
    fn paint_order_relative_treats_as_inflow() {
        // Relative NÃO é out_of_flow — pinta junto com static, antes de absolute/fixed.
        let tree = tree_with_children(vec![
            child(1, PositionKind::Relative, None),
            child(2, PositionKind::Absolute, None),
            child(3, PositionKind::Static, None),
        ]);
        let order = paint_order(&tree, &[0, 1, 2]);
        assert_eq!(order, vec![0, 2, 1]);
    }

    #[test]
    fn paint_order_fixed_orders_with_absolute() {
        // Fixed e absolute compartilham a mesma camada de paint (ambos out_of_flow).
        let tree = tree_with_children(vec![
            child(1, PositionKind::Fixed, Some(10)),
            child(2, PositionKind::Absolute, Some(1)),
        ]);
        let order = paint_order(&tree, &[0, 1]);
        assert_eq!(order, vec![1, 0]);
    }
}
