//! Painter Den para o backend egui.
//!
//! Esta é a ÚNICA ponte entre Den e egui pra renderização de páginas. Recebe
//! uma `RenderTree` com valores já resolvidos e uma `LayoutTable` com rects
//! calculados em CSS pixels. Desenha cada nó via `egui::Painter` (rect_filled,
//! galley, rect_stroke) — nada de widgets egui.
//!
//! Retorna `PaintEvent`s que o código gerado pelo `den_template!` despacha
//! pros handlers (`(click)`, `goto`, `InputChanged` pra two-way binding).

use den_layout::{
    DenNodeId, DenRouteState, LayoutRect, LayoutTable, PaintStyle, RenderKind, RenderNode,
    RenderTree, Rgb,
};
use eframe::egui::{self, Color32, FontId, Id, Pos2, Rect, Sense, Stroke, StrokeKind, Ui, Vec2};

/// Tamanho mínimo de fonte antes de desenhar (evita fontes impossíveis de ler).
const MIN_FONT_SIZE_PX: f32 = 6.0;

/// Largura mínima de borda em px de tela (evita borda sumir em scale baixo).
const MIN_BORDER_WIDTH_PX: f32 = 1.0;

/// Padding interno mínimo (CSS px) pra não encostar texto na borda do input.
const INPUT_TEXT_PADDING_X: f32 = 6.0;
const INPUT_TEXT_PADDING_Y: f32 = 4.0;

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

/// Mede texto, resolve layout e pinta a árvore — pipeline completo Den.
///
/// Recebe a tree construída pelo macro, faz a medição intrínseca via
/// `ui.fonts_mut`, repopula a `LayoutTable` com as entradas da tree, resolve
/// rects no viewport atual, e finalmente pinta no `egui::Painter`.
///
/// Retorna os eventos coletados (cliques, mudanças de input, goto).
pub fn paint_tree(
    ui: &mut Ui,
    scale: f32,
    tree: &mut RenderTree,
    layout: &mut LayoutTable,
    state: &mut DenRouteState,
) -> Vec<PaintEvent> {
    // 1. Medição intrínseca via egui fonts (substitui estimativa compile-time).
    measure_tree_text(ui, tree);

    // 2. Popula LayoutTable a partir da tree e reajusta sizes/rects.
    layout.entries = tree.to_layout_entries();
    let len = layout.entries.len();
    if layout.sizes.len() != len {
        layout.sizes.clear();
        layout.sizes.resize(len, None);
        layout.rects.clear();
        layout.rects.resize(len, LayoutRect::default());
    }

    // 3. Resolve no viewport atual, em CSS pixels.
    let viewport_w = ui.available_width() / scale;
    let viewport_h = ui.available_height() / scale;
    layout.resolve_in_viewport(viewport_w, viewport_h);

    // 4. Prepara origem + reseta hover antes do walk.
    let origin = ui.min_rect().min;
    let mut events = Vec::new();

    let body_rect = scaled_rect(
        layout.rects.first().copied().unwrap_or_default(),
        origin,
        scale,
    );
    ui.allocate_rect(body_rect, Sense::hover());

    // Pinta o body (seletor `body` no SCSS) antes dos filhos. Equivalente ao
    // <body> do browser — define background, border, etc. do viewport inteiro.
    if let Some(body_style) = &tree.body_style {
        let body_painter = ui.painter_at(body_rect);
        paint_background(&body_painter, body_rect, body_style, scale);
        paint_border(&body_painter, body_rect, body_style, scale);
    }

    state.hover_mut().clear();

    // 5. Walk + paint.
    for &root_idx in &tree.roots {
        paint_node(
            ui,
            scale,
            origin,
            tree,
            layout,
            state,
            root_idx,
            &mut events,
        );
    }

    events
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
                let base = if node.style.font_size > 0.0 {
                    node.style.font_size
                } else if *heading {
                    20.0
                } else {
                    14.0
                };
                let font = FontId::proportional(base);
                let galley =
                    ui.fonts_mut(|f| f.layout_no_wrap(content.clone(), font, Color32::WHITE));
                node.layout.intrinsic_width = galley.rect.width();
                node.layout.intrinsic_height = galley.rect.height();
            }
            RenderKind::Input { .. } => {
                // Input: uma linha de texto (font_size ou default 14).
                let base = if node.style.font_size > 0.0 {
                    node.style.font_size
                } else {
                    14.0
                };
                node.layout.intrinsic_height = base;
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

/// Desenha um único nó da `RenderTree` e recursivamente seus filhos.
#[allow(clippy::too_many_arguments)]
fn paint_node(
    ui: &mut Ui,
    scale: f32,
    origin: Pos2,
    tree: &RenderTree,
    layout: &LayoutTable,
    state: &mut DenRouteState,
    node_idx: usize,
    events: &mut Vec<PaintEvent>,
) {
    let node = &tree.nodes[node_idx];
    let rect = scaled_rect(layout.rects[node.layout_index], origin, scale);

    // Interação primeiro — precisa saber se tá em hover antes de escolher o estilo.
    let sense = if node.interact.is_clickable() {
        Sense::click()
    } else {
        Sense::hover()
    };
    let id = Id::new(node.node_id.raw());
    let resp = ui.interact(rect, id, sense);

    let hovering = resp.hovered();
    if hovering {
        state.hover_mut().insert(node.node_id);
    }

    let active_style: &PaintStyle = if hovering {
        node.hover_style.as_ref().unwrap_or(&node.style)
    } else {
        &node.style
    };

    // Cursor pointer: seja vindo do hover_style (cursor: pointer dentro do :hover)
    // seja explícito por ser clicável (goto, click com styling do pointer).
    if hovering && (active_style.cursor_pointer || node.interact.pointer_on_hover) {
        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
    }

    // Pinta backgrounds, bordas, conteúdo via painter com clip no próprio rect
    // (evita texto vazar pra fora do nó).
    let painter = ui.painter_at(rect);
    paint_background(&painter, rect, active_style, scale);

    match &node.kind {
        RenderKind::Container => {
            // Só fundo + borda; filhos vêm na recursão.
        }
        RenderKind::Text { content, heading } => {
            paint_text(ui, &painter, rect, content, *heading, active_style, scale);
        }
        RenderKind::Input {
            node_id,
            placeholder,
        } => {
            paint_input(
                ui,
                &painter,
                rect,
                *node_id,
                placeholder.as_deref(),
                active_style,
                scale,
                &resp,
                state,
                events,
            );
        }
    }

    paint_border(&painter, rect, active_style, scale);

    // Filhos primeiro, depois eventos — garante que o clique no pai vença ordem
    // visual de desenho (filhos por cima), mas eventos de filho já estão na lista.
    for &child_idx in &node.children {
        paint_node(ui, scale, origin, tree, layout, state, child_idx, events);
    }

    if resp.clicked() {
        if let Some(h) = node.interact.click_handler {
            events.push(PaintEvent::Click { handler: h });
        } else if let Some(g) = node.interact.goto_slot {
            events.push(PaintEvent::Goto { slot: g });
        }
    }

    let _ = (node, events); // suppress warnings while features incomplete
    let _ = RenderNode::new; // força referência ao construtor; silencia dead_code se não usado
}

/// Converte um `LayoutRect` (CSS px) pra `Rect` do egui em espaço de tela.
fn scaled_rect(r: LayoutRect, origin: Pos2, scale: f32) -> Rect {
    Rect::from_min_size(
        origin + Vec2::new(r.x * scale, r.y * scale),
        Vec2::new(r.width * scale, r.height * scale),
    )
}

/// Pinta o fundo do nó, respeitando border_radius.
fn paint_background(painter: &egui::Painter, rect: Rect, style: &PaintStyle, scale: f32) {
    let Some(bg) = style.background else {
        return;
    };
    let radius = style.border_radius * scale;
    painter.rect_filled(rect, radius, rgb_to_color(bg));
}

/// Pinta a borda do nó por cima do conteúdo.
fn paint_border(painter: &egui::Painter, rect: Rect, style: &PaintStyle, scale: f32) {
    if style.border_width <= 0.0 {
        return;
    }
    let Some(color) = style.border_color else {
        return;
    };
    let width = (style.border_width * scale).max(MIN_BORDER_WIDTH_PX);
    let radius = style.border_radius * scale;
    painter.rect_stroke(
        rect,
        radius,
        Stroke::new(width, rgb_to_color(color)),
        StrokeKind::Inside,
    );
}

/// Pinta texto dentro do rect usando a fonte proporcional do egui.
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
    let base = if style.font_size > 0.0 {
        style.font_size
    } else if heading {
        20.0
    } else {
        14.0
    };
    let size = (base * scale).max(MIN_FONT_SIZE_PX);
    let color = style
        .color
        .map(rgb_to_color)
        .unwrap_or(Color32::from_gray(220));

    // `layout_no_wrap` mantém o texto numa linha só; quebras de linha viram
    // no próximo passo quando tivermos wrap de texto na tree.
    let font = FontId::proportional(size);
    let galley = ui.fonts_mut(|f| f.layout_no_wrap(content.to_string(), font, color));
    painter.galley(rect.min, galley, color);
}

/// Pinta um input: fundo (desenhado pelo caller), texto ou placeholder, e caret piscante
/// quando o nó está focado. Também consome eventos de teclado em modo focado e emite
/// `PaintEvent::InputChanged` quando o valor muda.
#[allow(clippy::too_many_arguments)]
fn paint_input(
    ui: &Ui,
    painter: &egui::Painter,
    rect: Rect,
    node_id: DenNodeId,
    placeholder: Option<&str>,
    style: &PaintStyle,
    scale: f32,
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

    // Posição do caret (mantida entre frames).
    let mut cursor = state
        .cursor_of(node_id)
        .unwrap_or(value.len())
        .min(value.len());

    // Processa eventos de teclado quando focado. Emite InputChanged se o valor muda.
    let mut new_value = value.clone();
    let mut changed = false;
    if focused {
        let key_events = ui.ctx().input(|i| i.events.clone());
        for ev in key_events {
            match ev {
                egui::Event::Text(text) => {
                    // Filtra caracteres de controle que não queremos.
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

    // Persiste a posição do caret clampada ao novo valor.
    cursor = cursor.min(new_value.len());
    state.set_cursor(node_id, cursor);

    if changed {
        // Atualiza route state e emite o evento pra dispatch (mirror pro self.field).
        state.inputs_mut().set(node_id, new_value.clone());
        events.push(PaintEvent::InputChanged {
            node_id,
            value: new_value.clone(),
        });
    }

    // Texto a pintar: valor ou placeholder em cinza.
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

    let base_font = if style.font_size > 0.0 {
        style.font_size
    } else {
        14.0
    };
    let size = (base_font * scale).max(MIN_FONT_SIZE_PX);
    let text_color = if is_placeholder {
        Color32::from_gray(130)
    } else {
        style
            .color
            .map(rgb_to_color)
            .unwrap_or(Color32::from_gray(220))
    };
    let font = FontId::proportional(size);
    let text_pos = rect.min + Vec2::new(INPUT_TEXT_PADDING_X * scale, INPUT_TEXT_PADDING_Y * scale);

    if !display_text.is_empty() {
        let galley =
            ui.fonts_mut(|f| f.layout_no_wrap(display_text.clone(), font.clone(), text_color));
        painter.galley(text_pos, galley, text_color);
    }

    // Caret pisca quando focado.
    if focused {
        let show_caret = ui.ctx().input(|i| (i.time % 1.0) < 0.5);
        if show_caret {
            let caret_color = style
                .color
                .map(rgb_to_color)
                .unwrap_or(Color32::from_gray(220));
            let pre = &display_value[..cursor.min(display_value.len())];
            let pre_galley = ui.fonts_mut(|f| f.layout_no_wrap(pre.to_string(), font, caret_color));
            let caret_x = text_pos.x + pre_galley.rect.width();
            let top = rect.min.y + INPUT_TEXT_PADDING_Y * scale;
            let bot = rect.max.y - INPUT_TEXT_PADDING_Y * scale;
            painter.line_segment(
                [Pos2::new(caret_x, top), Pos2::new(caret_x, bot)],
                Stroke::new((1.0 * scale).max(1.0), caret_color),
            );
        }
        // Solicita repaint contínuo pra piscar o caret.
        ui.ctx().request_repaint();
    }
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

/// Conversão RGB → Color32 opaco.
fn rgb_to_color(rgb: Rgb) -> Color32 {
    Color32::from_rgb(rgb.0, rgb.1, rgb.2)
}
