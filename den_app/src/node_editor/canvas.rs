//! Canvas principal: render, drag, wire drag, hit testing, pan e grid.

use eframe::egui;
use super::{theme, types::*, node, wire};

/// Mapeia `PortType` para a cor de wire/highlight correspondente.
fn port_type_color(pt: PortType) -> egui::Color32 {
    match pt {
        PortType::Exec => theme::WIRE_EXEC,
        PortType::Data => theme::WIRE_DATA,
        PortType::Input => theme::PORT_INPUT,
        PortType::Output => theme::PORT_OUTPUT,
    }
}

pub struct NodeEditorCanvas {
    pub nodes: Vec<NodeData>,
    pub wires: Vec<WireData>,
    pub scale: f32,
    pub drag: Option<DragState>,
    pub wire_drag: Option<WireDragState>,
    /// Offset do viewport em CSS pixels (unscaled). Alterado via middle-click drag
    /// ou scroll sem Ctrl. Positivo = conteúdo moveu pra direita/baixo.
    pub pan_offset: egui::Vec2,
}

impl NodeEditorCanvas {
    /// Entry point do node editor: orquestra background, grid, wires, nodes e drag.
    /// Wires são renderizados antes dos nodes (z-order). O node arrastado
    /// renderiza por último (topo). Wire preview e snap highlight renderizam no final.
    ///
    /// # Sistema de coordenadas
    /// - **CSS pixels**: unscaled. Posições dos nodes (`node.x`, `node.y`), ports e hit tests.
    /// - **Screen pixels**: o que o egui vê. `css_pos * scale + effective_origin`.
    /// - **effective_origin**: posição screen do CSS pixel (0, 0). = `canvas_rect.min + pan_offset * scale`.
    /// - **Mouse → CSS**: `(mouse - effective_origin) / scale`.
    pub fn render(&mut self, ui: &mut egui::Ui) {
        let (response, painter) = ui.allocate_painter(
            ui.available_size(),
            egui::Sense::click_and_drag(),
        );
        let canvas_rect = response.rect;
        let s = self.scale;

        // Scroll sem Ctrl → pan vertical/horizontal (Ctrl+scroll é zoom, tratado em main.rs)
        let scroll = ui.ctx().input(|i| {
            if !i.modifiers.ctrl { i.raw_scroll_delta } else { egui::Vec2::ZERO }
        });
        if scroll != egui::Vec2::ZERO {
            self.pan_offset.x += scroll.x / s;
            self.pan_offset.y += scroll.y / s;
        }

        // Posição screen do CSS pixel (0, 0) — incorpora pan_offset
        let effective_origin = egui::pos2(
            canvas_rect.min.x + self.pan_offset.x * s,
            canvas_rect.min.y + self.pan_offset.y * s,
        );

        // Drag handling (antes do render pra posições estarem atualizadas)
        self.handle_drag(&response, effective_origin);

        // 1. Background
        painter.rect_filled(canvas_rect, 0.0, theme::BG);

        // 2. Grid dots (offset pelo pan)
        self.draw_grid(&painter, canvas_rect, effective_origin);

        // 3. Wires (behind nodes)
        for w in &self.wires {
            wire::draw_wire(&painter, w, &self.nodes, s, effective_origin);
        }

        // 4. Nodes — node arrastado renderiza por último (z-order topo)
        let dragged_id = self.drag.as_ref().map(|d| d.node_id.clone());
        for n in &self.nodes {
            if Some(&n.id) != dragged_id.as_ref() {
                node::draw_node(&painter, n, s, effective_origin);
            }
        }
        if let Some(ref drag_id) = dragged_id {
            if let Some(n) = self.nodes.iter().find(|n| &n.id == drag_id) {
                node::draw_node(&painter, n, s, effective_origin);
            }
        }

        // 5. Wire drag preview (bezier temporária port → cursor)
        if let Some(ref wd) = self.wire_drag {
            if let Some(mouse_pos) = response.interact_pointer_pos() {
                self.draw_wire_preview(&painter, wd, mouse_pos, effective_origin);

                // 6. Highlight do port alvo quando compatível
                let cx = (mouse_pos.x - effective_origin.x) / s;
                let cy = (mouse_pos.y - effective_origin.y) / s;
                if let Some((node_id, port_name, port_type)) = self.hit_test_input_port(cx, cy) {
                    if port_type == wd.port_type && node_id != wd.from_node_id {
                        if let Some(node) = self.nodes.iter().find(|n| n.id == node_id) {
                            if let Some(port_pos) = wire::get_port_position(
                                node, &port_name, false, s, effective_origin,
                            ) {
                                let wire_color = port_type_color(wd.port_type);
                                painter.circle_stroke(
                                    port_pos,
                                    theme::PORT_SNAP_HIGHLIGHT_RADIUS * s,
                                    egui::Stroke::new(
                                        theme::PORT_SNAP_HIGHLIGHT_STROKE,
                                        wire_color.linear_multiply(theme::PORT_SNAP_HIGHLIGHT_OPACITY),
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// State machine completa de drag. Prioridades no `drag_started`:
    /// 0 = input port com wire existente → desconecta e inicia drag reverso do output original;
    /// 1 = output port → inicia wire drag;
    /// 2 = node body → inicia node drag.
    /// No `drag_stopped`: tenta snap em input port compatível e cria `WireData` se válido.
    /// `effective_origin` é a posição screen do CSS pixel (0,0), incorporando pan_offset.
    fn handle_drag(&mut self, response: &egui::Response, effective_origin: egui::Pos2) {
        let s = self.scale;

        if response.drag_started() {
            if let Some(mouse_pos) = response.interact_pointer_pos() {
                let canvas_x = (mouse_pos.x - effective_origin.x) / s;
                let canvas_y = (mouse_pos.y - effective_origin.y) / s;

                // PRIORIDADE 0: input port com wire → desconecta e inicia drag reverso
                if let Some((node_id, port_name, _)) = self.hit_test_input_port(canvas_x, canvas_y) {
                    if let Some(wire_idx) = self.wires.iter().position(|w| {
                        w.to_node == node_id && w.to_port == port_name
                    }) {
                        let removed = self.wires.remove(wire_idx);
                        self.wire_drag = Some(WireDragState {
                            from_node_id: removed.from_node,
                            from_port_name: removed.from_port,
                            port_type: removed.wire_type,
                        });
                        return;
                    }
                }

                // PRIORIDADE 1: output port → inicia wire drag
                if let Some((node_id, port_name, port_type)) =
                    self.hit_test_output_port(canvas_x, canvas_y)
                {
                    self.wire_drag = Some(WireDragState {
                        from_node_id: node_id,
                        from_port_name: port_name,
                        port_type,
                    });
                    return;
                }

                // PRIORIDADE 2: node body → inicia node drag
                if let Some(node) = self.hit_test(canvas_x, canvas_y) {
                    let offset = egui::vec2(canvas_x - node.x, canvas_y - node.y);
                    self.drag = Some(DragState {
                        node_id: node.id.clone(),
                        offset,
                    });
                }
            }
        }

        // Drag move: node drag OU pan do canvas (fundo vazio)
        if response.dragged() && self.wire_drag.is_none() {
            if let Some(ref drag) = self.drag {
                // Node drag
                if let Some(mouse_pos) = response.interact_pointer_pos() {
                    let canvas_x = (mouse_pos.x - effective_origin.x) / s;
                    let canvas_y = (mouse_pos.y - effective_origin.y) / s;
                    let new_x = canvas_x - drag.offset.x;
                    let new_y = canvas_y - drag.offset.y;
                    let node_id = drag.node_id.clone();
                    if let Some(node) = self.nodes.iter_mut().find(|n| n.id == node_id) {
                        node.x = new_x;
                        node.y = new_y;
                    }
                }
            } else {
                // Fundo vazio → pan
                let delta = response.drag_delta();
                self.pan_offset.x += delta.x / s;
                self.pan_offset.y += delta.y / s;
            }
        }

        if response.drag_stopped() {
            if let Some(wd) = self.wire_drag.take() {
                if let Some(mouse_pos) = response.interact_pointer_pos() {
                    let cx = (mouse_pos.x - effective_origin.x) / s;
                    let cy = (mouse_pos.y - effective_origin.y) / s;
                    if let Some((to_node, to_port, to_type)) = self.hit_test_input_port(cx, cy) {
                        let valid = to_type == wd.port_type
                            && to_node != wd.from_node_id
                            && !self.wire_exists(&wd.from_node_id, &wd.from_port_name, &to_node, &to_port);
                        if valid {
                            self.wires.push(WireData {
                                from_node: wd.from_node_id,
                                from_port: wd.from_port_name,
                                to_node,
                                to_port,
                                wire_type: wd.port_type,
                            });
                        }
                    }
                }
            }
            self.drag = None;
        }
    }

    /// Hit test: encontra o node mais "acima" (último no Vec = z-order topo)
    /// que contém o ponto (x, y) em CSS pixels (unscaled).
    fn hit_test(&self, x: f32, y: f32) -> Option<&NodeData> {
        for node in self.nodes.iter().rev() {
            let node_h = node::calculate_node_height(node);
            let node_rect = egui::Rect::from_min_size(
                egui::pos2(node.x, node.y),
                egui::vec2(theme::NODE_WIDTH, node_h),
            );
            if node_rect.contains(egui::pos2(x, y)) {
                return Some(node);
            }
        }
        None
    }

    /// Checa se (x, y) em CSS pixels está sobre algum input port.
    /// Retorna (node_id, port_name, port_type) se encontrou.
    fn hit_test_input_port(&self, x: f32, y: f32) -> Option<(String, String, PortType)> {
        let hit_r_sq = theme::PORT_HIT_RADIUS * theme::PORT_HIT_RADIUS;
        for node in self.nodes.iter().rev() {
            for (i, port) in node.inputs.iter().enumerate() {
                let port_x = node.x;
                let port_y = node.y
                    + theme::HEADER_HEIGHT
                    + theme::BODY_PAD_TOP
                    + i as f32 * theme::PORT_ROW_HEIGHT
                    + theme::PORT_ROW_HEIGHT / 2.0;
                let dx = x - port_x;
                let dy = y - port_y;
                if dx * dx + dy * dy <= hit_r_sq {
                    return Some((node.id.clone(), port.name.clone(), port.port_type));
                }
            }
        }
        None
    }

    /// Checa se (x, y) em CSS pixels está sobre algum output port.
    /// Retorna (node_id, port_name, port_type) se encontrou.
    fn hit_test_output_port(&self, x: f32, y: f32) -> Option<(String, String, PortType)> {
        let hit_r_sq = theme::PORT_HIT_RADIUS * theme::PORT_HIT_RADIUS;
        for node in self.nodes.iter().rev() {
            for (i, port) in node.outputs.iter().enumerate() {
                let port_x = node.x + theme::NODE_WIDTH;
                let port_y = node.y
                    + theme::HEADER_HEIGHT
                    + theme::BODY_PAD_TOP
                    + i as f32 * theme::PORT_ROW_HEIGHT
                    + theme::PORT_ROW_HEIGHT / 2.0;
                let dx = x - port_x;
                let dy = y - port_y;
                if dx * dx + dy * dy <= hit_r_sq {
                    return Some((node.id.clone(), port.name.clone(), port.port_type));
                }
            }
        }
        None
    }

    /// Desenha bezier temporária do output port ao cursor durante wire drag.
    /// Cor determinada pelo `port_type` do drag. Retorna sem desenhar se o
    /// node de origem ou o port não forem encontrados.
    fn draw_wire_preview(
        &self,
        painter: &egui::Painter,
        wire_drag: &WireDragState,
        mouse_pos: egui::Pos2,
        canvas_origin: egui::Pos2,
    ) {
        let Some(from_node) = self.nodes.iter().find(|n| n.id == wire_drag.from_node_id)
        else { return };

        let Some(from_pos) = wire::get_port_position(
            from_node,
            &wire_drag.from_port_name,
            true,
            self.scale,
            canvas_origin,
        ) else { return };

        let wire_color = port_type_color(wire_drag.port_type);

        let dx = (mouse_pos.x - from_pos.x).abs();
        let tension = dx.max(theme::WIRE_MIN_TENSION * self.scale) * theme::WIRE_TENSION_RATIO;
        let cp1 = egui::pos2(from_pos.x + tension, from_pos.y);
        let cp2 = egui::pos2(mouse_pos.x - tension, mouse_pos.y);

        let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
            [from_pos, cp1, cp2, mouse_pos],
            false,
            egui::Color32::TRANSPARENT,
            egui::Stroke::new(
                theme::WIRE_THICKNESS,
                wire_color.linear_multiply(theme::WIRE_OPACITY),
            ),
        );
        painter.add(bezier);
    }

    /// Retorna `true` se já existe um wire com esses endpoints exatos (sem duplicatas).
    fn wire_exists(&self, from_node: &str, from_port: &str, to_node: &str, to_port: &str) -> bool {
        self.wires.iter().any(|w| {
            w.from_node == from_node && w.from_port == from_port
                && w.to_node == to_node && w.to_port == to_port
        })
    }

    /// Desenha a grade de pontos do background em intervalos de `GRID_SPACING`.
    /// `effective_origin` garante que os pontos se movem com o pan do viewport.
    fn draw_grid(&self, painter: &egui::Painter, rect: egui::Rect, effective_origin: egui::Pos2) {
        let spacing = theme::GRID_SPACING * self.scale;
        let dot_r = theme::GRID_DOT_RADIUS * self.scale;
        // rem_euclid mantém o offset positivo independente do sinal do pan
        let offset_x = effective_origin.x.rem_euclid(spacing);
        let offset_y = effective_origin.y.rem_euclid(spacing);
        // Começa um período antes do left/top pra garantir cobertura nas bordas
        let mut x = rect.left() + offset_x - spacing;
        while x < rect.right() {
            let mut y = rect.top() + offset_y - spacing;
            while y < rect.bottom() {
                painter.circle_filled(egui::pos2(x, y), dot_r, theme::GRID_DOT_COLOR);
                y += spacing;
            }
            x += spacing;
        }
    }
}
