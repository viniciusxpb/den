use eframe::egui;
use super::{theme, types::*, node, wire};

pub struct NodeEditorCanvas {
    pub nodes: Vec<NodeData>,
    pub wires: Vec<WireData>,
    pub scale: f32,
    pub drag: Option<DragState>,
    pub wire_drag: Option<WireDragState>,
}

impl NodeEditorCanvas {
    pub fn render(&mut self, ui: &mut egui::Ui) {
        let (response, painter) = ui.allocate_painter(
            ui.available_size(),
            egui::Sense::click_and_drag(),
        );
        let canvas_rect = response.rect;
        let origin = canvas_rect.min;

        // Drag handling (antes do render pra posições estarem atualizadas)
        self.handle_drag(&response, origin);

        // 1. Background
        painter.rect_filled(canvas_rect, 0.0, theme::BG);

        // 2. Grid dots
        self.draw_grid(&painter, canvas_rect);

        // 3. Wires (behind nodes)
        for w in &self.wires {
            wire::draw_wire(&painter, w, &self.nodes, self.scale, origin);
        }

        // 4. Nodes — node arrastado renderiza por último (z-order topo)
        let dragged_id = self.drag.as_ref().map(|d| d.node_id.clone());
        for n in &self.nodes {
            if Some(&n.id) != dragged_id.as_ref() {
                node::draw_node(&painter, n, self.scale, origin);
            }
        }
        if let Some(ref drag_id) = dragged_id {
            if let Some(n) = self.nodes.iter().find(|n| &n.id == drag_id) {
                node::draw_node(&painter, n, self.scale, origin);
            }
        }

        // 5. Wire drag preview (bezier temporária port → cursor)
        if let Some(ref wd) = self.wire_drag {
            if let Some(mouse_pos) = response.interact_pointer_pos() {
                self.draw_wire_preview(&painter, wd, mouse_pos, origin);
            }
        }
    }

    fn handle_drag(&mut self, response: &egui::Response, canvas_origin: egui::Pos2) {
        let s = self.scale;

        if response.drag_started() {
            if let Some(mouse_pos) = response.interact_pointer_pos() {
                let canvas_x = (mouse_pos.x - canvas_origin.x) / s;
                let canvas_y = (mouse_pos.y - canvas_origin.y) / s;

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

        // Node drag move (só se NÃO estiver em wire drag)
        if response.dragged() && self.wire_drag.is_none() {
            if let Some(ref drag) = self.drag {
                if let Some(mouse_pos) = response.interact_pointer_pos() {
                    let canvas_x = (mouse_pos.x - canvas_origin.x) / s;
                    let canvas_y = (mouse_pos.y - canvas_origin.y) / s;
                    let new_x = canvas_x - drag.offset.x;
                    let new_y = canvas_y - drag.offset.y;
                    let node_id = drag.node_id.clone();
                    if let Some(node) = self.nodes.iter_mut().find(|n| n.id == node_id) {
                        node.x = new_x;
                        node.y = new_y;
                    }
                }
            }
        }

        if response.drag_stopped() {
            self.wire_drag = None;
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

    /// Checa se (x, y) em CSS pixels está sobre algum output port.
    /// Retorna (node_id, port_name, port_type) se encontrou.
    fn hit_test_output_port(&self, x: f32, y: f32) -> Option<(String, String, PortType)> {
        let hit_r = theme::PORT_HIT_RADIUS;
        let hit_r_sq = hit_r * hit_r;
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

        let wire_color = match wire_drag.port_type {
            PortType::Exec => theme::WIRE_EXEC,
            PortType::Data => theme::WIRE_DATA,
            _ => theme::WIRE_DEFAULT,
        };

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

    fn draw_grid(&self, painter: &egui::Painter, rect: egui::Rect) {
        let spacing = theme::GRID_SPACING * self.scale;
        let dot_r = theme::GRID_DOT_RADIUS * self.scale;
        let mut x = rect.left();
        while x < rect.right() {
            let mut y = rect.top();
            while y < rect.bottom() {
                painter.circle_filled(egui::pos2(x, y), dot_r, theme::GRID_DOT_COLOR);
                y += spacing;
            }
            x += spacing;
        }
    }
}
