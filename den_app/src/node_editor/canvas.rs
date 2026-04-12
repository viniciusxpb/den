use eframe::egui;
use super::{theme, types::*, node, wire};

pub struct NodeEditorCanvas {
    pub nodes: Vec<NodeData>,
    pub wires: Vec<WireData>,
    pub scale: f32,
    pub drag: Option<DragState>,
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
    }

    fn handle_drag(&mut self, response: &egui::Response, canvas_origin: egui::Pos2) {
        let s = self.scale;

        // Drag start: identifica qual node foi clicado
        if response.drag_started() {
            if let Some(mouse_pos) = response.interact_pointer_pos() {
                let canvas_x = (mouse_pos.x - canvas_origin.x) / s;
                let canvas_y = (mouse_pos.y - canvas_origin.y) / s;

                if let Some(node) = self.hit_test(canvas_x, canvas_y) {
                    let offset = egui::vec2(canvas_x - node.x, canvas_y - node.y);
                    self.drag = Some(DragState {
                        node_id: node.id.clone(),
                        offset,
                    });
                }
            }
        }

        // Drag move: atualiza posição do node
        if response.dragged() {
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

        // Drag end: limpa estado
        if response.drag_stopped() {
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
