use eframe::egui;
use super::{theme, types::*, node, wire};

pub struct NodeEditorCanvas {
    pub nodes: Vec<NodeData>,
    pub wires: Vec<WireData>,
    pub scale: f32,
}

impl NodeEditorCanvas {
    pub fn render(&mut self, ui: &mut egui::Ui) {
        let (response, painter) = ui.allocate_painter(
            ui.available_size(),
            egui::Sense::click(),
        );
        let canvas_rect = response.rect;
        let origin = canvas_rect.min;

        // 1. Background
        painter.rect_filled(canvas_rect, 0.0, theme::BG);

        // 2. Grid dots
        self.draw_grid(&painter, canvas_rect);

        // 3. Wires (behind nodes)
        for w in &self.wires {
            wire::draw_wire(&painter, w, &self.nodes, self.scale, origin);
        }

        // 4. Nodes
        for n in &self.nodes {
            node::draw_node(&painter, n, self.scale, origin);
        }
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
