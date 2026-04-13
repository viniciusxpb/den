use eframe::egui;

pub struct NodesPage;

impl NodesPage {
    pub fn render(&mut self, ui: &mut egui::Ui, __den_scale: f32) {
        den_macros::den_template!("pages/nodes/nodes", self);
    }
}
