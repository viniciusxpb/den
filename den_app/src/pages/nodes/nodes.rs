use crate::AppRoute;
use den_layout::DenRouter;
use eframe::egui;

#[derive(Default)]
pub struct NodesPage;

impl NodesPage {
    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
    ) {
        den_macros::den_template!("pages/nodes/nodes", self);
    }
}
