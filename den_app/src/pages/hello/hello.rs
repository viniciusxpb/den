use crate::{AppRoute, models::Usuario};
use den_layout::DenRouter;
use eframe::egui;

#[den_macros::den_page]
pub struct HelloPage {
    pub usuario: Usuario,
}

impl HelloPage {
    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
    ) {
        den_macros::den_template!("pages/hello/hello", self);
    }
}
