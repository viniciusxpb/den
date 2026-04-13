use crate::{AppRoute, models::Usuario};
use den_layout::DenRouter;
use eframe::egui;

pub struct UsuarioPage {
    pub usuario: Usuario,
}

impl UsuarioPage {
    pub fn new() -> Self {
        Self {
            usuario: Usuario::vazio(),
        }
    }

    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
    ) {
        den_macros::den_template!("pages/usuario/usuario", self);
    }
}

impl Default for UsuarioPage {
    fn default() -> Self {
        Self::new()
    }
}
