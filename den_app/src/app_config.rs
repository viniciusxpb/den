use eframe::egui;

pub const APP_TITLE: &str = "Den";
pub const WINDOW_WIDTH: f32 = 400.0;
pub const WINDOW_HEIGHT: f32 = 300.0;

pub fn default_visuals() -> egui::Visuals {
    egui::Visuals::light()
}
