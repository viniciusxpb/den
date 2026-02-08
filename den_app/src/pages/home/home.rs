use eframe::egui;

pub struct HomePage;

impl HomePage {
    pub fn render(ui: &mut egui::Ui) {
        den_macros::den_template!("pages/home/home");
    }
}
