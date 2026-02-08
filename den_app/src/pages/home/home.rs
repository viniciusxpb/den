use eframe::egui;

pub struct HomePage {
    pub name: String,
    pub age: u32,
}

impl Default for HomePage {
    fn default() -> Self {
        Self {
            name: "Vini".to_string(),
            age: 25,
        }
    }
}

impl HomePage {
    pub fn render(&mut self, ui: &mut egui::Ui) {
        den_macros::den_template!("pages/home/home", self);
    }

    fn on_button_click(&mut self) {
        eprintln!("Button clicked!");
    }
}
