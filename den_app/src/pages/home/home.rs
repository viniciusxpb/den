use eframe::egui;

pub struct HomePage {
    pub name: String,
    pub age: u32,
    pub items: Vec<String>,
    pub logged_in: bool,
}

impl Default for HomePage {
    fn default() -> Self {
        Self {
            name: "Vini".to_string(),
            age: 25,
            items: vec!["Rust".to_string(), "egui".to_string(), "Den".to_string()],
            logged_in: true,
        }
    }
}

impl HomePage {
    pub fn render(&mut self, ui: &mut egui::Ui) {
        den_macros::den_template!("pages/home/home", self);
    }

    fn on_button_click(&mut self) {
        self.logged_in = !self.logged_in;
    }
}
