use eframe::egui;

pub struct HomePage {
    pub name: String,
    pub message: String,
    pub active: bool,
    pub count: u32,
    pub tags: Vec<String>,
}

impl Default for HomePage {
    fn default() -> Self {
        Self {
            name: "Vini".to_string(),
            message: String::new(),
            active: true,
            count: 0,
            tags: vec![
                "Rust".to_string(),
                "egui".to_string(),
                "Den".to_string(),
                "SCSS".to_string(),
                "Proc Macros".to_string(),
            ],
        }
    }
}

impl HomePage {
    pub fn render(&mut self, ui: &mut egui::Ui, __den_scale: f32) {
        den_macros::den_template!("pages/home/home", self);
    }

    fn toggle_status(&mut self) {
        self.active = !self.active;
    }

    fn increment_count(&mut self) {
        self.count += 1;
    }

    fn reset(&mut self) {
        self.active = true;
        self.count = 0;
        self.message.clear();
    }
}
