#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod pages;

use eframe::egui;
use pages::HomePage;

fn main() -> eframe::Result {
    env_logger::init();

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default().with_inner_size([400.0, 300.0]),
        ..Default::default()
    };

    eframe::run_native(
        "Den",
        options,
        Box::new(|_cc| Ok(Box::new(DenApp::default()))),
    )
}

struct DenApp {
    home: HomePage,
}

impl Default for DenApp {
    fn default() -> Self {
        Self {
            home: HomePage::default(),
        }
    }
}

impl eframe::App for DenApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            self.home.render(ui);
        });
    }
}
