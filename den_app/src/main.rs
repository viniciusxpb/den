#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app_config;
mod pages;

use eframe::egui;
use pages::HomePage;

fn main() -> eframe::Result {
    env_logger::init();

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([app_config::WINDOW_WIDTH, app_config::WINDOW_HEIGHT]),
        ..Default::default()
    };

    eframe::run_native(
        app_config::APP_TITLE,
        options,
        Box::new(|cc| {
            cc.egui_ctx.set_visuals(app_config::default_visuals());
            Ok(Box::new(DenApp::new()))
        }),
    )
}

struct DenApp {
    home: HomePage,
    scale: f32,
}

impl DenApp {
    fn new() -> Self {
        Self {
            home: HomePage::default(),
            scale: app_config::DEFAULT_SCALE,
        }
    }

    fn render_zoom_controls(&mut self, ctx: &egui::Context) {
        egui::Area::new(egui::Id::new("den_zoom_controls"))
            .anchor(egui::Align2::RIGHT_BOTTOM, egui::vec2(-16.0, -16.0))
            .order(egui::Order::Foreground)
            .show(ctx, |ui| {
                egui::Frame::default()
                    .fill(egui::Color32::from_rgba_unmultiplied(40, 40, 40, 220))
                    .corner_radius(8.0)
                    .inner_margin(4.0)
                    .show(ui, |ui| {
                        ui.horizontal(|ui| {
                            if ui.small_button("−").clicked() {
                                self.scale = (self.scale - app_config::SCALE_STEP)
                                    .max(app_config::MIN_SCALE);
                            }
                            let pct = (self.scale * 100.0).round() as u32;
                            ui.label(
                                egui::RichText::new(format!("{pct}%"))
                                    .color(egui::Color32::WHITE)
                                    .size(12.0),
                            );
                            if ui.small_button("+").clicked() {
                                self.scale = (self.scale + app_config::SCALE_STEP)
                                    .min(app_config::MAX_SCALE);
                            }
                        });
                    });
            });
    }
}

impl eframe::App for DenApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Atalhos de teclado
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Equals)) {
            self.scale = (self.scale + app_config::SCALE_STEP).min(app_config::MAX_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Minus)) {
            self.scale = (self.scale - app_config::SCALE_STEP).max(app_config::MIN_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num0)) {
            self.scale = app_config::DEFAULT_SCALE;
        }
        // Ctrl+scroll: zoom com roda do mouse
        let scroll_delta = ctx.input(|i| {
            if i.modifiers.ctrl { i.raw_scroll_delta.y } else { 0.0 }
        });
        if scroll_delta != 0.0 {
            let steps = (scroll_delta / 50.0).clamp(-1.0, 1.0);
            self.scale = (self.scale + steps * app_config::SCALE_STEP)
                .clamp(app_config::MIN_SCALE, app_config::MAX_SCALE);
        }

        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::vertical().show(ui, |ui| {
                self.home.render(ui, self.scale);
            });
        });

        self.render_zoom_controls(ctx);
    }
}
