#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app_config;
mod models;
mod node_editor;
mod pages;
mod routes;

use den_layout::DenRouter;
use eframe::egui;

pub use routes::*;

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
    router: DenRouter<AppRoute>,
    pages: AppPages,
    scale: f32,
}

impl DenApp {
    fn new() -> Self {
        Self {
            router: DenRouter::new(routes::initial_route()),
            pages: AppPages::new(),
            scale: app_config::DEFAULT_SCALE,
        }
    }

    fn render_zoom_controls(&mut self, ctx: &egui::Context) {
        let mut current_scale = match self.router.current() {
            AppRoute::NodeEditor => self.pages.node_editor_scale(),
            _ => self.scale,
        };

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
                                current_scale = (current_scale - app_config::SCALE_STEP)
                                    .max(app_config::MIN_SCALE);
                            }
                            let pct = (current_scale * 100.0).round() as u32;
                            ui.label(
                                egui::RichText::new(format!("{pct}%"))
                                    .color(egui::Color32::WHITE)
                                    .size(12.0),
                            );
                            if ui.small_button("+").clicked() {
                                current_scale = (current_scale + app_config::SCALE_STEP)
                                    .min(app_config::MAX_SCALE);
                            }
                        });
                    });
            });

        match self.router.current() {
            AppRoute::NodeEditor => self.pages.set_node_editor_scale(current_scale),
            _ => self.scale = current_scale,
        }
    }

    fn queue_next_route(&mut self) {
        self.router
            .goto(routes::demo_next_route(self.router.current()));
    }
}

impl eframe::App for DenApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // F2: ciclo manual de rotas enquanto o goto HTML ainda não existe.
        if ctx.input(|i| i.key_pressed(egui::Key::F2)) {
            self.queue_next_route();
        }

        if self.router.flush() {
            self.pages.sync_from_route(self.router.current());
        }

        // Zoom: roteia pro scale da view ativa
        let is_node_editor = matches!(self.router.current(), AppRoute::NodeEditor);
        let mut active_scale = if is_node_editor {
            self.pages.node_editor_scale()
        } else {
            self.scale
        };

        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Equals)) {
            active_scale = (active_scale + app_config::SCALE_STEP).min(app_config::MAX_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Minus)) {
            active_scale = (active_scale - app_config::SCALE_STEP).max(app_config::MIN_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num0)) {
            active_scale = app_config::DEFAULT_SCALE;
            if is_node_editor {
                self.pages.reset_node_editor_pan();
            }
        }
        let scroll_delta = ctx.input(|i| {
            if i.modifiers.ctrl {
                i.raw_scroll_delta.y
            } else {
                0.0
            }
        });
        if scroll_delta != 0.0 {
            let steps = (scroll_delta / app_config::SCROLL_SENSITIVITY).clamp(-1.0, 1.0);
            active_scale = (active_scale + steps * app_config::SCALE_STEP)
                .clamp(app_config::MIN_SCALE, app_config::MAX_SCALE);
        }
        if is_node_editor {
            self.pages.set_node_editor_scale(active_scale);
        } else {
            self.scale = active_scale;
        }

        // Render
        egui::CentralPanel::default().show(ctx, |ui| {
            self.pages.render_current(ui, self.scale, &mut self.router);
        });

        self.render_zoom_controls(ctx);
    }
}
