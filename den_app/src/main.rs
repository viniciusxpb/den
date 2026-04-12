#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

mod app_config;
mod node_editor;
mod pages;

use eframe::egui;
use node_editor::NodeEditorCanvas;
use pages::HomePage;

enum ActiveView {
    Home,
    NodeEditor,
}

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
    node_editor: NodeEditorCanvas,
    scale: f32,
    active_view: ActiveView,
}

impl DenApp {
    fn new() -> Self {
        Self {
            home: HomePage::default(),
            node_editor: NodeEditorCanvas::new(),
            scale: app_config::DEFAULT_SCALE,
            active_view: ActiveView::Home,
        }
    }

    fn render_zoom_controls(&mut self, ctx: &egui::Context) {
        let mut current_scale = match self.active_view {
            ActiveView::Home => self.scale,
            ActiveView::NodeEditor => self.node_editor.scale,
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

        match self.active_view {
            ActiveView::Home => self.scale = current_scale,
            ActiveView::NodeEditor => self.node_editor.scale = current_scale,
        }
    }
}

impl eframe::App for DenApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // F2: toggle view
        if ctx.input(|i| i.key_pressed(egui::Key::F2)) {
            self.active_view = match self.active_view {
                ActiveView::Home => ActiveView::NodeEditor,
                ActiveView::NodeEditor => ActiveView::Home,
            };
        }

        // Zoom: roteia pro scale da view ativa
        let active_scale = match self.active_view {
            ActiveView::Home => &mut self.scale,
            ActiveView::NodeEditor => &mut self.node_editor.scale,
        };

        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Equals)) {
            *active_scale = (*active_scale + app_config::SCALE_STEP).min(app_config::MAX_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Minus)) {
            *active_scale = (*active_scale - app_config::SCALE_STEP).max(app_config::MIN_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num0)) {
            *active_scale = app_config::DEFAULT_SCALE;
        }
        let scroll_delta = ctx.input(|i| {
            if i.modifiers.ctrl { i.raw_scroll_delta.y } else { 0.0 }
        });
        if scroll_delta != 0.0 {
            let steps = (scroll_delta / app_config::SCROLL_SENSITIVITY).clamp(-1.0, 1.0);
            *active_scale = (*active_scale + steps * app_config::SCALE_STEP)
                .clamp(app_config::MIN_SCALE, app_config::MAX_SCALE);
        }

        // Render
        egui::CentralPanel::default().show(ctx, |ui| {
            match self.active_view {
                ActiveView::Home => {
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        self.home.render(ui, self.scale);
                    });
                }
                ActiveView::NodeEditor => {
                    self.node_editor.render(ui);
                }
            }
        });

        self.render_zoom_controls(ctx);
    }
}
