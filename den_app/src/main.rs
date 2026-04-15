#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

pub mod den_paint;
mod models;
mod pages;
mod paint_config;
mod routes;

use den_app::app_config;
use den_layout::DenRouter;
use eframe::egui;

pub use routes::*;

/// Tipo de UI usado pelo backend atual do app demo.
pub type DenUi = egui::Ui;

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
        let mut current_scale = self.scale;
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

        self.scale = current_scale;
    }
}

impl eframe::App for DenApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if self.router.flush() {
            self.pages.sync_from_route(self.router.current());
        }

        let mut active_scale = self.scale;

        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Equals)) {
            active_scale = (active_scale + app_config::SCALE_STEP).min(app_config::MAX_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Minus)) {
            active_scale = (active_scale - app_config::SCALE_STEP).max(app_config::MIN_SCALE);
        }
        if ctx.input(|i| i.modifiers.ctrl && i.key_pressed(egui::Key::Num0)) {
            active_scale = app_config::DEFAULT_SCALE;
        }

        // Dev-only: F2 cicla entre todas as rotas sem argumentos, em ordem de
        // declaração no `den_router!`. Boot começa em HomePage (primeira da lista),
        // F2 vai pra próxima arg-less, e assim por diante até voltar pra Home.
        // Rotas com args (HelloPage) ficam fora do ciclo. Compilado fora em release.
        // Remover quando o router tiver navegação in-app equivalente (quick-switcher).
        #[cfg(debug_assertions)]
        if ctx.input(|i| i.key_pressed(egui::Key::F2)) {
            let next = routes::next_argless_route(self.router.current());
            self.router.goto(next);
        }
        // Ctrl+scroll = zoom. Consumimos o delta aqui ANTES de `CentralPanel` renderizar,
        // pra que o `ScrollArea` interno não receba esse scroll e role a página junto.
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
            // Zera o scroll desse frame pro ScrollArea não rolar junto.
            ctx.input_mut(|i| {
                i.raw_scroll_delta.y = 0.0;
                i.smooth_scroll_delta.y = 0.0;
            });
        }
        self.scale = active_scale;

        // Render com scroll vertical nativo do egui — o body do Den cresce com o
        // conteúdo (`paint_tree` aloca `rect = max(viewport, content_height)`),
        // e a `ScrollArea` cuida de clip + scrollbar.
        egui::CentralPanel::default().show(ctx, |ui| {
            egui::ScrollArea::vertical()
                .auto_shrink([false; 2])
                .show(ui, |ui| {
                    self.pages.render_current(ui, self.scale, &mut self.router);
                });
        });

        self.render_zoom_controls(ctx);
    }
}
