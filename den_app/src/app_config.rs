//! Constantes de configuração compartilhadas pelo app e pelos binários auxiliares.
//!
//! Usado por:
//! - `bin/den_app` (main app) — janela, zoom, visuals.
//! - `bin/preview` — `WINDOW_WIDTH` como largura do viewport HTML.
//! - `bin/style_editor` — `MANIFEST_DIR` pra localizar `src/pages/`.
//!
//! Pra os 3 bins consumirem este módulo, `den_app` é compilado como lib
//! (ver `lib.rs`). Constantes aqui devem ser as REALMENTE compartilhadas;
//! valores específicos de cada bin ficam em seu próprio `*_config`.

use eframe::egui;

pub const APP_TITLE: &str = "Den";
/// Largura da janela egui em CSS pixels — também usada pelo `preview` como
/// largura do viewport HTML pra que browser e runtime renderizem com mesma caixa.
pub const WINDOW_WIDTH: f32 = 1200.0;
pub const WINDOW_HEIGHT: f32 = 800.0;

/// Path do crate `den_app` em compile time. Bins usam pra localizar `src/pages/`.
pub const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");

/// Zoom padrão (100%)
pub const DEFAULT_SCALE: f32 = 1.0;
/// Zoom mínimo (50%) — abaixo disso texto fica ilegível
pub const MIN_SCALE: f32 = 0.5;
/// Zoom máximo (300%)
pub const MAX_SCALE: f32 = 3.0;
/// Incremento por clique de +/-
pub const SCALE_STEP: f32 = 0.1;
/// Sensibilidade do scroll pra zoom (delta_pixels / SENSITIVITY = steps)
pub const SCROLL_SENSITIVITY: f32 = 50.0;

pub fn default_visuals() -> egui::Visuals {
    egui::Visuals::light()
}
