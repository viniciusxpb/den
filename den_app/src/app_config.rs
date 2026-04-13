//! Constantes de configuração da janela e sistema de zoom.

use eframe::egui;

pub const APP_TITLE: &str = "Den";
pub const WINDOW_WIDTH: f32 = 1200.0;
pub const WINDOW_HEIGHT: f32 = 800.0;

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
