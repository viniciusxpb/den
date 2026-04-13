//! Editor visual de grafos de nodes usando egui::Painter.

mod canvas;
mod demo;
mod node;
mod theme;
mod types;
mod wire;

pub use canvas::NodeEditorCanvas;
use eframe::egui;

impl NodeEditorCanvas {
    pub fn new() -> Self {
        Self {
            nodes: demo::demo_nodes(),
            wires: demo::demo_wires(),
            scale: 1.0,
            drag: None,
            wire_drag: None,
            pan_offset: egui::Vec2::ZERO,
        }
    }
}
