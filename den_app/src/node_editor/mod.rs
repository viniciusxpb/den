mod canvas;
mod demo;
mod node;
mod theme;
mod types;
mod wire;

pub use canvas::NodeEditorCanvas;

impl NodeEditorCanvas {
    pub fn new() -> Self {
        Self {
            nodes: demo::demo_nodes(),
            wires: demo::demo_wires(),
            scale: 1.0,
            drag: None,
        }
    }
}
