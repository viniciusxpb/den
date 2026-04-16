//! Den Layout System — resolução de layout em runtime.
//!
//! Flat list em ordem DFS, com o `body` invisível no índice 0. O pai define o
//! algoritmo de layout dos filhos: block, flex e futuramente grid. Roda a cada
//! frame porque a janela pode mudar de tamanho.

mod config;
mod dimension;
mod display;
mod element_style;
mod entry;
mod flex;
mod geometry;
mod ghost;
mod height;
mod margin;
pub mod pipes;
mod render;
mod router;
mod state;
mod table;
mod width;

pub use dimension::{DimensionRule, PositionKind};
pub use ghost::{DenGhost, DenGhostService};
pub use pipes::Pipe;
pub use display::{AlignItems, DisplayMode, FlexDirection, JustifyContent, OverflowKind};
pub use element_style::DenElementStyle;
pub use entry::LayoutEntry;
pub use geometry::LayoutRect;
pub use render::{
    BoxShadow, Interact, LayoutIntent, PaintStyle, RenderKind, RenderNode, RenderTree, Rgb,
    TextAlign, TextTransform, Transform2d,
};
pub use router::{DenPage, DenRouter};
pub use state::{DenDebugState, DenInputState, DenNodeId, DenRouteState};
pub use table::{LayoutTable, layout_debug_enabled};

/// Índice do body na lista. Sempre 0.
pub const BODY_INDEX: usize = 0;
