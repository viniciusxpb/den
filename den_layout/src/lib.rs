//! Den Layout System — resolução de layout em runtime.
//!
//! Flat list em ordem DFS, com o `body` invisível no índice 0. O pai define o
//! algoritmo de layout dos filhos: block, flex e futuramente grid. Roda a cada
//! frame porque a janela pode mudar de tamanho.

mod dimension;
mod display;
mod element_style;
mod entry;
mod flex;
mod geometry;
mod router;
mod spacing;
mod state;
mod table;

pub use dimension::DimensionRule;
pub use display::DisplayMode;
pub use element_style::DenElementStyle;
pub use entry::LayoutEntry;
pub use geometry::LayoutRect;
pub use router::{DenPage, DenRouter};
pub use state::{DenDebugState, DenInputState, DenNodeId, DenRouteState};
pub use table::LayoutTable;

/// Índice do body na lista. Sempre 0.
pub const BODY_INDEX: usize = 0;
