//! Fase 1 do pipeline: parsers de HTML e SCSS.

mod html;
mod scss;
pub mod color;
pub mod text;

pub use html::parse_html;
pub use scss::parse_scss;
