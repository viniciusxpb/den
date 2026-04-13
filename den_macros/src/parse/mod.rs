//! Fase 1 do pipeline: parsers de HTML e SCSS.

pub mod color;
mod html;
mod scss;
pub mod text;

pub use html::parse_html;
pub use scss::parse_scss;
