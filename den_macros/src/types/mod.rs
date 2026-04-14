//! Tipos compartilhados entre as fases parse, resolve e codegen.
//!
//! Este módulo não contém lógica, só definições de dados.

mod raw;
mod resolved;
mod style;

pub use raw::*;
pub use resolved::*;
pub use style::*;
