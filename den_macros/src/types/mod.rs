//! Tipos compartilhados entre as fases parse, resolve e codegen.
//!
//! Este módulo não contém lógica, só definições de dados.

mod raw;
mod resolved;
mod style;
mod walk;

pub use raw::*;
pub use resolved::*;
pub use style::*;
pub use walk::*;
