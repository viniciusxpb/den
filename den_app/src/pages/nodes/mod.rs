//! Tela dedicada a gerenciamento e execução de nodes ndnm.
//!
//! Auto-descobre subpastas em `nodes/` que tenham `node.toml`, mostra info de
//! cada uma, e dispara `/health` ou `/run` no servidor HTTP que cada node expõe.

#[allow(clippy::module_inception)]
mod nodes;
pub use nodes::{NodeConfig, NodesPage, read_and_parse};
