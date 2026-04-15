//! Lib de `den_app` — expõe configs compartilhados pelos binários (`den_app`,
//! `preview`, `style_editor`).
//!
//! O propósito desta lib é EVITAR DUPLICAÇÃO de constantes entre os bins.
//! Qualquer coisa que dois ou mais bins precisem (ex: `WINDOW_WIDTH`, paths
//! de assets) vive aqui. Coisas exclusivas de cada bin continuam em seu
//! próprio `*_config` privado.
//!
//! Não exponha aqui o pipeline de render do app principal (`den_paint`,
//! `routes`, `pages`) — esses são internos do bin `den_app`.

pub mod app_config;