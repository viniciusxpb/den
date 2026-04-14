//! Configuração do gerador HTML de preview.

/// Largura da janela egui em pixels.
///
/// Deve coincidir com `app_config::WINDOW_WIDTH = 1200`; o preview é um binário
/// separado e ainda não compartilha config de app sem um crate comum.
pub(crate) const EGUI_WINDOW_WIDTH: u32 = 1200;

/// Nome do único arquivo HTML gerado pelo preview.
pub(crate) const PREVIEW_FILE_NAME: &str = "preview.html";

/// Nome legado gerado pela versão antiga do preview.
pub(crate) const LEGACY_INDEX_FILE_NAME: &str = "index.html";

/// Intervalo de auto-refresh do preview no browser.
pub(crate) const AUTO_REFRESH_SECONDS: u32 = 3;

/// Quantas iterações simular em `<for>` quando não há dados reais.
pub(crate) const FOR_LOOP_ITERATIONS: usize = 3;

/// Propriedades que recebem `px` no preview quando declaradas sem unidade.
pub(crate) const PX_PROPS: &[&str] = &[
    "font-size",
    "padding",
    "border-radius",
    "margin",
    "width",
    "height",
    "top",
    "left",
    "right",
    "bottom",
    "border-width",
    "gap",
    "letter-spacing",
];
