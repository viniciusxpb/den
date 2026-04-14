//! Configuração do editor visual de SCSS.

use std::time::Duration;

/// Path do pacote `den_app` em compile time.
pub(crate) const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");

/// Delay de debounce antes de escrever alterações em disco.
pub(crate) const WRITE_DELAY: Duration = Duration::from_millis(300);

/// Intervalo de re-scan dos arquivos SCSS.
pub(crate) const SCAN_INTERVAL: Duration = Duration::from_secs(1);

/// Menor valor permitido no slider de `font-size`.
pub(crate) const FONT_SIZE_MIN: f32 = 6.0;

/// Maior valor permitido no slider de `font-size`.
pub(crate) const FONT_SIZE_MAX: f32 = 72.0;

/// Maior valor permitido nos sliders de `padding` e `margin`.
pub(crate) const PADDING_MAX: f32 = 64.0;

/// Maior valor permitido no slider de `border-radius`.
pub(crate) const BORDER_RADIUS_MAX: f32 = 32.0;

/// Maior valor permitido no slider de `width` em pixels.
pub(crate) const WIDTH_PX_MAX: f32 = 800.0;

/// Maior valor permitido no slider de largura de borda.
pub(crate) const BORDER_WIDTH_MAX: f32 = 10.0;

/// Valor padrão quando `font-size` não pode ser parseado.
pub(crate) const DEFAULT_FONT_SIZE: f32 = 16.0;

/// Valor padrão quando `width` em pixels não pode ser parseado.
pub(crate) const DEFAULT_WIDTH_PX: f32 = 100.0;

/// Valor padrão quando `width` percentual não pode ser parseado.
pub(crate) const DEFAULT_WIDTH_PERCENT: f32 = 100.0;

/// Largura da coluna de label de propriedade no style editor.
pub(crate) const PROPERTY_LABEL_WIDTH: f32 = 96.0;

/// Altura da label de propriedade.
pub(crate) const PROPERTY_LABEL_HEIGHT: f32 = 16.0;
