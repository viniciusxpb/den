//! Configuração das estimativas usadas durante o codegen.

/// Altura de linha usada quando texto não define `font-size`.
pub(super) const DEFAULT_TEXT_LINE_HEIGHT: f32 = 14.0;

/// Altura de linha usada para inputs sem `font-size`.
pub(super) const DEFAULT_INPUT_LINE_HEIGHT: f32 = 16.0;

/// Largura média de glifo usada na estimativa textual antes da medição runtime.
pub(super) const AVERAGE_GLYPH_WIDTH_RATIO: f32 = 0.55;

/// Largura estimada para expressões dinâmicas desconhecidas.
pub(super) const DEFAULT_EXPR_TEXT_WIDTH: f32 = 48.0;

/// Largura estimada para inputs sem largura explícita.
pub(super) const DEFAULT_INPUT_WIDTH: f32 = 180.0;
