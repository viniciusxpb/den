//! Regras de espaçamento usadas pelo motor de layout Den.

use crate::LayoutRect;

/// Quantidade de lados em um eixo para espaçamento uniforme.
const SIDES_PER_AXIS: f32 = 2.0;

/// Retorna a largura interna de um retângulo após padding horizontal uniforme.
pub(crate) fn content_width(rect: LayoutRect, padding: f32) -> f32 {
    (rect.width - uniform_padding_extent(padding)).max(0.0)
}

/// Retorna a largura disponível para o conteúdo de um filho com margin uniforme.
pub(crate) fn child_content_width(parent_content_width: f32, margin: f32) -> f32 {
    (parent_content_width - uniform_margin_extent(margin)).max(0.0)
}

/// Retorna o espaço interno ocupado por padding uniforme em um eixo.
pub(crate) fn uniform_padding_extent(padding: f32) -> f32 {
    padding * SIDES_PER_AXIS
}

/// Retorna o espaço externo ocupado por margin uniforme em um eixo.
///
/// Margens ainda não colapsam como no CSS do browser; cada lado é reservado.
pub(crate) fn uniform_margin_extent(margin: f32) -> f32 {
    margin * SIDES_PER_AXIS
}
