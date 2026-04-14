//! Regra `margin` — espaço externo reservado FORA da border box.
//!
//! Margin não faz parte da caixa visível do elemento (não recebe background,
//! não é incluído em `rect.width/height`). Ele separa elementos irmãos no
//! fluxo block e reserva espaço horizontal em flex.
//!
//! **Não implementa collapse**: margens verticais adjacentes são reservadas
//! inteiras em ambos, ao contrário do CSS de browser. Ver PENDING.md.

use crate::config;

/// Retorna o espaço total ocupado por margin uniforme nos dois lados de um eixo.
///
/// Ex.: `margin: 10px` ocupa 20px no total (10 topo + 10 base, ou 10 esq + 10 dir).
pub(crate) fn uniform_extent(margin: f32) -> f32 {
    margin * config::SIDES_PER_AXIS
}

/// Retorna a largura disponível para um filho depois de descontar sua margin.
///
/// Usado quando o pai calcula o espaço que o filho tem dentro do content box:
/// o filho "come" margin dos dois lados antes do seu próprio rect começar.
pub(crate) fn child_content_width(parent_content_width: f32, margin: f32) -> f32 {
    (parent_content_width - uniform_extent(margin)).max(0.0)
}
