//! Helpers específicos de layout flex no codegen.
//!
//! A distribuição flex em si roda em `den_layout` (runtime). Este módulo
//! centraliza só as decisões compile-time: detectar que um elemento é
//! container flex ou filho flex, e marcar o `LayoutIntent.flex_grow`.

use crate::types::{DenElement, DisplayMode};

/// `true` se o elemento é um container `display: flex`.
/// `display` é `Option` no `DenVisual` (regra cascade) — `None` cai no default
/// `Block`, então só `Some(Flex)` ativa o container flex aqui.
pub(super) fn is_flex_container(el: &DenElement) -> bool {
    matches!(el.visual.display, Some(DisplayMode::Flex))
}

/// `true` se o elemento tem `flex: 1` / `flex-grow: 1` declarado no SCSS.
/// `Option` por consistência com a regra cascade — `None` ou `Some(false)` = não cresce.
pub(super) fn has_flex_grow(el: &DenElement) -> bool {
    el.visual.flex_grow.unwrap_or(false)
}
