//! Helpers específicos de layout flex no codegen.
//!
//! A distribuição flex em si roda em `den_layout` (runtime). Este módulo
//! centraliza só as decisões compile-time: detectar que um elemento é
//! container flex ou filho flex, e marcar o `LayoutIntent.flex_grow`.

use crate::types::{DenElement, DisplayMode};

/// `true` se o elemento é um container `display: flex`.
pub(super) fn is_flex_container(el: &DenElement) -> bool {
    el.visual.display == DisplayMode::Flex
}

/// `true` se o elemento tem `flex: 1` / `flex-grow: 1` declarado no SCSS.
pub(super) fn has_flex_grow(el: &DenElement) -> bool {
    el.visual.flex_grow
}
