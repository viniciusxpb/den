//! Geração de container `display: flex`.

use quote::quote;

/// Gera um container flex horizontal.
///
/// A distribuição de largura é responsabilidade do runtime `den_layout`.
/// O codegen só aplica o `gap` visual do egui e renderiza os filhos.
pub(super) fn build_flex_layout(
    inner: proc_macro2::TokenStream,
    gap: Option<f32>,
) -> proc_macro2::TokenStream {
    let gap = gap.unwrap_or(0.0);

    quote! {
        ui.horizontal(|ui| {
            ui.spacing_mut().item_spacing.x = #gap * __den_scale;
            #inner
        });
    }
}
