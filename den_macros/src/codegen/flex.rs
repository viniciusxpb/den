use crate::types::{DenNode, WidthValue};
use quote::quote;

/// Info sobre um filho direto de um flex container, usada pra gerar
/// o cálculo de largura por filho em runtime.
pub(super) struct FlexChildInfo {
    /// Layout index deste filho (pra mapear no LayoutTable).
    pub layout_index: usize,
    /// Regra de largura declarada no SCSS.
    pub width: WidthValue,
    /// `flex: 1` — participa da distribuição igual de espaço.
    pub flex_grow: bool,
}

/// Coleta layout index + width de filhos DIRETOS de um flex container.
/// Usa `walk_den_nodes` (fonte única de verdade pra DFS) e filtra por
/// `parent_index == parent_idx` pra pegar só filhos diretos.
///
/// LIMITAÇÃO: `IfChain` contribui filhos de AMBOS os branches pro `auto_count`.
/// Em runtime só um branch executa, então `__den_flex_share` é calculado pra
/// mais filhos do que estão visíveis. Resultado: filhos ficam mais estreitos
/// do que deveriam quando `<if>` está dentro de `<div display:flex>`.
/// Fix futuro: calcular `__den_flex_share` em runtime contando filhos renderizados.
pub(super) fn collect_flex_children_info(
    children: &[DenNode],
    parent_idx: usize,
    layout_index: &mut usize,
) -> Vec<FlexChildInfo> {
    let mut infos = Vec::new();

    crate::types::walk_den_nodes(children, parent_idx, layout_index, &mut |el, idx, parent| {
        if parent == parent_idx {
            infos.push(FlexChildInfo {
                layout_index: idx,
                width: el.visual.width,
                flex_grow: el.visual.flex_grow,
            });
        }
    });

    infos
}

/// Gera o layout horizontal (flex) com distribuição de largura por filho.
///
/// egui's `ui.horizontal()` não limita a largura dos filhos — conteúdo largo
/// transborda o container. CSS resolve com `flex-shrink: 1` (default). Aqui
/// simulamos calculando `__den_flex_share` em runtime: o espaço restante
/// (depois de filhos Px/Percent) dividido igualmente entre filhos Auto.
/// Cada filho Auto é envolvido em `ui.allocate_ui(__den_flex_share, ...)`
/// no `generate_element` (via `is_flex_auto_child`).
pub(super) fn build_flex_layout(
    inner: proc_macro2::TokenStream,
    flex_info: Option<&[FlexChildInfo]>,
) -> proc_macro2::TokenStream {
    let Some(infos) = flex_info else {
        return quote! { ui.horizontal(|ui| { #inner }); };
    };

    // Só filhos com flex_grow participam da distribuição igualitária.
    // Filhos Auto sem flex_grow são content-sized (comportamento CSS padrão).
    let auto_count = infos.iter().filter(|i| i.flex_grow).count();

    if auto_count == 0 {
        return quote! { ui.horizontal(|ui| { #inner }); };
    }

    // Espaço fixo consumido por filhos Px/Percent.
    let mut fixed_terms = Vec::new();
    for info in infos {
        match info.width {
            WidthValue::Px(_) => {
                let idx = info.layout_index;
                fixed_terms.push(quote! {
                    __den_layout.sizes[#idx].unwrap_or(0.0) * __den_scale
                });
            }
            WidthValue::Percent(pct) => {
                fixed_terms.push(quote! {
                    __den_flex_total * #pct
                });
            }
            WidthValue::Auto => {}
        }
    }

    let fixed_sum = if fixed_terms.is_empty() {
        quote! { 0.0f32 }
    } else {
        let mut acc = fixed_terms[0].clone();
        for term in &fixed_terms[1..] {
            acc = quote! { #acc + #term };
        }
        acc
    };

    let auto_count_lit = auto_count;
    // LIMITAÇÃO: gaps calculados em compile time. Se IfChain dentro do flex
    // tiver branches com quantidade diferente de filhos, o gap count em runtime
    // pode diferir. Mesmo edge case que o auto_count acima.
    let spacing_gaps = infos.len().saturating_sub(1);

    quote! {
        ui.horizontal(|ui| {
            let __den_flex_total = ui.available_width();
            let __den_flex_item_spacing = ui.spacing().item_spacing.x;
            let __den_flex_fixed_sum = #fixed_sum;
            let __den_flex_spacing_total = __den_flex_item_spacing * #spacing_gaps as f32;
            let __den_flex_share = ((__den_flex_total - __den_flex_fixed_sum - __den_flex_spacing_total) / #auto_count_lit as f32).max(0.0);
            #inner
        });
    }
}
