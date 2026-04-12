use crate::types::{DenElement, DenNode, DenVisual, DisplayMode, WidthValue};
use super::{generate_node, CodegenCtx};
use super::frame::{build_frame_expr, build_rich_text_expr};
use super::text::build_text_token_stream;
use quote::quote;
use std::hash::{Hash, Hasher};

/// Info sobre um filho direto de um flex container, usada pra gerar
/// o cálculo de largura por filho em runtime.
struct FlexChildInfo {
    /// Layout index deste filho (pra mapear no LayoutTable).
    layout_index: usize,
    /// Regra de largura declarada no SCSS.
    width: WidthValue,
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
fn collect_flex_children_info(
    children: &[DenNode],
    parent_idx: usize,
    layout_index: &mut usize,
) -> Vec<FlexChildInfo> {
    let mut infos = Vec::new();

    // walk_den_nodes avança o counter pra todos os descendentes,
    // mas só coletamos os que são filhos diretos (parent == parent_idx).
    crate::types::walk_den_nodes(children, parent_idx, layout_index, &mut |el, idx, parent| {
        if parent == parent_idx {
            infos.push(FlexChildInfo {
                layout_index: idx,
                width: el.visual.width,
            });
        }
    });

    infos
}

pub fn generate_element(
    el: &DenElement,
    ctx: &mut CodegenCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let visual = &el.visual;

    // Valida uso de (click) sem self
    if el.on_click.is_some() && !ctx.has_self {
        return Err(
            "Template uses (click) event but `self` was not passed to den_template!. \
             Use: den_template!(\"path\", self);"
                .to_string(),
        );
    }

    // Constrói conteúdo de texto
    let text_ts = build_text_token_stream(&el.segments, ctx.has_self)?;

    // Layout index deste elemento. Deve ser capturado ANTES de gerar filhos
    // pra que os filhos recebam os índices subsequentes (mesmo DFS do pré-passo).
    let my_layout_index = ctx.layout_index;
    ctx.layout_index += 1;

    // Se este elemento é flex, pré-coleta info dos filhos diretos pra computar
    // a distribuição de largura em runtime. Usa um clone do layout_index pra
    // não interferir com a geração real dos filhos.
    let flex_children_info = if visual.display == DisplayMode::Flex {
        let mut peek_index = ctx.layout_index;
        Some(collect_flex_children_info(&el.children, my_layout_index, &mut peek_index))
    } else {
        None
    };

    // Gera filhos. Se este elemento é flex, seta parent_is_flex pros filhos
    // pra que filhos Auto saibam usar __den_flex_share.
    let prev_parent_is_flex = ctx.parent_is_flex;
    ctx.parent_is_flex = visual.display == DisplayMode::Flex;

    let mut children_code = Vec::new();
    for (i, child) in el.children.iter().enumerate() {
        ctx.tree_path.push(i);
        children_code.push(generate_node(child, ctx)?);
        ctx.tree_path.pop();
    }

    // Restaura o estado anterior (pra irmãos não herdarem).
    ctx.parent_is_flex = prev_parent_is_flex;

    let tag = el.tag.as_str();
    let has_hover = visual.needs_hover();
    let has_click = el.on_click.is_some();
    let needs_interaction = has_hover || has_click;

    // Constrói o call do click handler
    let click_call = if let Some(func_name) = &el.on_click {
        let tokens: proc_macro2::TokenStream = format!("self.{func_name}()")
            .parse()
            .map_err(|e| format!("Invalid function name '{func_name}': {e}"))?;
        Some(tokens)
    } else {
        None
    };

    let flex_info_ref = flex_children_info.as_deref();

    // Determina se este elemento é um filho Auto de flex — precisa de constraint.
    let is_flex_auto_child = prev_parent_is_flex && visual.width == WidthValue::Auto;

    let mut element_code = if needs_interaction {
        let element_id = den_element_id(ctx.template_path, &ctx.tree_path, tag, &el.classes);

        let render_code = if has_hover {
            let hovered = visual.resolve_hover();

            let base_inner = build_inner(visual, &text_ts, &children_code, tag, my_layout_index, flex_info_ref);
            let hover_inner = build_inner(&hovered, &text_ts, &children_code, tag, my_layout_index, flex_info_ref);

            let base_code = if visual.needs_frame() {
                let frame = build_frame_expr(visual);
                quote! { #frame.show(ui, |ui| { #base_inner }); }
            } else {
                base_inner
            };

            let hover_code = if hovered.needs_frame() {
                let frame = build_frame_expr(&hovered);
                quote! { #frame.show(ui, |ui| { #hover_inner }); }
            } else {
                hover_inner
            };

            let cursor_code = if hovered.cursor_pointer {
                quote! {
                    if __den_is_hovered {
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                }
            } else {
                quote! {}
            };

            quote! {
                let __den_was_hovered = ui.data(|d| d.get_temp::<bool>(__den_id).unwrap_or(false));
                let __den_scope = ui.scope(|ui| {
                    if __den_was_hovered {
                        #hover_code
                    } else {
                        #base_code
                    }
                });
                let __den_is_hovered = ui.rect_contains_pointer(__den_scope.response.rect);
                ui.data_mut(|d| d.insert_temp(__den_id, __den_is_hovered));
                #cursor_code
            }
        } else {
            // Click apenas, sem hover — wrap em scope pra capturar rect
            let inner_code = build_inner(visual, &text_ts, &children_code, tag, my_layout_index, flex_info_ref);
            let wrapped = if visual.needs_frame() {
                let frame_expr = build_frame_expr(visual);
                quote! { #frame_expr.show(ui, |ui| { #inner_code }); }
            } else {
                inner_code
            };
            quote! {
                let __den_scope = ui.scope(|ui| { #wrapped });
            }
        };

        let click_code = if let Some(call) = click_call {
            quote! {
                let __den_resp = ui.interact(
                    __den_scope.response.rect,
                    __den_id.with("click"),
                    egui::Sense::click(),
                );
                if __den_resp.clicked() {
                    #call;
                }
            }
        } else {
            quote! {}
        };

        let id_expr = if ctx.loop_depth > 0 {
            let mut salt = quote! { 0u64 };
            for d in 0..ctx.loop_depth {
                let idx: proc_macro2::TokenStream = format!("__den_idx_{d}")
                    .parse()
                    .map_err(|e| format!("Internal error building loop salt ident: {e}"))?;
                salt = quote! { (#salt).wrapping_mul(31).wrapping_add(#idx as u64) };
            }
            quote! { egui::Id::new(#element_id ^ #salt) }
        } else {
            quote! { egui::Id::new(#element_id) }
        };

        quote! {
            {
                let __den_id = #id_expr;
                #render_code
                #click_code
            }
        }
    } else {
        // Sem hover, sem click — caminho simples
        let inner_code = build_inner(visual, &text_ts, &children_code, tag, my_layout_index, flex_info_ref);

        if visual.needs_frame() {
            let frame_expr = build_frame_expr(visual);
            quote! {
                #frame_expr.show(ui, |ui| {
                    #inner_code
                });
            }
        } else {
            inner_code
        }
    };

    // Se este elemento é um filho Auto dentro de um flex container,
    // limita sua largura ao share calculado pelo pai (__den_flex_share).
    //
    // Usa allocate_ui_with_layout com Layout::top_down pra que:
    // 1. O sub-UI tenha layout vertical — texto faz wrap em vez de estender.
    //    (ui.horizontal() herda layout horizontal; sem isso, labels estendem
    //     infinitamente e overflow persiste.)
    // 2. max_rect do sub-UI = __den_flex_share — Frame e filhos respeitam.
    if is_flex_auto_child {
        element_code = quote! {
            ui.allocate_ui_with_layout(
                egui::vec2(__den_flex_share, ui.available_height()),
                egui::Layout::top_down(egui::Align::Min),
                |ui| {
                    #element_code
                },
            );
        };
    }

    Ok(element_code)
}

fn build_inner(
    visual: &DenVisual,
    text_ts: &Option<proc_macro2::TokenStream>,
    children_code: &[proc_macro2::TokenStream],
    tag: &str,
    layout_index: usize,
    flex_info: Option<&[FlexChildInfo]>,
) -> proc_macro2::TokenStream {
    let text_expr = text_ts.as_ref().map(|ts| build_rich_text_expr(ts, visual));

    let inner = match tag {
        "heading" | "h1" | "h2" | "h3" => {
            if let Some(rt) = text_expr {
                quote! { ui.heading(#rt); }
            } else if !children_code.is_empty() {
                quote! { #( #children_code )* }
            } else {
                quote! {}
            }
        }
        _ => {
            let mut stmts = Vec::new();
            if let Some(rt) = text_expr {
                stmts.push(quote! { ui.label(#rt); });
            }
            for child in children_code {
                stmts.push(child.clone());
            }
            quote! { #( #stmts )* }
        }
    };

    let inner = if visual.display == DisplayMode::Flex {
        build_flex_layout(inner, flex_info)
    } else {
        inner
    };

    // Larguras explícitas (Px/Percent): layout system resolveu em CSS pixels,
    // multiplica pelo scale pra converter pra pixels físicos do egui.
    //
    // Percent: usa ui.available_width() inline — já desconta padding do frame pai.
    //   O layout system NÃO sabe sobre padding, então sizes[i] pra Percent
    //   seria a largura total do pai (sem desconto), causando overflow.
    //
    // Px: usa layout system — valor fixo, independente de padding.
    //   O layout system serve aqui pra flex distribution (saber quais
    //   filhos são fixos vs auto).
    //
    // Auto: não força largura — deixa o egui decidir pelo conteúdo.
    //   EXCETO filhos diretos de flex: usam __den_flex_share (calculado pelo pai).
    match visual.width {
        WidthValue::Percent(pct) => quote! {
            ui.set_width(ui.available_width() * #pct);
            #inner
        },
        WidthValue::Px(_) => quote! {
            if let Some(__lw) = __den_layout.sizes[#layout_index] {
                ui.set_width(__lw * __den_scale);
            }
            #inner
        },
        WidthValue::Auto => inner,
    }
}

/// Gera o layout horizontal (flex) com distribuição de largura por filho.
///
/// egui's `ui.horizontal()` não limita a largura dos filhos — conteúdo largo
/// transborda o container. CSS resolve com `flex-shrink: 1` (default). Aqui
/// simulamos calculando `__den_flex_share` em runtime: o espaço restante
/// (depois de filhos Px/Percent) dividido igualmente entre filhos Auto.
/// Cada filho Auto é envolvido em `ui.allocate_ui(__den_flex_share, ...)`
/// no `generate_element` (via `is_flex_auto_child`).
fn build_flex_layout(
    inner: proc_macro2::TokenStream,
    flex_info: Option<&[FlexChildInfo]>,
) -> proc_macro2::TokenStream {
    let Some(infos) = flex_info else {
        return quote! { ui.horizontal(|ui| { #inner }); };
    };

    let auto_count = infos.iter().filter(|i| i.width == WidthValue::Auto).count();

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


fn den_element_id(
    template_path: &str,
    tree_path: &[usize],
    tag: &str,
    classes: &[String],
) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    template_path.hash(&mut hasher);
    tree_path.hash(&mut hasher);
    tag.hash(&mut hasher);
    classes.hash(&mut hasher);
    hasher.finish()
}
