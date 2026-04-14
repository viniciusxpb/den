//! Geração de `<input bind="self.field" ...>`: sync route state ↔ page field
//! e emissão do `RenderKind::Input` token.
//!
//! Responsabilidades:
//! - Antes do push do nó: hidrata `DenRouteState.inputs` no primeiro render,
//!   ou puxa o valor atual do route state pro page field nos frames seguintes.
//! - Registra o "mirror" (código `if node_id == N { self.field = value; }`)
//!   pra ser executado quando chegar `PaintEvent::InputChanged` — garante
//!   que o page struct continua sendo fonte de verdade ergonômica.
//! - Emite o `RenderKind::Input { node_id, placeholder }`.

use super::render_tree::BuildCtx;
use crate::types::DenElement;
use quote::quote;

/// Código emitido ANTES do `__den_tree.push(...)` de um input. Faz a sincronização
/// route state → page field. Registra o mirror no `ctx.input_mirrors`.
///
/// Sem `bind_expr` → retorna statement vazio.
pub(super) fn emit_sync_pre(
    el: &DenElement,
    node_id_expr: &proc_macro2::TokenStream,
    ctx: &mut BuildCtx,
) -> Result<proc_macro2::TokenStream, String> {
    let Some(bind) = &el.bind_expr else {
        return Ok(quote! {});
    };
    let bind_tokens: proc_macro2::TokenStream = bind
        .parse()
        .map_err(|e| format!("Invalid bind expression '{bind}': {e}"))?;

    // Mirror: quando `PaintEvent::InputChanged { node_id: __ev_node_id, value: __ev_value }`
    // bate com o id deste input, escreve no page field também.
    let mirror = quote! {
        if __ev_node_id == den_layout::DenNodeId::new(#node_id_expr) {
            #bind_tokens = __ev_value.clone();
        }
    };
    ctx.input_mirrors.push(mirror);

    // Pré-push: hidrata se vazio; senão puxa pro self.field.
    Ok(quote! {
        let __den_input_id = den_layout::DenNodeId::new(#node_id_expr);
        if __den_route_state.inputs().get(__den_input_id).is_none() {
            __den_route_state
                .inputs_mut()
                .set(__den_input_id, (#bind_tokens).clone());
        } else {
            #bind_tokens = __den_route_state
                .inputs()
                .get(__den_input_id)
                .unwrap_or("")
                .to_string();
        }
    })
}

/// Emite o token do `RenderKind::Input { node_id, placeholder }` pro elemento.
/// Chamado pelo `kind_tokens` quando `el.bind_expr.is_some()`.
pub(super) fn emit_kind_tokens(el: &DenElement) -> proc_macro2::TokenStream {
    let placeholder_tokens = match &el.placeholder {
        Some(p) => quote! { Some(#p.to_string()) },
        None => quote! { None },
    };
    quote! {
        den_layout::RenderKind::Input {
            node_id: __den_node_id,
            placeholder: #placeholder_tokens,
        }
    }
}
