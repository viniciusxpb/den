//! Registro de slots de click handler e tradução de argumentos.
//!
//! Cada `(click)="handler(...)"` num template é atribuído a um slot `u32`.
//! O match de dispatch em `codegen/mod.rs` roteia `PaintEvent::Click{slot}`
//! pra chamada Rust apropriada (`self.handler(...)`).

use super::render_tree::BuildCtx;
use crate::types::DenElement;

/// Registra o handler de click deste elemento em `ctx.handlers`, se existir,
/// e retorna o slot atribuído.
pub(super) fn build_click_slot(el: &DenElement, ctx: &mut BuildCtx) -> Result<Option<u32>, String> {
    let Some(func_name) = &el.on_click else {
        return Ok(None);
    };

    // v1: só handlers sem args. Args dentro de `<for>` com `den-bind` ficam pra depois,
    // porque as variáveis do loop não existem no escopo do dispatch. Ver PENDING.md.
    if !el.on_click_args.is_empty() {
        return Err(
            "Den: (click) com argumentos ainda não suportado no renderer genérico. \
             Use handlers sem args por enquanto, ou evite argumentos dentro de <for>."
                .to_string(),
        );
    }

    let call: proc_macro2::TokenStream = format!("self.{func_name}()")
        .parse()
        .map_err(|e| format!("Invalid function name '{func_name}': {e}"))?;
    let slot = ctx.handlers.len() as u32;
    ctx.handlers.push(call);
    Ok(Some(slot))
}
