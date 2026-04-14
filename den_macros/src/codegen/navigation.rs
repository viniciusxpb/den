//! Registro de slots de navegação `goto` e parsing de argumentos `with`.

use super::render_tree::BuildCtx;
use crate::types::DenElement;
use quote::quote;

/// Registra a navegação `goto` deste elemento em `ctx.goto_slots`, se existir,
/// e retorna o slot atribuído. Também valida a combinação com `(click)`.
pub(super) fn build_goto_slot(
    el: &DenElement,
    ctx: &mut BuildCtx,
) -> Result<Option<u32>, String> {
    let Some(page) = &el.goto_page else {
        if el.goto_with.is_some() {
            return Err("Den: `with` requires `goto` on the same element.".to_string());
        }
        return Ok(None);
    };
    if el.on_click.is_some() {
        return Err(
            "Den: `goto` and `(click)` cannot be used on the same element.".to_string(),
        );
    }

    let helper: proc_macro2::TokenStream = format!("crate::__den_route_{page}")
        .parse()
        .map_err(|e| format!("Invalid goto page '{page}': {e}"))?;

    let args: Vec<proc_macro2::TokenStream> = match &el.goto_with {
        Some(expr) => split_goto_args(expr)?
            .into_iter()
            .map(|arg| {
                let tokens: proc_macro2::TokenStream = arg
                    .parse()
                    .map_err(|e| format!("Invalid goto `with` expression '{arg}': {e}"))?;
                Ok(quote! { (#tokens).clone() })
            })
            .collect::<Result<Vec<_>, String>>()?,
        None => Vec::new(),
    };

    let nav = quote! { __den_router.goto(#helper(#( #args ),*)); };
    let slot = ctx.goto_slots.len() as u32;
    ctx.goto_slots.push(nav);
    Ok(Some(slot))
}

/// Quebra a expressão de `with="..."` em argumentos separados, respeitando
/// aninhamento de parênteses/colchetes/chaves.
fn split_goto_args(expr: &str) -> Result<Vec<String>, String> {
    let trimmed = expr.trim();
    if trimmed.is_empty() {
        return Ok(Vec::new());
    }
    if !(trimmed.starts_with('(') && trimmed.ends_with(')')) {
        return Ok(vec![trimmed.to_string()]);
    }
    let inner = &trimmed[1..trimmed.len() - 1];
    let mut args = Vec::new();
    let mut current = String::new();
    let mut depth = 0i32;
    for ch in inner.chars() {
        match ch {
            '(' | '[' | '{' => {
                depth += 1;
                current.push(ch);
            }
            ')' | ']' | '}' => {
                depth -= 1;
                if depth < 0 {
                    return Err(format!("Den: invalid `with` expression `{expr}`."));
                }
                current.push(ch);
            }
            ',' if depth == 0 => {
                let part = current.trim();
                if !part.is_empty() {
                    args.push(part.to_string());
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if depth != 0 {
        return Err(format!("Den: invalid `with` expression `{expr}`."));
    }
    let part = current.trim();
    if !part.is_empty() {
        args.push(part.to_string());
    }
    Ok(args)
}
