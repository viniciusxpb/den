//! Geração de código para navegação declarada com `goto`.

use crate::types::DenElement;
use quote::quote;

/// Gera a chamada de navegação para elementos com `goto`.
pub fn generate_goto_call(el: &DenElement) -> Result<Option<proc_macro2::TokenStream>, String> {
    let Some(page) = &el.goto_page else {
        if el.goto_with.is_some() {
            return Err("Den: `with` attribute requires `goto` on the same element.".to_string());
        }
        return Ok(None);
    };

    if el.on_click.is_some() {
        return Err(
            "Den: `goto` and `(click)` cannot be used on the same element. Use one action."
                .to_string(),
        );
    }

    let helper_name: proc_macro2::TokenStream = format!("crate::__den_route_{page}")
        .parse()
        .map_err(|e| format!("Invalid goto page '{page}': {e}"))?;

    let args = match &el.goto_with {
        Some(expr) => split_goto_args(expr)?
            .into_iter()
            .map(|arg| {
                let tokens: proc_macro2::TokenStream = arg
                    .parse()
                    .map_err(|e| format!("Invalid goto `with` expression `{arg}`: {e}"))?;
                Ok(quote! { (#tokens).clone() })
            })
            .collect::<Result<Vec<_>, String>>()?,
        None => Vec::new(),
    };

    Ok(Some(quote! {
        __den_router.goto(#helper_name(#( #args ),*));
    }))
}

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
