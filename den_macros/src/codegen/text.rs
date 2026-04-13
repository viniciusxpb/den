//! Construção de TokenStream pra conteúdo de texto (literais e interpolação).

use crate::types::TextSegment;
use quote::quote;

/// Constrói TokenStream pro conteúdo de texto de um elemento.
///
/// - Só literals → string literal simples
/// - Com expressões → `format!()` call
pub fn build_text_token_stream(
    segments: &[TextSegment],
    has_self: bool,
) -> Result<Option<proc_macro2::TokenStream>, String> {
    if segments.is_empty() {
        return Ok(None);
    }

    let has_exprs = segments.iter().any(|s| matches!(s, TextSegment::Expr(_)));

    if !has_exprs {
        let full: String = segments
            .iter()
            .map(|s| match s {
                TextSegment::Literal(l) => l.as_str(),
                _ => "",
            })
            .collect();
        return if full.is_empty() {
            Ok(None)
        } else {
            Ok(Some(quote! { #full }))
        };
    }

    if !has_self {
        return Err(
            "Template uses {{ expr }} interpolation but `self` was not passed to den_template!. \
             Use: den_template!(\"path\", self);"
                .to_string(),
        );
    }

    let mut fmt_string = String::new();
    let mut fmt_args: Vec<proc_macro2::TokenStream> = Vec::new();

    for seg in segments {
        match seg {
            TextSegment::Literal(lit) => {
                fmt_string.push_str(&lit.replace('{', "{{").replace('}', "}}"));
            }
            TextSegment::Expr(expr) => {
                fmt_string.push_str("{}");
                let tokens: proc_macro2::TokenStream = expr
                    .parse()
                    .map_err(|e| format!("Invalid expression `{expr}`: {e}"))?;
                fmt_args.push(tokens);
            }
        }
    }

    Ok(Some(quote! { format!(#fmt_string, #( #fmt_args ),*) }))
}
