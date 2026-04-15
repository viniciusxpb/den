//! Construção de TokenStream pra conteúdo de texto (literais, interpolação, pipes).

use crate::types::{PipeCall, TextSegment};
use quote::quote;

/// Constrói TokenStream pro conteúdo de texto de um elemento.
///
/// - Só literals → string literal simples
/// - Com expressões → `format!()` call
/// - Expressões com pipes → `PipeName::transform(value, &["args"])` aninhado
pub fn build_text_token_stream(
    segments: &[TextSegment],
    has_self: bool,
) -> Result<Option<proc_macro2::TokenStream>, String> {
    if segments.is_empty() {
        return Ok(None);
    }

    let has_exprs = segments
        .iter()
        .any(|s| matches!(s, TextSegment::Expr { .. }));

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
            TextSegment::Expr { expr, pipes } => {
                fmt_string.push_str("{}");
                fmt_args.push(build_piped_value(expr, pipes)?);
            }
        }
    }

    Ok(Some(quote! { format!(#fmt_string, #( #fmt_args ),*) }))
}

/// Envolve `expr` com cada pipe da cadeia, da esquerda pra direita:
/// `self.x | upper | truncate(80)` → `Truncate::transform(Upper::transform(&self.x, &[]), &["80"])`.
///
/// O valor inicial é sempre passado por referência (`&(expr)`) pra não mover
/// fora de `self`. Pipes subsequentes recebem `String` da saída do anterior
/// por valor — sem problema, já é owned.
pub fn build_piped_value(
    expr: &str,
    pipes: &[PipeCall],
) -> Result<proc_macro2::TokenStream, String> {
    let expr_ts: proc_macro2::TokenStream = expr
        .parse()
        .map_err(|e| format!("Invalid expression `{expr}`: {e}"))?;

    if pipes.is_empty() {
        return Ok(expr_ts);
    }

    // Primeiro pipe: valor borrowed do campo.
    let mut current = quote! { &(#expr_ts) };
    for pipe in pipes {
        let pipe_type = pipe_struct_ident(&pipe.name)?;
        let args = &pipe.args;
        current = quote! {
            <#pipe_type as ::den_layout::Pipe<_>>::transform(
                #current,
                &[ #( #args ),* ]
            )
        };
    }
    Ok(current)
}

/// Mapeia nome no template → path do struct do pipe.
///
/// Built-ins vivem em `::den_layout::pipes::*`; custom devem ser exportados em
/// `crate::pipes::NomePascal` (convenção). Quando o pipe custom não existe,
/// o rustc vai reclamar do path inválido — a mensagem de erro inclui o path
/// esperado explicitamente pra facilitar.
fn pipe_struct_ident(name: &str) -> Result<proc_macro2::TokenStream, String> {
    if let Some(builtin) = builtin_pipe_struct(name) {
        return format!("::den_layout::pipes::{builtin}")
            .parse()
            .map_err(|e| format!("Pipe built-in `{name}` inválido: {e}"));
    }
    let pascal = to_pascal_case(name);
    format!("crate::pipes::{pascal}").parse().map_err(|e| {
        format!(
            "Pipe `{name}` (esperado em `crate::pipes::{pascal}`) gerou path inválido: {e}. \
             Defina `pub mod pipes {{ pub use path::to::{pascal}; }}` no crate root."
        )
    })
}

/// Retorna o nome do struct built-in correspondente ao nome no template.
/// `None` = pipe não é built-in, busca em `crate::pipes`.
fn builtin_pipe_struct(name: &str) -> Option<&'static str> {
    match name {
        "upper" => Some("Upper"),
        "lower" => Some("Lower"),
        "trim" => Some("Trim"),
        "truncate" => Some("Truncate"),
        "currency" => Some("Currency"),
        "money" => Some("Money"),
        "date" => Some("Date"),
        "number" => Some("Number"),
        "join" => Some("Join"),
        "default" => Some("OrDefault"), // nome `default` colidiria com std::default::Default
        _ => None,
    }
}

fn to_pascal_case(name: &str) -> String {
    let mut out = String::new();
    let mut upper_next = true;
    for c in name.chars() {
        if c == '_' || c == '-' {
            upper_next = true;
            continue;
        }
        if upper_next {
            out.extend(c.to_uppercase());
            upper_next = false;
        } else {
            out.push(c);
        }
    }
    out
}
