//! Derive `#[derive(DenGhost)]` — implementa `DenGhost::ghost()` com mocks por campo.
//!
//! Cada campo recebe:
//! - `#[ghost("literal")]` → parse do literal no tipo do campo (via `FromStr`) ou `String::from`.
//! - `#[ghost(expr)]` → expressão Rust arbitrária (não-string).
//! - Nada → `Default::default()`.
//!
//! Tipos suportados no atalho `#[ghost("...")]`:
//! - `String` → `.to_string()`
//! - `&'static str` / `&str` → o literal direto
//! - `i*`/`u*`/`f*` → `.parse().unwrap()` do literal
//! - `bool` → `.parse().unwrap()`
//! - qualquer outro → assume que o literal é uma expressão Rust e injeta como tal

use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Lit, LitStr, Meta, Type, parse_macro_input};

pub fn expand(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;

    let Data::Struct(data) = &ast.data else {
        return syn::Error::new_spanned(&ast, "DenGhost só funciona em structs")
            .to_compile_error()
            .into();
    };

    let Fields::Named(named) = &data.fields else {
        return syn::Error::new_spanned(
            &ast,
            "DenGhost só funciona em structs com campos nomeados",
        )
        .to_compile_error()
        .into();
    };

    let field_inits = named.named.iter().map(|f| {
        let ident = f.ident.as_ref().expect("field named");
        let ty = &f.ty;
        let ghost_attr = f.attrs.iter().find(|a| a.path().is_ident("ghost"));
        let value = match ghost_attr {
            None => quote! { ::core::default::Default::default() },
            Some(attr) => match extract_ghost_value(attr, ty) {
                Ok(ts) => ts,
                Err(e) => return e.to_compile_error(),
            },
        };
        quote! { #ident: #value }
    });

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::den_layout::DenGhost for #name #ty_generics #where_clause {
            fn ghost() -> Self {
                Self {
                    #( #field_inits ),*
                }
            }
        }
    };

    expanded.into()
}

/// Converte o conteúdo de `#[ghost(...)]` em uma expressão Rust, considerando o tipo do campo.
fn extract_ghost_value(
    attr: &syn::Attribute,
    ty: &Type,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    // Formato: `#[ghost("literal")]` ou `#[ghost(expr)]`
    let Meta::List(list) = &attr.meta else {
        return Err(syn::Error::new_spanned(
            attr,
            "ghost attr requer argumento: #[ghost(\"valor\")]",
        ));
    };

    // Tenta parsear como string literal primeiro.
    if let Ok(lit_str) = list.parse_args::<LitStr>() {
        return Ok(coerce_string_to_type(&lit_str, ty));
    }

    // Se não for string literal, tenta como literal puro (number, bool).
    if let Ok(lit) = list.parse_args::<Lit>() {
        return Ok(quote! { #lit });
    }

    // Fallback: expressão Rust arbitrária.
    let expr: syn::Expr = list.parse_args().map_err(|e| {
        syn::Error::new_spanned(
            attr,
            format!("ghost attr deve ser literal ou expressão: {e}"),
        )
    })?;
    Ok(quote! { #expr })
}

/// Dado um string literal `#[ghost("X")]` e o tipo do campo, gera a conversão apropriada.
///
/// Pra tipos numéricos, emite um literal com sufixo de tipo (`42u8`, `3.14f64`) em vez de
/// `(42.0f64) as u8` — evita clippy `cast_possible_truncation` e fica mais legível.
fn coerce_string_to_type(lit: &LitStr, ty: &Type) -> proc_macro2::TokenStream {
    let type_str = type_name(ty);
    let value = lit.value();

    match type_str.as_str() {
        "String" => quote! { #lit.to_string() },
        "&str" | "& str" => quote! { #lit },
        "bool" => {
            let b: bool = value.parse().unwrap_or(false);
            quote! { #b }
        }
        t if is_float(t) => match value.parse::<f64>() {
            Ok(n) if t == "f32" => {
                let lit_n = proc_macro2::Literal::f32_suffixed(n as f32);
                quote! { #lit_n }
            }
            Ok(n) => {
                let lit_n = proc_macro2::Literal::f64_suffixed(n);
                quote! { #lit_n }
            }
            Err(_) => quote! { ::core::default::Default::default() },
        },
        t if is_integer(t) => match emit_integer_literal(&value, t) {
            Some(ts) => ts,
            None => quote! { ::core::default::Default::default() },
        },
        _ => {
            // Tipo desconhecido: assume que o literal é uma expressão Rust.
            match value.parse::<proc_macro2::TokenStream>() {
                Ok(ts) => ts,
                Err(_) => quote! { ::core::default::Default::default() },
            }
        }
    }
}

/// Emite um literal inteiro tipado (`42u8`, `-5i32`, ...). Valida range de forma
/// leve e deixa o rustc reclamar se passar do limite.
fn emit_integer_literal(value: &str, ty: &str) -> Option<proc_macro2::TokenStream> {
    let n: i128 = value.parse().ok()?;
    let lit = match ty {
        "i8" => proc_macro2::Literal::i8_suffixed(n as i8),
        "i16" => proc_macro2::Literal::i16_suffixed(n as i16),
        "i32" => proc_macro2::Literal::i32_suffixed(n as i32),
        "i64" => proc_macro2::Literal::i64_suffixed(n as i64),
        "i128" => proc_macro2::Literal::i128_suffixed(n),
        "isize" => proc_macro2::Literal::isize_suffixed(n as isize),
        "u8" => proc_macro2::Literal::u8_suffixed(n as u8),
        "u16" => proc_macro2::Literal::u16_suffixed(n as u16),
        "u32" => proc_macro2::Literal::u32_suffixed(n as u32),
        "u64" => proc_macro2::Literal::u64_suffixed(n as u64),
        "u128" => proc_macro2::Literal::u128_suffixed(n as u128),
        "usize" => proc_macro2::Literal::usize_suffixed(n as usize),
        _ => return None,
    };
    Some(quote! { #lit })
}

fn is_float(t: &str) -> bool {
    matches!(t, "f32" | "f64")
}

fn is_integer(t: &str) -> bool {
    matches!(
        t,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
    )
}

fn type_name(ty: &Type) -> String {
    match ty {
        Type::Path(p) => {
            let segs: Vec<String> = p
                .path
                .segments
                .iter()
                .map(|s| s.ident.to_string())
                .collect();
            segs.join("::")
        }
        Type::Reference(r) => {
            let inner = type_name(&r.elem);
            format!("&{inner}")
        }
        _ => String::new(),
    }
}

