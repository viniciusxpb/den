//! Macro `den_router!` para gerar enum de rotas e construtores tipados.

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};

struct RouterInput {
    routes: Vec<RouteDecl>,
}

struct RouteDecl {
    name: syn::Ident,
    fields: Vec<RouteField>,
}

struct RouteField {
    name: syn::Ident,
    ty: syn::Type,
}

impl Parse for RouterInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut routes = Vec::new();

        while !input.is_empty() {
            let name: syn::Ident = input.parse()?;
            let fields = if input.peek(syn::token::Brace) {
                let content;
                syn::braced!(content in input);
                let punct: syn::punctuated::Punctuated<RouteField, syn::Token![,]> =
                    content.parse_terminated(RouteField::parse, syn::Token![,])?;
                punct.into_iter().collect()
            } else {
                Vec::new()
            };
            routes.push(RouteDecl { name, fields });

            if input.peek(syn::Token![,]) {
                input.parse::<syn::Token![,]>()?;
            }
        }

        Ok(Self { routes })
    }
}

impl Parse for RouteField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        input.parse::<syn::Token![:]>()?;
        let ty: syn::Type = input.parse()?;
        Ok(Self { name, ty })
    }
}

/// Gera o enum `AppRoute` e funções auxiliares usadas pelo codegen de `goto`.
pub fn expand(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = syn::parse_macro_input!(input as RouterInput);

    let variants = parsed.routes.iter().map(|route| {
        let name = &route.name;
        if route.fields.is_empty() {
            quote! { #name }
        } else {
            let fields = route.fields.iter().map(|field| {
                let field_name = &field.name;
                let ty = &field.ty;
                quote! { #field_name: #ty }
            });
            quote! { #name { #( #fields ),* } }
        }
    });

    let helpers = parsed.routes.iter().map(build_route_helper);

    quote! {
        #[derive(Debug, Clone)]
        pub enum AppRoute {
            #( #variants ),*
        }

        #( #helpers )*
    }
    .into()
}

fn build_route_helper(route: &RouteDecl) -> TokenStream {
    let name = &route.name;
    let helper_name = format_ident!("__den_route_{}", name);

    if route.fields.is_empty() {
        return quote! {
            #[allow(non_snake_case)]
            pub fn #helper_name() -> AppRoute {
                AppRoute::#name
            }
        };
    }

    let params = route.fields.iter().map(|field| {
        let field_name = &field.name;
        let ty = &field.ty;
        quote! { #field_name: #ty }
    });
    let init_fields = route.fields.iter().map(|field| {
        let field_name = &field.name;
        quote! { #field_name }
    });

    quote! {
        #[allow(non_snake_case)]
        pub fn #helper_name(#( #params ),*) -> AppRoute {
            AppRoute::#name { #( #init_fields ),* }
        }
    }
}
