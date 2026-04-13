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

/// Gera o enum `AppRoute`, funções auxiliares e o host `AppPages`.
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
    let initial_route = parsed.routes.first().map(|route| &route.name);
    let Some(initial_route) = initial_route else {
        return syn::Error::new(
            proc_macro2::Span::call_site(),
            "Den: den_router! requires at least one page.",
        )
        .to_compile_error()
        .into();
    };

    let app_pages = build_app_pages(&parsed.routes);

    quote! {
        #[derive(Debug, Clone)]
        pub enum AppRoute {
            #( #variants ),*
        }

        #( #helpers )*

        /// Retorna a rota inicial do app.
        pub fn initial_route() -> AppRoute {
            AppRoute::#initial_route
        }

        #app_pages
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

fn build_app_pages(routes: &[RouteDecl]) -> TokenStream {
    let fields = routes.iter().map(|route| {
        let page_field = page_field_ident(&route.name);
        let name = &route.name;
        if route.fields.is_empty() {
            quote! { #page_field: #name }
        } else {
            quote! { #page_field: Option<#name> }
        }
    });

    let init_fields = routes.iter().map(|route| {
        let page_field = page_field_ident(&route.name);
        if route.fields.is_empty() {
            quote! { #page_field: ::std::default::Default::default() }
        } else {
            quote! { #page_field: None }
        }
    });

    let sync_arms = routes
        .iter()
        .filter(|route| !route.fields.is_empty())
        .map(|route| {
            let page_field = page_field_ident(&route.name);
            let name = &route.name;
            quote! {
                AppRoute::#name { .. } => {
                    self.#page_field = <#name as den_layout::DenPage<AppRoute>>::from_route(route);
                }
            }
        });

    let render_arms = routes.iter().map(|route| {
        let page_field = page_field_ident(&route.name);
        let name = &route.name;
        if route.fields.is_empty() {
            quote! {
                AppRoute::#name => {
                    self.#page_field.render(ui, scale, router);
                }
            }
        } else {
            quote! {
                AppRoute::#name { .. } => {
                    if self.#page_field.is_none() {
                        self.#page_field =
                            <#name as den_layout::DenPage<AppRoute>>::from_route(&current);
                    }
                    if let Some(page) = &mut self.#page_field {
                        page.render(ui, scale, router);
                    }
                }
            }
        }
    });

    quote! {
        /// Estado das páginas instanciadas pelo router.
        pub struct AppPages {
            #( #fields ),*
        }

        impl AppPages {
            /// Cria o conjunto de páginas do app.
            pub fn new() -> Self {
                Self {
                    #( #init_fields ),*
                }
            }

            /// Sincroniza páginas stateful quando uma rota nova entra.
            pub fn sync_from_route(&mut self, route: &AppRoute) {
                match route {
                    #( #sync_arms )*
                    _ => {}
                }
            }

            /// Renderiza a página correspondente à rota atual.
            pub fn render_current(
                &mut self,
                ui: &mut eframe::egui::Ui,
                scale: f32,
                router: &mut den_layout::DenRouter<AppRoute>,
            ) {
                let current = router.current().clone();
                match current {
                    #( #render_arms )*
                }
            }
        }

        impl Default for AppPages {
            fn default() -> Self {
                Self::new()
            }
        }
    }
}

fn page_field_ident(page_name: &syn::Ident) -> syn::Ident {
    format_ident!("{}", to_snake_case(&page_name.to_string()))
}

fn to_snake_case(input: &str) -> String {
    let mut out = String::new();
    for (idx, ch) in input.chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if idx > 0 {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push(ch);
        }
    }
    out
}
