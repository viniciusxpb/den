//! Macro `#[den_page]` para declarar o contrato de rota de uma página.

use quote::quote;

/// Gera uma implementação de `DenPage<crate::AppRoute>` para a struct anotada.
pub fn expand(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item_struct = syn::parse_macro_input!(item as syn::ItemStruct);
    let name = &item_struct.ident;
    let page_name = name.to_string();

    let from_route_body = match &item_struct.fields {
        syn::Fields::Unit => quote! {
            match route {
                crate::AppRoute::#name => Some(Self),
                _ => None,
            }
        },
        syn::Fields::Named(fields) => {
            let field_names: Vec<_> = fields
                .named
                .iter()
                .filter_map(|field| field.ident.as_ref())
                .collect();
            quote! {
                match route {
                    crate::AppRoute::#name { #( #field_names ),* } => {
                        Some(Self {
                            #( #field_names: #field_names.clone() ),*
                        })
                    }
                    _ => None,
                }
            }
        }
        syn::Fields::Unnamed(_) => {
            return syn::Error::new_spanned(
                &item_struct,
                "Den: #[den_page] supports unit structs or structs with named fields.",
            )
            .to_compile_error()
            .into();
        }
    };

    quote! {
        #item_struct

        impl den_layout::DenPage<crate::AppRoute> for #name {
            fn page_name() -> &'static str {
                #page_name
            }

            fn from_route(route: &crate::AppRoute) -> Option<Self> {
                #from_route_body
            }

            fn render(
                &mut self,
                ui: &mut egui::Ui,
                __den_scale: f32,
                __den_router: &mut den_layout::DenRouter<crate::AppRoute>,
            ) {
                #name::render(self, ui, __den_scale, __den_router);
            }
        }
    }
    .into()
}
