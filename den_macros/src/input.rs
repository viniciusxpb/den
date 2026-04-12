use syn::parse::{Parse, ParseStream};

pub struct DenTemplateInput {
    pub path: syn::LitStr,
    pub has_self: bool,
}

impl Parse for DenTemplateInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let path: syn::LitStr = input.parse()?;
        let has_self = if input.peek(syn::Token![,]) {
            input.parse::<syn::Token![,]>()?;
            input.parse::<syn::Token![self]>()?;
            true
        } else {
            false
        };
        Ok(Self { path, has_self })
    }
}
