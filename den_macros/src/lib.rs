mod codegen;
mod input;
mod page;
mod parse;
mod resolve;
mod router;
mod types;

use proc_macro::TokenStream;

/// Macro que carrega um par de templates HTML + SCSS e gera código egui.
///
/// Uso:
/// ```rust,ignore
/// // Sem data binding:
/// den_template!("pages/home/home");
///
/// // Com data binding (habilita {{ this.field }} nos templates):
/// den_template!("pages/home/home", self);
/// ```
///
/// Interpolação usa sintaxe `{{ this.field }}`. A keyword `this` mapeia pra `self`.
/// Campos usados em templates precisam implementar `Display`.
#[proc_macro]
pub fn den_template(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as input::DenTemplateInput);
    let template_path = parsed.path.value();
    let has_self = parsed.has_self;

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let base = std::path::Path::new(&manifest_dir)
        .join("src")
        .join(&template_path);

    let html = match std::fs::read_to_string(base.with_extension("html")) {
        Ok(c) => c,
        Err(e) => {
            let msg = format!("Failed to read {}.html: {e}", base.display());
            return syn::Error::new(parsed.path.span(), msg)
                .to_compile_error()
                .into();
        }
    };
    let scss = match std::fs::read_to_string(base.with_extension("scss")) {
        Ok(c) => c,
        Err(e) => {
            let msg = format!("Failed to read {}.scss: {e}", base.display());
            return syn::Error::new(parsed.path.span(), msg)
                .to_compile_error()
                .into();
        }
    };

    // Fase 1: Parse
    let raw_nodes = parse::parse_html(&html);
    let style_map = parse::parse_scss(&scss);

    // Fase 2: Resolve (styles → DenVisual em cada nó)
    let den_nodes = resolve::resolve(&raw_nodes, &style_map);

    // Fase 3: Codegen (DenNode tree → TokenStream)
    match codegen::generate(&den_nodes, has_self, &template_path) {
        Ok(tokens) => tokens.into(),
        Err(msg) => syn::Error::new(parsed.path.span(), msg)
            .to_compile_error()
            .into(),
    }
}

#[proc_macro_attribute]
pub fn den_page(attr: TokenStream, item: TokenStream) -> TokenStream {
    page::expand(attr, item)
}

#[proc_macro]
pub fn den_router(input: TokenStream) -> TokenStream {
    router::expand(input)
}
