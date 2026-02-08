use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;
use syn::parse::{Parse, ParseStream};

/// Macro that loads an HTML + SCSS template pair and generates egui code.
///
/// Usage:
/// ```rust
/// // Without data binding:
/// den_template!("pages/home/home");
///
/// // With data binding (enables {{ this.field }} in templates):
/// den_template!("pages/home/home", self);
/// ```
///
/// Template interpolation uses `{{ this.field }}` syntax. The `this` keyword
/// maps to `self` in Rust. Fields used in templates must implement `Display`.
///
/// This reads `pages/home/home.html` and `pages/home/home.scss` relative to
/// the crate's `src/` directory and generates the corresponding egui widget calls.
#[proc_macro]
pub fn den_template(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as DenTemplateInput);
    let template_path = parsed.path.value();
    let has_self = parsed.has_self;

    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let base = std::path::Path::new(&manifest_dir).join("src").join(&template_path);

    let html_path = base.with_extension("html");
    let scss_path = base.with_extension("scss");

    let html = match std::fs::read_to_string(&html_path) {
        Ok(content) => content,
        Err(e) => {
            let msg = format!("Failed to read {}: {e}", html_path.display());
            return syn::Error::new(parsed.path.span(), msg)
                .to_compile_error()
                .into();
        }
    };

    let scss = match std::fs::read_to_string(&scss_path) {
        Ok(content) => content,
        Err(e) => {
            let msg = format!("Failed to read {}: {e}", scss_path.display());
            return syn::Error::new(parsed.path.span(), msg)
                .to_compile_error()
                .into();
        }
    };

    let styles = parse_scss(&scss);
    let elements = parse_html(&html);

    match generate_egui_code(&elements, &styles, has_self) {
        Ok(tokens) => tokens.into(),
        Err(msg) => syn::Error::new(parsed.path.span(), msg)
            .to_compile_error()
            .into(),
    }
}

// ---------------------------------------------------------------------------
// Macro input parsing with syn (Fix #2)
// ---------------------------------------------------------------------------

struct DenTemplateInput {
    path: syn::LitStr,
    has_self: bool,
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

// ---------------------------------------------------------------------------
// HTML parser (minimal, hand-rolled, UTF-8 safe — Fix #5)
// ---------------------------------------------------------------------------

/// A segment of text content — either a literal string or a `{{ expr }}` interpolation.
#[derive(Debug, Clone)]
enum TextSegment {
    Literal(String),
    Expr(String),
}

#[derive(Debug)]
struct HtmlElement {
    tag: String,
    classes: Vec<String>,
    segments: Vec<TextSegment>,
    children: Vec<HtmlElement>,
}

/// Map `this` to `self` only as a keyword, not inside identifiers. (Fix #4)
/// `this.name` -> `self.name`, but `this_value` stays `this_value`.
fn map_this_to_self(expr: &str) -> String {
    let expr = expr.trim();
    if expr == "this" {
        return "self".to_string();
    }

    let mut result = String::with_capacity(expr.len());
    let chars: Vec<char> = expr.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if i + 4 <= chars.len() && chars[i..i + 4] == ['t', 'h', 'i', 's'] {
            let before_ok = i == 0 || !(chars[i - 1].is_alphanumeric() || chars[i - 1] == '_');
            let after_ok = i + 4 >= chars.len()
                || chars[i + 4] == '.'
                || !(chars[i + 4].is_alphanumeric() || chars[i + 4] == '_');

            if before_ok && after_ok {
                result.push_str("self");
                i += 4;
                continue;
            }
        }
        result.push(chars[i]);
        i += 1;
    }

    result
}

/// Parse raw text content into segments, splitting on `{{ expr }}` patterns.
fn parse_text_segments(raw: &str) -> Vec<TextSegment> {
    let mut segments = Vec::new();
    let mut rest = raw;

    while let Some(start) = rest.find("{{") {
        let before = &rest[..start];
        if !before.is_empty() {
            segments.push(TextSegment::Literal(before.to_string()));
        }
        let after_open = &rest[start + 2..];
        if let Some(end) = after_open.find("}}") {
            let expr = after_open[..end].trim().to_string();
            let expr = map_this_to_self(&expr);
            if !expr.is_empty() {
                segments.push(TextSegment::Expr(expr));
            }
            rest = &after_open[end + 2..];
        } else {
            // No closing }}, treat rest as literal
            segments.push(TextSegment::Literal(rest.to_string()));
            return segments;
        }
    }

    if !rest.is_empty() {
        segments.push(TextSegment::Literal(rest.to_string()));
    }

    segments
}

// All HTML parsing now operates on `Vec<char>` for proper UTF-8 support (Fix #5).

fn parse_html(input: &str) -> Vec<HtmlElement> {
    let input = input.trim();
    let chars: Vec<char> = input.chars().collect();
    let mut pos = 0;
    let mut elements = Vec::new();

    while pos < chars.len() {
        skip_ws(&chars, &mut pos);
        if pos >= chars.len() {
            break;
        }
        if chars[pos] == '<' {
            if let Some(el) = parse_element_chars(&chars, &mut pos) {
                elements.push(el);
            }
        } else {
            pos += 1;
        }
    }
    elements
}

fn parse_element_chars(chars: &[char], pos: &mut usize) -> Option<HtmlElement> {
    if chars[*pos] != '<' {
        return None;
    }
    *pos += 1; // skip '<'

    // Read tag name
    skip_ws(chars, pos);
    let tag = read_ident(chars, pos);
    if tag.is_empty() {
        return None;
    }

    // Read attributes
    let mut classes = Vec::new();
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        let attr_name = read_ident(chars, pos);
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '=' {
            *pos += 1; // skip '='
            skip_ws(chars, pos);
            let value = read_quoted(chars, pos);
            if attr_name == "class" {
                classes = value.split_whitespace().map(|s| s.to_string()).collect();
            }
        }
        skip_ws(chars, pos);
    }

    // Skip self-closing or '>'
    if *pos < chars.len() && chars[*pos] == '/' {
        *pos += 1;
        if *pos < chars.len() && chars[*pos] == '>' {
            *pos += 1;
        }
        return Some(HtmlElement {
            tag,
            classes,
            segments: Vec::new(),
            children: Vec::new(),
        });
    }
    if *pos < chars.len() && chars[*pos] == '>' {
        *pos += 1;
    }

    // Read content (text + children)
    let mut raw_text = String::new();
    let mut children = Vec::new();

    while *pos < chars.len() {
        if chars[*pos] == '<' {
            if *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
                // Closing tag — skip past '>'
                while *pos < chars.len() && chars[*pos] != '>' {
                    *pos += 1;
                }
                if *pos < chars.len() {
                    *pos += 1; // skip '>'
                }
                break;
            } else if let Some(child) = parse_element_chars(chars, pos) {
                children.push(child);
            }
        } else {
            raw_text.push(chars[*pos]);
            *pos += 1;
        }
    }

    let trimmed = raw_text.trim().to_string();
    let segments = parse_text_segments(&trimmed);

    Some(HtmlElement {
        tag,
        classes,
        segments,
        children,
    })
}

fn skip_ws(chars: &[char], pos: &mut usize) {
    while *pos < chars.len() && chars[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn read_ident(chars: &[char], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < chars.len()
        && (chars[*pos].is_ascii_alphanumeric() || chars[*pos] == '_' || chars[*pos] == '-')
    {
        *pos += 1;
    }
    chars[start..*pos].iter().collect()
}

fn read_quoted(chars: &[char], pos: &mut usize) -> String {
    if *pos >= chars.len() {
        return String::new();
    }
    let quote_char = chars[*pos];
    if quote_char != '"' && quote_char != '\'' {
        return read_ident(chars, pos);
    }
    *pos += 1; // skip opening quote
    let start = *pos;
    while *pos < chars.len() && chars[*pos] != quote_char {
        *pos += 1;
    }
    let val: String = chars[start..*pos].iter().collect();
    if *pos < chars.len() {
        *pos += 1; // skip closing quote
    }
    val
}

// ---------------------------------------------------------------------------
// SCSS parser (minimal, hand-rolled)
// SCSS identifiers are ASCII-only, so byte-level parsing is safe here.
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct StyleRule {
    properties: HashMap<String, String>,
}

fn parse_scss(input: &str) -> HashMap<String, StyleRule> {
    let mut styles = HashMap::new();
    let input = input.trim();
    let bytes = input.as_bytes();
    let mut pos = 0;

    while pos < bytes.len() {
        skip_whitespace(bytes, &mut pos);
        if pos >= bytes.len() {
            break;
        }

        // Expect '.'
        if bytes[pos] != b'.' {
            pos += 1;
            continue;
        }
        pos += 1; // skip '.'

        // Read class name
        let class_name = read_identifier(bytes, &mut pos);
        if class_name.is_empty() {
            continue;
        }

        skip_whitespace(bytes, &mut pos);

        // Expect '{'
        if pos >= bytes.len() || bytes[pos] != b'{' {
            continue;
        }
        pos += 1; // skip '{'

        // Read properties until '}'
        let mut properties = HashMap::new();
        loop {
            skip_whitespace(bytes, &mut pos);
            if pos >= bytes.len() || bytes[pos] == b'}' {
                if pos < bytes.len() {
                    pos += 1; // skip '}'
                }
                break;
            }

            let prop_name = read_css_identifier(bytes, &mut pos);
            skip_whitespace(bytes, &mut pos);

            // Expect ':'
            if pos >= bytes.len() || bytes[pos] != b':' {
                continue;
            }
            pos += 1; // skip ':'
            skip_whitespace(bytes, &mut pos);

            // Read value until ';' or '}'
            let start = pos;
            while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'}' {
                pos += 1;
            }
            let value = String::from_utf8_lossy(&bytes[start..pos]).trim().to_string();

            if pos < bytes.len() && bytes[pos] == b';' {
                pos += 1; // skip ';'
            }

            properties.insert(prop_name, value);
        }

        styles.insert(class_name, StyleRule { properties });
    }

    styles
}

fn skip_whitespace(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn read_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len()
        && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'_' || bytes[*pos] == b'-')
    {
        *pos += 1;
    }
    String::from_utf8_lossy(&bytes[start..*pos]).to_string()
}

fn read_css_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len()
        && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'-' || bytes[*pos] == b'_')
    {
        *pos += 1;
    }
    String::from_utf8_lossy(&bytes[start..*pos]).to_string()
}

// ---------------------------------------------------------------------------
// Hex color parser
// ---------------------------------------------------------------------------

fn parse_hex_color(hex: &str) -> (u8, u8, u8) {
    let hex = hex.trim_start_matches('#');
    let hex = if hex.len() == 3 {
        let mut expanded = String::with_capacity(6);
        for c in hex.chars() {
            expanded.push(c);
            expanded.push(c);
        }
        expanded
    } else {
        hex.to_string()
    };

    let r = u8::from_str_radix(&hex[0..2], 16).unwrap_or(255);
    let g = u8::from_str_radix(&hex[2..4], 16).unwrap_or(255);
    let b = u8::from_str_radix(&hex[4..6], 16).unwrap_or(255);
    (r, g, b)
}

// ---------------------------------------------------------------------------
// Code generator: HTML + SCSS -> egui TokenStream
// All functions return Result to propagate errors as compile_error! (Fix #3)
// ---------------------------------------------------------------------------

fn generate_egui_code(
    elements: &[HtmlElement],
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
) -> Result<proc_macro2::TokenStream, String> {
    let mut stmts = Vec::new();
    for el in elements {
        stmts.push(generate_element(el, styles, has_self)?);
    }

    Ok(quote! {
        #( #stmts )*
    })
}

/// Build a token stream for the text content of an element.
///
/// If all segments are literals, produces a simple string literal.
/// If there are expression segments (e.g. `{{ this.name }}`), produces a `format!()` call.
/// Fields used in interpolation must implement `Display`.
fn build_text_token_stream(
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
        if full.is_empty() {
            return Ok(None);
        }
        return Ok(Some(quote! { #full }));
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
                let escaped = lit.replace('{', "{{").replace('}', "}}");
                fmt_string.push_str(&escaped);
            }
            TextSegment::Expr(expr) => {
                fmt_string.push_str("{}");
                let expr_tokens: proc_macro2::TokenStream = expr
                    .parse()
                    .map_err(|e| format!("Invalid expression `{expr}`: {e}"))?;
                fmt_args.push(expr_tokens);
            }
        }
    }

    Ok(Some(quote! { format!(#fmt_string, #( #fmt_args ),*) }))
}

fn generate_element(
    el: &HtmlElement,
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
) -> Result<proc_macro2::TokenStream, String> {
    // Merge styles from all classes
    let mut color: Option<(u8, u8, u8)> = None;
    let mut font_size: Option<f32> = None;
    let mut background: Option<(u8, u8, u8)> = None;
    let mut padding: Option<f32> = None;

    for class in &el.classes {
        if let Some(rule) = styles.get(class) {
            if let Some(c) = rule.properties.get("color") {
                color = Some(parse_hex_color(c));
            }
            if let Some(fs) = rule.properties.get("font-size")
                && let Ok(v) = fs.trim_end_matches("px").parse::<f32>()
            {
                font_size = Some(v);
            }
            if let Some(bg) = rule.properties.get("background") {
                background = Some(parse_hex_color(bg));
            }
            if let Some(p) = rule.properties.get("padding")
                && let Ok(v) = p.trim_end_matches("px").parse::<f32>()
            {
                padding = Some(v);
            }
        }
    }

    // Generate children code
    let mut children_code = Vec::new();
    for child in &el.children {
        children_code.push(generate_element(child, styles, has_self)?);
    }

    // Build text content from segments
    let text_ts = build_text_token_stream(&el.segments, has_self)?;
    let has_children = !children_code.is_empty();

    // Build RichText with applied styles
    let text_expr = if let Some(ts) = text_ts {
        let mut rt = quote! { egui::RichText::new(#ts) };
        if let Some((r, g, b)) = color {
            rt = quote! { #rt.color(egui::Color32::from_rgb(#r, #g, #b)) };
        }
        if let Some(size) = font_size {
            rt = quote! { #rt.size(#size) };
        }
        Some(rt)
    } else {
        None
    };

    // Generate based on tag
    let tag = el.tag.as_str();
    let needs_frame = background.is_some() || padding.is_some();

    let inner_code = match tag {
        "heading" | "h1" | "h2" | "h3" => {
            if let Some(rt) = text_expr {
                quote! { ui.heading(#rt); }
            } else if has_children {
                quote! { #( #children_code )* }
            } else {
                quote! {}
            }
        }
        _ => {
            // Fix #9: removed no-op empty element block
            let mut stmts = Vec::new();
            if let Some(rt) = text_expr {
                stmts.push(quote! { ui.label(#rt); });
            }
            for child in &children_code {
                stmts.push(child.clone());
            }
            quote! { #( #stmts )* }
        }
    };

    Ok(if needs_frame {
        let mut frame_expr = quote! { egui::Frame::default() };
        if let Some((r, g, b)) = background {
            frame_expr = quote! {
                #frame_expr.fill(egui::Color32::from_rgb(#r, #g, #b))
            };
        }
        if let Some(p) = padding {
            frame_expr = quote! {
                #frame_expr.inner_margin(#p)
            };
        }
        quote! {
            #frame_expr.show(ui, |ui| {
                #inner_code
            });
        }
    } else {
        inner_code
    })
}
