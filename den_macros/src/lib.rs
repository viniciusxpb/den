use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;

/// Macro that loads an HTML + SCSS template pair and generates egui code.
///
/// Usage: `den_template!("pages/home/home");`
///
/// This reads `pages/home/home.html` and `pages/home/home.scss` relative to
/// the crate's `src/` directory and generates the corresponding egui widget calls.
#[proc_macro]
pub fn den_template(input: TokenStream) -> TokenStream {
    let input_str = input.to_string();
    let template_path = input_str.trim().trim_matches('"');

    let manifest_dir =
        std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let base = std::path::Path::new(&manifest_dir).join("src").join(template_path);

    let html_path = base.with_extension("html");
    let scss_path = base.with_extension("scss");

    let html = std::fs::read_to_string(&html_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", html_path.display()));
    let scss = std::fs::read_to_string(&scss_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {e}", scss_path.display()));

    let styles = parse_scss(&scss);
    let elements = parse_html(&html);
    let generated = generate_egui_code(&elements, &styles);

    generated.into()
}

// ---------------------------------------------------------------------------
// HTML parser (minimal, hand-rolled)
// ---------------------------------------------------------------------------

#[derive(Debug)]
struct HtmlElement {
    tag: String,
    classes: Vec<String>,
    text: String,
    children: Vec<HtmlElement>,
}

fn parse_html(input: &str) -> Vec<HtmlElement> {
    let input = input.trim();
    let mut pos = 0;
    let bytes = input.as_bytes();
    let mut elements = Vec::new();

    while pos < bytes.len() {
        skip_whitespace(bytes, &mut pos);
        if pos >= bytes.len() {
            break;
        }
        if bytes[pos] == b'<' {
            if let Some(el) = parse_element(input, &mut pos) {
                elements.push(el);
            }
        } else {
            pos += 1;
        }
    }
    elements
}

fn parse_element(input: &str, pos: &mut usize) -> Option<HtmlElement> {
    let bytes = input.as_bytes();
    if bytes[*pos] != b'<' {
        return None;
    }
    *pos += 1; // skip '<'

    // Read tag name
    skip_whitespace(bytes, pos);
    let tag = read_identifier(bytes, pos);
    if tag.is_empty() {
        return None;
    }

    // Read attributes
    let mut classes = Vec::new();
    skip_whitespace(bytes, pos);
    while *pos < bytes.len() && bytes[*pos] != b'>' && bytes[*pos] != b'/' {
        let attr_name = read_identifier(bytes, pos);
        skip_whitespace(bytes, pos);
        if *pos < bytes.len() && bytes[*pos] == b'=' {
            *pos += 1; // skip '='
            skip_whitespace(bytes, pos);
            let value = read_quoted_value(bytes, pos);
            if attr_name == "class" {
                classes = value.split_whitespace().map(|s| s.to_string()).collect();
            }
        }
        skip_whitespace(bytes, pos);
    }

    // Skip self-closing or '>'
    if *pos < bytes.len() && bytes[*pos] == b'/' {
        *pos += 1;
        if *pos < bytes.len() && bytes[*pos] == b'>' {
            *pos += 1;
        }
        return Some(HtmlElement {
            tag,
            classes,
            text: String::new(),
            children: Vec::new(),
        });
    }
    if *pos < bytes.len() && bytes[*pos] == b'>' {
        *pos += 1;
    }

    // Read content (text + children)
    let mut text = String::new();
    let mut children = Vec::new();

    while *pos < bytes.len() {
        if bytes[*pos] == b'<' {
            // Check for closing tag
            if *pos + 1 < bytes.len() && bytes[*pos + 1] == b'/' {
                // Closing tag — skip past '>'
                while *pos < bytes.len() && bytes[*pos] != b'>' {
                    *pos += 1;
                }
                if *pos < bytes.len() {
                    *pos += 1; // skip '>'
                }
                break;
            } else {
                // Child element
                if let Some(child) = parse_element(input, pos) {
                    children.push(child);
                }
            }
        } else {
            text.push(bytes[*pos] as char);
            *pos += 1;
        }
    }

    Some(HtmlElement {
        tag,
        classes,
        text: text.trim().to_string(),
        children,
    })
}

fn skip_whitespace(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

fn read_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len() && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'_' || bytes[*pos] == b'-') {
        *pos += 1;
    }
    String::from_utf8_lossy(&bytes[start..*pos]).to_string()
}

fn read_quoted_value(bytes: &[u8], pos: &mut usize) -> String {
    if *pos >= bytes.len() {
        return String::new();
    }
    let quote_char = bytes[*pos];
    if quote_char != b'"' && quote_char != b'\'' {
        return read_identifier(bytes, pos);
    }
    *pos += 1; // skip opening quote
    let start = *pos;
    while *pos < bytes.len() && bytes[*pos] != quote_char {
        *pos += 1;
    }
    let val = String::from_utf8_lossy(&bytes[start..*pos]).to_string();
    if *pos < bytes.len() {
        *pos += 1; // skip closing quote
    }
    val
}

// ---------------------------------------------------------------------------
// SCSS parser (minimal, hand-rolled)
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
        // Expand shorthand: #abc -> #aabbcc
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
// ---------------------------------------------------------------------------

fn generate_egui_code(
    elements: &[HtmlElement],
    styles: &HashMap<String, StyleRule>,
) -> proc_macro2::TokenStream {
    let stmts: Vec<proc_macro2::TokenStream> =
        elements.iter().map(|el| generate_element(el, styles)).collect();

    quote! {
        #( #stmts )*
    }
}

fn generate_element(
    el: &HtmlElement,
    styles: &HashMap<String, StyleRule>,
) -> proc_macro2::TokenStream {
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
            if let Some(fs) = rule.properties.get("font-size") {
                if let Ok(v) = fs.trim_end_matches("px").parse::<f32>() {
                    font_size = Some(v);
                }
            }
            if let Some(bg) = rule.properties.get("background") {
                background = Some(parse_hex_color(bg));
            }
            if let Some(p) = rule.properties.get("padding") {
                if let Ok(v) = p.trim_end_matches("px").parse::<f32>() {
                    padding = Some(v);
                }
            }
        }
    }

    // Generate children code
    let children_code: Vec<proc_macro2::TokenStream> = el
        .children
        .iter()
        .map(|child| generate_element(child, styles))
        .collect();

    // Build text widget with styles
    let text = &el.text;
    let has_text = !text.is_empty();
    let has_children = !children_code.is_empty();

    // Build RichText with applied styles
    let text_expr = if has_text {
        let mut rt = quote! { egui::RichText::new(#text) };
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

    // Wrap in frame if background or padding is set
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
            // Default: div, span, label, p, etc. -> ui.label or vertical group
            let mut stmts = Vec::new();
            if let Some(rt) = text_expr {
                stmts.push(quote! { ui.label(#rt); });
            }
            if has_children {
                for child in &children_code {
                    stmts.push(child.clone());
                }
            }
            quote! { #( #stmts )* }
        }
    };

    if needs_frame {
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
    }
}
