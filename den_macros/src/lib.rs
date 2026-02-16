use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
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

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");
    let base = std::path::Path::new(&manifest_dir)
        .join("src")
        .join(&template_path);

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

    match generate_egui_code(&elements, &styles, has_self, &template_path) {
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
enum TemplateNode {
    Element(HtmlElement),
    ForLoop(ForLoop),
    IfChain(IfChain),
}

#[derive(Debug)]
struct ForLoop {
    each_var: String,
    iterable_expr: String,
    children: Vec<TemplateNode>,
}

#[derive(Debug)]
struct IfChain {
    condition: String,
    then_children: Vec<TemplateNode>,
    else_children: Vec<TemplateNode>,
}

#[derive(Debug)]
struct HtmlElement {
    tag: String,
    classes: Vec<String>,
    segments: Vec<TextSegment>,
    children: Vec<TemplateNode>,
    on_click: Option<String>,
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

fn parse_html(input: &str) -> Vec<TemplateNode> {
    let input = input.trim();
    let chars: Vec<char> = input.chars().collect();
    let mut pos = 0;
    let mut nodes = Vec::new();

    while pos < chars.len() {
        skip_ws(&chars, &mut pos);
        if pos >= chars.len() {
            break;
        }
        if chars[pos] == '<' {
            if let Some(node) = parse_node(&chars, &mut pos) {
                nodes.push(node);
            }
        } else {
            pos += 1;
        }
    }
    nodes
}

/// Peek at the tag name starting after '<' without advancing `pos`.
fn peek_tag_name(chars: &[char], pos: usize) -> String {
    let mut p = pos;
    // skip '<'
    if p < chars.len() && chars[p] == '<' {
        p += 1;
    }
    while p < chars.len() && chars[p].is_ascii_whitespace() {
        p += 1;
    }
    let start = p;
    while p < chars.len()
        && (chars[p].is_ascii_alphanumeric() || chars[p] == '_' || chars[p] == '-')
    {
        p += 1;
    }
    chars[start..p].iter().collect()
}

/// Dispatch to the appropriate parser based on tag name.
fn parse_node(chars: &[char], pos: &mut usize) -> Option<TemplateNode> {
    let tag = peek_tag_name(chars, *pos);
    match tag.as_str() {
        "for" => parse_for_node(chars, pos),
        "if" => parse_if_node(chars, pos),
        "else" => {
            // Orphan <else> — should have been consumed by parse_if_node
            eprintln!("Den: orphan <else> without preceding <if>, ignoring");
            // Skip past the element
            skip_past_closing_tag(chars, pos, "else");
            None
        }
        _ => parse_element_chars(chars, pos).map(TemplateNode::Element),
    }
}

/// Parse `<for each="var" in="expr">...children...</for>`
fn parse_for_node(chars: &[char], pos: &mut usize) -> Option<TemplateNode> {
    if chars[*pos] != '<' {
        return None;
    }
    *pos += 1; // skip '<'
    skip_ws(chars, pos);
    let _tag = read_ident(chars, pos); // consume "for"

    // Parse attributes: each="var" in="expr"
    let mut each_var = None;
    let mut in_expr = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        let attr_name = read_ident(chars, pos);
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '=' {
            *pos += 1; // skip '='
            skip_ws(chars, pos);
            let value = read_quoted(chars, pos);
            match attr_name.as_str() {
                "each" => each_var = Some(value),
                "in" => in_expr = Some(map_this_to_self(&value)),
                _ => eprintln!("Den: unknown attribute '{attr_name}' on <for>, ignoring"),
            }
        }
        skip_ws(chars, pos);
    }

    // Skip '>'
    if *pos < chars.len() && chars[*pos] == '>' {
        *pos += 1;
    }

    let each_var = match each_var {
        Some(v) => v,
        None => {
            eprintln!("Den: <for> missing 'each' attribute");
            return None;
        }
    };
    let iterable_expr = match in_expr {
        Some(v) => v,
        None => {
            eprintln!("Den: <for> missing 'in' attribute");
            return None;
        }
    };

    // Parse children until </for>
    let children = parse_children_nodes(chars, pos);

    Some(TemplateNode::ForLoop(ForLoop {
        each_var,
        iterable_expr,
        children,
    }))
}

/// Parse `<if cond="expr">...children...</if>` optionally followed by `<else>...children...</else>`
fn parse_if_node(chars: &[char], pos: &mut usize) -> Option<TemplateNode> {
    if chars[*pos] != '<' {
        return None;
    }
    *pos += 1; // skip '<'
    skip_ws(chars, pos);
    let _tag = read_ident(chars, pos); // consume "if"

    // Parse attributes: cond="expr"
    let mut condition = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        let attr_name = read_ident(chars, pos);
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '=' {
            *pos += 1; // skip '='
            skip_ws(chars, pos);
            let value = read_quoted(chars, pos);
            match attr_name.as_str() {
                "cond" => condition = Some(map_this_to_self(&value)),
                _ => eprintln!("Den: unknown attribute '{attr_name}' on <if>, ignoring"),
            }
        }
        skip_ws(chars, pos);
    }

    // Skip '>'
    if *pos < chars.len() && chars[*pos] == '>' {
        *pos += 1;
    }

    let condition = match condition {
        Some(v) => v,
        None => {
            eprintln!("Den: <if> missing 'cond' attribute");
            return None;
        }
    };

    // Parse then-children until </if>
    let then_children = parse_children_nodes(chars, pos);

    // Peek ahead for <else>
    let else_children = {
        let saved_pos = *pos;
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '<' {
            let tag = peek_tag_name(chars, *pos);
            if tag == "else" {
                // Consume <else>...</else>
                *pos += 1; // skip '<'
                skip_ws(chars, pos);
                let _tag = read_ident(chars, pos); // consume "else"
                skip_ws(chars, pos);
                // Skip '>'
                if *pos < chars.len() && chars[*pos] == '>' {
                    *pos += 1;
                }
                parse_children_nodes(chars, pos)
            } else {
                *pos = saved_pos;
                Vec::new()
            }
        } else {
            *pos = saved_pos;
            Vec::new()
        }
    };

    Some(TemplateNode::IfChain(IfChain {
        condition,
        then_children,
        else_children,
    }))
}

/// Parse child nodes until a closing tag is encountered (e.g. `</for>`, `</if>`, `</div>`).
fn parse_children_nodes(chars: &[char], pos: &mut usize) -> Vec<TemplateNode> {
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
            } else if let Some(node) = parse_node(chars, pos) {
                children.push(node);
            }
        } else {
            *pos += 1;
        }
    }
    children
}

/// Skip past a closing tag like `</tagname>`.
fn skip_past_closing_tag(chars: &[char], pos: &mut usize, _tag: &str) {
    // Skip the opening '<'
    if *pos < chars.len() && chars[*pos] == '<' {
        *pos += 1;
    }
    // Skip tag name and attributes
    while *pos < chars.len() && chars[*pos] != '>' {
        *pos += 1;
    }
    if *pos < chars.len() {
        *pos += 1; // skip '>'
    }
    // Now skip children until closing tag
    while *pos < chars.len() {
        if chars[*pos] == '<' && *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
            while *pos < chars.len() && chars[*pos] != '>' {
                *pos += 1;
            }
            if *pos < chars.len() {
                *pos += 1; // skip '>'
            }
            return;
        }
        *pos += 1;
    }
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
    let mut on_click = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        if chars[*pos] == '(' {
            // Event binding: (click)="funcao()"
            *pos += 1; // skip '('
            let event_name = read_ident(chars, pos);
            if *pos < chars.len() && chars[*pos] == ')' {
                *pos += 1; // skip ')'
            }
            skip_ws(chars, pos);
            if *pos < chars.len() && chars[*pos] == '=' {
                *pos += 1; // skip '='
                skip_ws(chars, pos);
                let raw_value = read_quoted(chars, pos);
                let func_name = raw_value.trim_end_matches("()");
                let func_name = map_this_to_self(func_name);
                if event_name == "click" {
                    on_click = Some(func_name);
                } else {
                    eprintln!("Den: unsupported event '({event_name})', ignoring");
                }
            }
        } else {
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
            on_click,
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
            } else if let Some(node) = parse_node(chars, pos) {
                children.push(node);
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
        on_click,
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

type RgbColor = (u8, u8, u8);

#[derive(Debug, Clone, Copy, Default, PartialEq)]
enum DisplayMode {
    #[default]
    Block,
    Flex,
}

#[derive(Debug, Clone, Copy)]
struct BorderStyle {
    width: f32,
    color: RgbColor,
}

impl Default for BorderStyle {
    fn default() -> Self {
        Self {
            width: 1.0,
            color: (0, 0, 0),
        }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq)]
enum WidthValue {
    #[default]
    Auto,
    Percent(f32),
    Px(f32),
}

#[derive(Debug, Clone, Default)]
struct StyleRule {
    color: Option<RgbColor>,
    font_size: Option<f32>,
    background: Option<RgbColor>,
    padding: Option<f32>,
    display: DisplayMode,
    border: Option<BorderStyle>,
    border_radius: Option<f32>,
    width: WidthValue,
    cursor_pointer: bool,
    hover: Option<Box<StyleRule>>,
}

impl StyleRule {
    /// Merge another rule into this one (last-wins for set properties).
    fn merge_from(&mut self, other: &Self) {
        if other.color.is_some() {
            self.color = other.color;
        }
        if other.font_size.is_some() {
            self.font_size = other.font_size;
        }
        if other.background.is_some() {
            self.background = other.background;
        }
        if other.padding.is_some() {
            self.padding = other.padding;
        }
        if other.display != DisplayMode::Block {
            self.display = other.display;
        }
        if other.border.is_some() {
            self.border = other.border;
        }
        if other.border_radius.is_some() {
            self.border_radius = other.border_radius;
        }
        if other.width != WidthValue::Auto {
            self.width = other.width;
        }
        if other.cursor_pointer {
            self.cursor_pointer = true;
        }
        if other.hover.is_some() {
            self.hover = other.hover.clone();
        }
    }

    /// Whether this resolved style requires an egui Frame wrapper.
    fn needs_frame(&self) -> bool {
        self.background.is_some()
            || self.padding.is_some()
            || self.border.is_some()
            || self.border_radius.is_some()
    }

    /// Extract only inheritable CSS properties (color, font-size) for propagation to children.
    /// Hover is NOT inheritable in CSS.
    fn inheritable(&self) -> Self {
        Self {
            color: self.color,
            font_size: self.font_size,
            ..Default::default()
        }
    }

    /// Whether this style has any hover overrides.
    fn needs_hover(&self) -> bool {
        self.hover.is_some()
    }

    /// Merge base style with hover overrides to produce the hovered appearance.
    fn resolve_hover(&self) -> Self {
        let mut hovered = self.clone();
        if let Some(h) = &self.hover {
            hovered.merge_from(h);
        }
        hovered.hover = None;
        hovered
    }
}

/// Parse a raw CSS value as a size in pixels (strips optional "px" suffix).
fn parse_size_value(value: &str) -> Option<f32> {
    value.trim_end_matches("px").parse::<f32>().ok()
}

/// Parse `width` values: `100%`, `50%`, `200px`, `200`.
/// Returns `Auto` and prints a compile-time warning for unrecognized values.
fn parse_width_value(value: &str) -> WidthValue {
    if value == "auto" {
        return WidthValue::Auto;
    }
    if let Some(pct) = value.strip_suffix('%')
        && let Ok(v) = pct.trim().parse::<f32>()
    {
        return WidthValue::Percent(v / 100.0);
    }
    if let Some(v) = parse_size_value(value) {
        return WidthValue::Px(v);
    }
    eprintln!("Den: unsupported width value '{value}', falling back to auto");
    WidthValue::Auto
}

/// Parse `border: 1px solid #e94560` into BorderStyle.
/// Only `solid` style is supported; other styles emit a compile-time warning.
fn parse_border_value(value: &str) -> Option<BorderStyle> {
    let parts: Vec<&str> = value.split_whitespace().collect();
    if parts.len() < 3 {
        return None;
    }
    let width = parse_size_value(parts[0])?;
    let style = parts[1];
    if style != "solid" {
        eprintln!("Den: border style '{style}' is not supported, rendering as solid");
    }
    let color = parse_hex_color(parts[2])?;
    Some(BorderStyle { width, color })
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

        // Check for pseudo-selector (e.g. :hover)
        let pseudo = if pos < bytes.len() && bytes[pos] == b':' {
            pos += 1; // skip ':'
            let p = read_identifier(bytes, &mut pos);
            if p.is_empty() { None } else { Some(p) }
        } else {
            None
        };

        skip_whitespace(bytes, &mut pos);

        // Expect '{'
        if pos >= bytes.len() || bytes[pos] != b'{' {
            continue;
        }
        pos += 1; // skip '{'

        // Read properties until '}'
        let mut rule = StyleRule::default();

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
            let value = String::from_utf8_lossy(&bytes[start..pos])
                .trim()
                .to_string();

            if pos < bytes.len() && bytes[pos] == b';' {
                pos += 1; // skip ';'
            }

            match prop_name.as_str() {
                "color" => rule.color = parse_hex_color(&value),
                "font-size" => rule.font_size = parse_size_value(&value),
                "background" => rule.background = parse_hex_color(&value),
                "padding" => rule.padding = parse_size_value(&value),
                "display" if value == "flex" => rule.display = DisplayMode::Flex,
                "border" => rule.border = parse_border_value(&value),
                "border-radius" => rule.border_radius = parse_size_value(&value),
                "width" => rule.width = parse_width_value(&value),
                "cursor" if value == "pointer" => rule.cursor_pointer = true,
                _ => {}
            }
        }

        match pseudo.as_deref() {
            Some("hover") => {
                let entry = styles.entry(class_name).or_insert_with(StyleRule::default);
                entry.hover = Some(Box::new(rule));
            }
            Some(p) => {
                eprintln!("Den: unsupported pseudo-selector ':{p}', ignoring");
            }
            None => {
                styles
                    .entry(class_name)
                    .or_insert_with(StyleRule::default)
                    .merge_from(&rule);
            }
        }
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

fn parse_hex_color(hex: &str) -> Option<RgbColor> {
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

    if hex.len() < 6 {
        eprintln!("Den: invalid hex color '#{hex}', expected #RGB or #RRGGBB");
        return None;
    }

    let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
    Some((r, g, b))
}

// ---------------------------------------------------------------------------
// Code generator: HTML + SCSS -> egui TokenStream
// All functions return Result to propagate errors as compile_error! (Fix #3)
// ---------------------------------------------------------------------------

fn generate_egui_code(
    nodes: &[TemplateNode],
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
    template_path: &str,
) -> Result<proc_macro2::TokenStream, String> {
    let inherited = StyleRule::default();
    let mut stmts = Vec::new();
    for (i, node) in nodes.iter().enumerate() {
        let mut path = vec![i];
        stmts.push(generate_node(
            node,
            styles,
            has_self,
            &inherited,
            template_path,
            &mut path,
            0,
        )?);
    }

    Ok(quote! {
        #( #stmts )*
    })
}

/// Dispatch code generation based on node type.
fn generate_node(
    node: &TemplateNode,
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
    inherited: &StyleRule,
    template_path: &str,
    tree_path: &mut Vec<usize>,
    loop_depth: usize,
) -> Result<proc_macro2::TokenStream, String> {
    match node {
        TemplateNode::Element(el) => generate_element(
            el,
            styles,
            has_self,
            inherited,
            template_path,
            tree_path,
            loop_depth,
        ),
        TemplateNode::ForLoop(fl) => generate_for_loop(
            fl,
            styles,
            has_self,
            inherited,
            template_path,
            tree_path,
            loop_depth,
        ),
        TemplateNode::IfChain(ic) => generate_if_chain(
            ic,
            styles,
            has_self,
            inherited,
            template_path,
            tree_path,
            loop_depth,
        ),
    }
}

/// Generate code for `<for each="var" in="expr">...children...</for>`.
fn generate_for_loop(
    fl: &ForLoop,
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
    inherited: &StyleRule,
    template_path: &str,
    tree_path: &mut Vec<usize>,
    loop_depth: usize,
) -> Result<proc_macro2::TokenStream, String> {
    let var_ident: proc_macro2::TokenStream = fl
        .each_var
        .parse()
        .map_err(|e| format!("Invalid loop variable '{}': {e}", fl.each_var))?;
    let iter_expr: proc_macro2::TokenStream = fl
        .iterable_expr
        .parse()
        .map_err(|e| format!("Invalid iterable expression '{}': {e}", fl.iterable_expr))?;

    let idx_ident: proc_macro2::TokenStream = format!("__den_idx_{loop_depth}").parse().unwrap();

    let mut children_code = Vec::new();
    for (i, child) in fl.children.iter().enumerate() {
        tree_path.push(i);
        children_code.push(generate_node(
            child,
            styles,
            has_self,
            inherited,
            template_path,
            tree_path,
            loop_depth + 1,
        )?);
        tree_path.pop();
    }

    Ok(quote! {
        for (#idx_ident, #var_ident) in (#iter_expr).iter().enumerate() {
            #( #children_code )*
        }
    })
}

/// Generate code for `<if cond="expr">...then...</if>` with optional `<else>...else...</else>`.
fn generate_if_chain(
    ic: &IfChain,
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
    inherited: &StyleRule,
    template_path: &str,
    tree_path: &mut Vec<usize>,
    loop_depth: usize,
) -> Result<proc_macro2::TokenStream, String> {
    let cond_expr: proc_macro2::TokenStream = ic
        .condition
        .parse()
        .map_err(|e| format!("Invalid condition '{}': {e}", ic.condition))?;

    let mut then_code = Vec::new();
    for (i, child) in ic.then_children.iter().enumerate() {
        tree_path.push(i);
        then_code.push(generate_node(
            child,
            styles,
            has_self,
            inherited,
            template_path,
            tree_path,
            loop_depth,
        )?);
        tree_path.pop();
    }

    if ic.else_children.is_empty() {
        Ok(quote! {
            if #cond_expr {
                #( #then_code )*
            }
        })
    } else {
        let mut else_code = Vec::new();
        for (i, child) in ic.else_children.iter().enumerate() {
            tree_path.push(i);
            else_code.push(generate_node(
                child,
                styles,
                has_self,
                inherited,
                template_path,
                tree_path,
                loop_depth,
            )?);
            tree_path.pop();
        }

        Ok(quote! {
            if #cond_expr {
                #( #then_code )*
            } else {
                #( #else_code )*
            }
        })
    }
}

/// Generate a deterministic ID for a hover element based on template path,
/// element position in the tree, tag name, and classes.
fn den_element_id(template_path: &str, tree_path: &[usize], tag: &str, classes: &[String]) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    template_path.hash(&mut hasher);
    tree_path.hash(&mut hasher);
    tag.hash(&mut hasher);
    classes.hash(&mut hasher);
    hasher.finish()
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

/// Build an `egui::Frame` expression from a resolved style.
fn build_frame_expr(style: &StyleRule) -> proc_macro2::TokenStream {
    let mut frame_expr = quote! { egui::Frame::default() };
    if let Some((r, g, b)) = style.background {
        frame_expr = quote! { #frame_expr.fill(egui::Color32::from_rgb(#r, #g, #b)) };
    }
    if let Some(pad) = style.padding {
        frame_expr = quote! { #frame_expr.inner_margin(#pad) };
    }
    if let Some(radius) = style.border_radius {
        frame_expr = quote! { #frame_expr.corner_radius(#radius) };
    }
    if let Some(border_style) = style.border {
        let border_width = border_style.width;
        let (border_r, border_g, border_b) = border_style.color;
        frame_expr = quote! {
            #frame_expr.stroke(egui::Stroke::new(
                #border_width,
                egui::Color32::from_rgb(#border_r, #border_g, #border_b),
            ))
        };
    }
    frame_expr
}

/// Build an `egui::RichText` expression from text tokens and a resolved style.
fn build_rich_text_expr(
    text_ts: &proc_macro2::TokenStream,
    style: &StyleRule,
) -> proc_macro2::TokenStream {
    let mut rt = quote! { egui::RichText::new(#text_ts) };
    if let Some((r, g, b)) = style.color {
        rt = quote! { #rt.color(egui::Color32::from_rgb(#r, #g, #b)) };
    }
    if let Some(size) = style.font_size {
        rt = quote! { #rt.size(#size) };
    }
    rt
}

fn generate_element(
    el: &HtmlElement,
    styles: &HashMap<String, StyleRule>,
    has_self: bool,
    inherited: &StyleRule,
    template_path: &str,
    tree_path: &mut Vec<usize>,
    loop_depth: usize,
) -> Result<proc_macro2::TokenStream, String> {
    // Start from inherited styles, then merge this element's own classes on top
    let mut resolved = inherited.inheritable();
    for class in &el.classes {
        if let Some(rule) = styles.get(class) {
            resolved.merge_from(rule);
        }
    }

    // Generate children code, propagating inheritable styles
    let child_inherited = resolved.inheritable();
    let mut children_code = Vec::new();
    for (i, child) in el.children.iter().enumerate() {
        tree_path.push(i);
        children_code.push(generate_node(
            child,
            styles,
            has_self,
            &child_inherited,
            template_path,
            tree_path,
            loop_depth,
        )?);
        tree_path.pop();
    }

    // Build text content from segments
    let text_ts = build_text_token_stream(&el.segments, has_self)?;
    let has_hover = resolved.needs_hover();

    // Build inner content (text + children) for a given style
    let build_inner = |style: &StyleRule,
                       text_ts: &Option<proc_macro2::TokenStream>,
                       children_code: &[proc_macro2::TokenStream],
                       tag: &str|
     -> proc_macro2::TokenStream {
        let text_expr = text_ts.as_ref().map(|ts| build_rich_text_expr(ts, style));

        let inner = match tag {
            "heading" | "h1" | "h2" | "h3" => {
                if let Some(rt) = text_expr {
                    quote! { ui.heading(#rt); }
                } else if !children_code.is_empty() {
                    quote! { #( #children_code )* }
                } else {
                    quote! {}
                }
            }
            _ => {
                let mut stmts = Vec::new();
                if let Some(rt) = text_expr {
                    stmts.push(quote! { ui.label(#rt); });
                }
                for child in children_code {
                    stmts.push(child.clone());
                }
                quote! { #( #stmts )* }
            }
        };

        // Wrap in horizontal layout if display: flex
        let inner = if style.display == DisplayMode::Flex {
            quote! { ui.horizontal(|ui| { #inner }); }
        } else {
            inner
        };

        // Apply width constraint
        match style.width {
            WidthValue::Percent(pct) => {
                quote! {
                    ui.set_width(ui.available_width() * #pct);
                    #inner
                }
            }
            WidthValue::Px(px) => {
                quote! {
                    ui.set_width(#px);
                    #inner
                }
            }
            WidthValue::Auto => inner,
        }
    };

    let tag = el.tag.as_str();
    let has_click = el.on_click.is_some();

    if has_click && !has_self {
        return Err(
            "Template uses (click) event but `self` was not passed to den_template!. \
             Use: den_template!(\"path\", self);"
                .to_string(),
        );
    }

    // Build the click handler token stream (if any)
    let click_call = el.on_click.as_ref().map(|func_name| {
        let func_tokens: proc_macro2::TokenStream = format!("self.{func_name}()")
            .parse()
            .expect("Den: invalid function name in (click)");
        func_tokens
    });

    let needs_interaction = has_hover || has_click;

    if needs_interaction {
        let element_id = den_element_id(template_path, tree_path, tag, &el.classes);

        // Build the rendered content (with or without hover style switching)
        let render_code = if has_hover {
            let hovered_style = resolved.resolve_hover();

            let base_inner = build_inner(&resolved, &text_ts, &children_code, tag);
            let hover_inner = build_inner(&hovered_style, &text_ts, &children_code, tag);

            let base_code = if resolved.needs_frame() {
                let frame = build_frame_expr(&resolved);
                quote! { #frame.show(ui, |ui| { #base_inner }); }
            } else {
                base_inner
            };

            let hover_code = if hovered_style.needs_frame() {
                let frame = build_frame_expr(&hovered_style);
                quote! { #frame.show(ui, |ui| { #hover_inner }); }
            } else {
                hover_inner
            };

            let cursor_code = if hovered_style.cursor_pointer {
                quote! {
                    if __den_is_hovered {
                        ui.ctx().set_cursor_icon(egui::CursorIcon::PointingHand);
                    }
                }
            } else {
                quote! {}
            };

            quote! {
                let __den_was_hovered = ui.data(|d| d.get_temp::<bool>(__den_id).unwrap_or(false));
                let __den_scope = ui.scope(|ui| {
                    if __den_was_hovered {
                        #hover_code
                    } else {
                        #base_code
                    }
                });
                let __den_is_hovered = ui.rect_contains_pointer(__den_scope.response.rect);
                ui.data_mut(|d| d.insert_temp(__den_id, __den_is_hovered));
                #cursor_code
            }
        } else {
            // Click only, no hover — wrap in scope to capture rect
            let inner_code = build_inner(&resolved, &text_ts, &children_code, tag);
            let wrapped = if resolved.needs_frame() {
                let frame_expr = build_frame_expr(&resolved);
                quote! { #frame_expr.show(ui, |ui| { #inner_code }); }
            } else {
                inner_code
            };
            quote! {
                let __den_scope = ui.scope(|ui| {
                    #wrapped
                });
            }
        };

        // Build click handling (if any)
        let click_code = if let Some(call) = click_call {
            quote! {
                let __den_resp = ui.interact(
                    __den_scope.response.rect,
                    __den_id.with("click"),
                    egui::Sense::click(),
                );
                if __den_resp.clicked() {
                    #call;
                }
            }
        } else {
            quote! {}
        };

        let id_expr = if loop_depth > 0 {
            // Inside a loop: XOR compile-time hash with runtime loop indices for uniqueness
            let mut salt = quote! { 0u64 };
            for d in 0..loop_depth {
                let idx: proc_macro2::TokenStream = format!("__den_idx_{d}").parse().unwrap();
                salt = quote! { (#salt).wrapping_mul(31).wrapping_add(#idx as u64) };
            }
            quote! { egui::Id::new(#element_id ^ #salt) }
        } else {
            quote! { egui::Id::new(#element_id) }
        };

        Ok(quote! {
            {
                let __den_id = #id_expr;
                #render_code
                #click_code
            }
        })
    } else {
        // No hover, no click — original simple path
        let inner_code = build_inner(&resolved, &text_ts, &children_code, tag);

        Ok(if resolved.needs_frame() {
            let frame_expr = build_frame_expr(&resolved);
            quote! {
                #frame_expr.show(ui, |ui| {
                    #inner_code
                });
            }
        } else {
            inner_code
        })
    }
}
