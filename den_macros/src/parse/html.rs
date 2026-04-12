use crate::types::{RawElement, RawForLoop, RawIfChain, RawNode};
use super::text::parse_text_segments;

// Toda a lógica de parsing HTML opera em `Vec<char>` para suporte correto a UTF-8.

pub fn parse_html(input: &str) -> Vec<RawNode> {
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

/// Peek no nome da tag após '<' sem avançar `pos`.
fn peek_tag_name(chars: &[char], pos: usize) -> String {
    let mut p = pos;
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

/// Dispatch para o parser correto baseado no nome da tag.
fn parse_node(chars: &[char], pos: &mut usize) -> Option<RawNode> {
    let tag = peek_tag_name(chars, *pos);
    match tag.as_str() {
        "for" => parse_for_node(chars, pos),
        "if" => parse_if_node(chars, pos),
        "else" => {
            eprintln!("Den: orphan <else> without preceding <if>, ignoring");
            skip_past_closing_tag(chars, pos, "else");
            None
        }
        _ => parse_element_chars(chars, pos).map(RawNode::Element),
    }
}

/// Parse `<for each="var" in="expr">...children...</for>`
fn parse_for_node(chars: &[char], pos: &mut usize) -> Option<RawNode> {
    if chars[*pos] != '<' {
        return None;
    }
    *pos += 1;
    skip_ws(chars, pos);
    let _tag = read_ident(chars, pos); // consume "for"

    let mut each_var = None;
    let mut in_expr = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        let attr_name = read_ident(chars, pos);
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '=' {
            *pos += 1;
            skip_ws(chars, pos);
            let value = read_quoted(chars, pos);
            match attr_name.as_str() {
                "each" => each_var = Some(value),
                "in" => in_expr = Some(value),
                _ => eprintln!("Den: unknown attribute '{attr_name}' on <for>, ignoring"),
            }
        }
        skip_ws(chars, pos);
    }

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

    let children = parse_children_nodes(chars, pos);

    Some(RawNode::ForLoop(RawForLoop {
        each_var,
        iterable_expr,
        children,
    }))
}

/// Parse `<if cond="expr">...then...</if>` com opcional `<else>...else...</else>`
fn parse_if_node(chars: &[char], pos: &mut usize) -> Option<RawNode> {
    if chars[*pos] != '<' {
        return None;
    }
    *pos += 1;
    skip_ws(chars, pos);
    let _tag = read_ident(chars, pos); // consume "if"

    let mut condition = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        let attr_name = read_ident(chars, pos);
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '=' {
            *pos += 1;
            skip_ws(chars, pos);
            let value = read_quoted(chars, pos);
            match attr_name.as_str() {
                "cond" => condition = Some(value),
                _ => eprintln!("Den: unknown attribute '{attr_name}' on <if>, ignoring"),
            }
        }
        skip_ws(chars, pos);
    }

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

    let then_children = parse_children_nodes(chars, pos);

    let else_children = {
        let saved_pos = *pos;
        skip_ws(chars, pos);
        if *pos < chars.len() && chars[*pos] == '<' {
            let tag = peek_tag_name(chars, *pos);
            if tag == "else" {
                *pos += 1; // skip '<'
                skip_ws(chars, pos);
                let _tag = read_ident(chars, pos); // consume "else"
                skip_ws(chars, pos);
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

    Some(RawNode::IfChain(RawIfChain {
        condition,
        then_children,
        else_children,
    }))
}

/// Parse filhos até encontrar uma closing tag (`</for>`, `</if>`, `</div>` etc).
fn parse_children_nodes(chars: &[char], pos: &mut usize) -> Vec<RawNode> {
    let mut children = Vec::new();
    while *pos < chars.len() {
        if chars[*pos] == '<' {
            if *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
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

/// Pula past uma closing tag como `</tagname>`.
fn skip_past_closing_tag(chars: &[char], pos: &mut usize, _tag: &str) {
    if *pos < chars.len() && chars[*pos] == '<' {
        *pos += 1;
    }
    while *pos < chars.len() && chars[*pos] != '>' {
        *pos += 1;
    }
    if *pos < chars.len() {
        *pos += 1;
    }
    while *pos < chars.len() {
        if chars[*pos] == '<' && *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
            while *pos < chars.len() && chars[*pos] != '>' {
                *pos += 1;
            }
            if *pos < chars.len() {
                *pos += 1;
            }
            return;
        }
        *pos += 1;
    }
}

fn parse_element_chars(chars: &[char], pos: &mut usize) -> Option<RawElement> {
    if chars[*pos] != '<' {
        return None;
    }
    *pos += 1;

    skip_ws(chars, pos);
    let tag = read_ident(chars, pos);
    if tag.is_empty() {
        return None;
    }

    let mut classes = Vec::new();
    let mut on_click = None;
    let mut den_bind = None;
    let mut bind_expr = None;
    let mut placeholder = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        if chars[*pos] == '(' {
            *pos += 1; // skip '('
            let event_name = read_ident(chars, pos);
            if *pos < chars.len() && chars[*pos] == ')' {
                *pos += 1; // skip ')'
            }
            skip_ws(chars, pos);
            if *pos < chars.len() && chars[*pos] == '=' {
                *pos += 1;
                skip_ws(chars, pos);
                let raw_value = read_quoted(chars, pos);
                if event_name == "click" {
                    // Armazena a expressão inteira (e.g. "on_edit(user.id)")
                    // Parsing em func_name + args é feito no resolve.
                    on_click = Some(raw_value);
                } else {
                    eprintln!("Den: unsupported event '({event_name})', ignoring");
                }
            }
        } else {
            let attr_name = read_ident(chars, pos);
            skip_ws(chars, pos);
            if *pos < chars.len() && chars[*pos] == '=' {
                *pos += 1;
                skip_ws(chars, pos);
                let value = read_quoted(chars, pos);
                if attr_name == "class" {
                    classes = value.split_whitespace().map(|s| s.to_string()).collect();
                } else if attr_name == "den-bind" {
                    den_bind = Some(value);
                } else if attr_name == "bind" {
                    bind_expr = Some(value);
                } else if attr_name == "placeholder" {
                    placeholder = Some(value);
                }
            }
        }
        skip_ws(chars, pos);
    }

    // Self-closing explícito (`/>`) ou void element (`input` é sempre self-closing)
    let is_void = tag == "input";
    if (*pos < chars.len() && chars[*pos] == '/') || is_void {
        // Consome `/>` ou apenas `>` (void element)
        if *pos < chars.len() && chars[*pos] == '/' {
            *pos += 1;
        }
        if *pos < chars.len() && chars[*pos] == '>' {
            *pos += 1;
        }
        return Some(RawElement {
            tag,
            classes,
            segments: Vec::new(),
            children: Vec::new(),
            on_click,
            den_bind,
            bind_expr,
            placeholder,
        });
    }
    if *pos < chars.len() && chars[*pos] == '>' {
        *pos += 1;
    }

    // Lê conteúdo (texto + filhos)
    let mut raw_text = String::new();
    let mut children = Vec::new();

    while *pos < chars.len() {
        if chars[*pos] == '<' {
            if *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
                while *pos < chars.len() && chars[*pos] != '>' {
                    *pos += 1;
                }
                if *pos < chars.len() {
                    *pos += 1;
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

    let segments = parse_text_segments(raw_text.trim());

    Some(RawElement {
        tag,
        classes,
        segments,
        children,
        on_click,
        den_bind,
        bind_expr,
        placeholder,
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
    *pos += 1;
    let start = *pos;
    while *pos < chars.len() && chars[*pos] != quote_char {
        *pos += 1;
    }
    let val: String = chars[start..*pos].iter().collect();
    if *pos < chars.len() {
        *pos += 1;
    }
    val
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::RawNode;

    #[test]
    fn input_bind_parsed() {
        let nodes = parse_html(r#"<input bind="self.name" placeholder="Nome" class="inp" />"#);
        assert_eq!(nodes.len(), 1);
        if let RawNode::Element(el) = &nodes[0] {
            assert_eq!(el.tag, "input");
            assert_eq!(el.bind_expr.as_deref(), Some("self.name"));
            assert_eq!(el.placeholder.as_deref(), Some("Nome"));
            assert_eq!(el.classes, vec!["inp"]);
            assert!(el.children.is_empty());
        } else {
            panic!("expected Element");
        }
    }

    #[test]
    fn input_without_bind_has_none() {
        let nodes = parse_html(r#"<input class="inp" />"#);
        assert_eq!(nodes.len(), 1);
        if let RawNode::Element(el) = &nodes[0] {
            assert_eq!(el.tag, "input");
            assert!(el.bind_expr.is_none());
            assert!(el.placeholder.is_none());
        } else {
            panic!("expected Element");
        }
    }

    #[test]
    fn input_is_void_element() {
        // `<input>` sem `/>` deve ser tratado como self-closing (sem filhos)
        let nodes = parse_html(r#"<input bind="self.x">"#);
        assert_eq!(nodes.len(), 1);
        if let RawNode::Element(el) = &nodes[0] {
            assert!(el.children.is_empty());
        } else {
            panic!("expected Element");
        }
    }

    #[test]
    fn input_inside_div() {
        let nodes = parse_html(r#"<div class="form"><input bind="self.email" /></div>"#);
        assert_eq!(nodes.len(), 1);
        if let RawNode::Element(div) = &nodes[0] {
            assert_eq!(div.tag, "div");
            assert_eq!(div.children.len(), 1);
            if let RawNode::Element(input) = &div.children[0] {
                assert_eq!(input.tag, "input");
                assert_eq!(input.bind_expr.as_deref(), Some("self.email"));
            } else {
                panic!("expected input Element");
            }
        } else {
            panic!("expected div Element");
        }
    }
}
