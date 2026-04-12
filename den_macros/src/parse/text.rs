use crate::types::TextSegment;

/// Mapeia `this` → `self` como keyword, não dentro de identificadores.
/// `this.name` → `self.name`, mas `this_value` fica `this_value`.
pub fn map_this_to_self(expr: &str) -> String {
    let expr = expr.trim();
    if expr == "this" {
        return "self".to_string();
    }

    let mut result = String::with_capacity(expr.len());
    let chars: Vec<char> = expr.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if i + 4 <= chars.len() && chars[i..i + 4] == ['t', 'h', 'i', 's'] {
            let before_ok =
                i == 0 || !(chars[i - 1].is_alphanumeric() || chars[i - 1] == '_');
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

/// Quebra texto raw em segmentos de literal e `{{ expr }}`.
pub fn parse_text_segments(raw: &str) -> Vec<TextSegment> {
    let mut segments = Vec::new();
    let mut rest = raw;

    while let Some(start) = rest.find("{{") {
        let before = &rest[..start];
        if !before.is_empty() {
            segments.push(TextSegment::Literal(before.to_string()));
        }
        let after_open = &rest[start + 2..];
        if let Some(end) = after_open.find("}}") {
            let expr = map_this_to_self(after_open[..end].trim());
            if !expr.is_empty() {
                segments.push(TextSegment::Expr(expr));
            }
            rest = &after_open[end + 2..];
        } else {
            segments.push(TextSegment::Literal(rest.to_string()));
            return segments;
        }
    }

    if !rest.is_empty() {
        segments.push(TextSegment::Literal(rest.to_string()));
    }
    segments
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- map_this_to_self ---

    #[test]
    fn maps_this_dot_field() {
        assert_eq!(map_this_to_self("this.name"), "self.name");
    }

    #[test]
    fn maps_bare_this() {
        assert_eq!(map_this_to_self("this"), "self");
    }

    #[test]
    fn does_not_map_this_prefix_in_ident() {
        assert_eq!(map_this_to_self("this_value"), "this_value");
    }

    #[test]
    fn does_not_map_this_suffix_in_ident() {
        assert_eq!(map_this_to_self("not_this"), "not_this");
    }

    #[test]
    fn maps_this_in_expression() {
        assert_eq!(map_this_to_self("this.items.len()"), "self.items.len()");
    }

    // --- parse_text_segments ---

    #[test]
    fn pure_literal() {
        let segs = parse_text_segments("hello world");
        assert!(matches!(&segs[..], [TextSegment::Literal(s)] if s == "hello world"));
    }

    #[test]
    fn pure_expression() {
        let segs = parse_text_segments("{{ this.name }}");
        assert!(matches!(&segs[..], [TextSegment::Expr(e)] if e == "self.name"));
    }

    #[test]
    fn literal_then_expr() {
        let segs = parse_text_segments("Hello, {{ this.name }}!");
        assert_eq!(segs.len(), 3);
        assert!(matches!(&segs[0], TextSegment::Literal(s) if s == "Hello, "));
        assert!(matches!(&segs[1], TextSegment::Expr(e) if e == "self.name"));
        assert!(matches!(&segs[2], TextSegment::Literal(s) if s == "!"));
    }

    #[test]
    fn multiple_expressions() {
        let segs = parse_text_segments("{{ this.a }} - {{ this.b }}");
        assert_eq!(segs.len(), 3);
        assert!(matches!(&segs[0], TextSegment::Expr(e) if e == "self.a"));
        assert!(matches!(&segs[1], TextSegment::Literal(s) if s == " - "));
        assert!(matches!(&segs[2], TextSegment::Expr(e) if e == "self.b"));
    }

    #[test]
    fn unclosed_braces_treated_as_literal() {
        let segs = parse_text_segments("{{ oops");
        assert_eq!(segs.len(), 1);
        assert!(matches!(&segs[0], TextSegment::Literal(_)));
    }
}
