use crate::types::TextSegment;

/// Quebra texto raw em segmentos de literal e `{{ expr }}`.
/// Expressões passam direto (sem tradução) — templates usam `self.` diretamente.
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
            let expr = after_open[..end].trim().to_string();
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

/// Parseia `"handler(arg1, arg2)"` → `("handler", vec!["arg1", "arg2"])`.
/// `"handler()"` → `("handler", vec![])`.
/// `"handler"` (sem parens) → `("handler", vec![])`.
pub fn parse_click_call(raw: &str) -> (String, Vec<String>) {
    let raw = raw.trim().trim_end_matches(')');
    if let Some(paren_pos) = raw.find('(') {
        let func_name = raw[..paren_pos].trim().to_string();
        let args_str = raw[paren_pos + 1..].trim();
        let args = if args_str.is_empty() {
            vec![]
        } else {
            args_str.split(',').map(|a| a.trim().to_string()).collect()
        };
        (func_name, args)
    } else {
        (raw.to_string(), vec![])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- parse_text_segments ---

    #[test]
    fn pure_literal() {
        let segs = parse_text_segments("hello world");
        assert!(matches!(&segs[..], [TextSegment::Literal(s)] if s == "hello world"));
    }

    #[test]
    fn pure_expression() {
        let segs = parse_text_segments("{{ self.name }}");
        assert!(matches!(&segs[..], [TextSegment::Expr(e)] if e == "self.name"));
    }

    #[test]
    fn literal_then_expr() {
        let segs = parse_text_segments("Hello, {{ self.name }}!");
        assert_eq!(segs.len(), 3);
        assert!(matches!(&segs[0], TextSegment::Literal(s) if s == "Hello, "));
        assert!(matches!(&segs[1], TextSegment::Expr(e) if e == "self.name"));
        assert!(matches!(&segs[2], TextSegment::Literal(s) if s == "!"));
    }

    #[test]
    fn multiple_expressions() {
        let segs = parse_text_segments("{{ self.a }} - {{ self.b }}");
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

    #[test]
    fn loop_variable_passes_through() {
        let segs = parse_text_segments("{{ user.name }}");
        assert!(matches!(&segs[..], [TextSegment::Expr(e)] if e == "user.name"));
    }

    // --- parse_click_call ---

    #[test]
    fn click_no_args() {
        let (name, args) = parse_click_call("toggle()");
        assert_eq!(name, "toggle");
        assert!(args.is_empty());
    }

    #[test]
    fn click_one_arg() {
        let (name, args) = parse_click_call("on_edit(user.id)");
        assert_eq!(name, "on_edit");
        assert_eq!(args, vec!["user.id"]);
    }

    #[test]
    fn click_multiple_args() {
        let (name, args) = parse_click_call("on_update(user.id, user.name)");
        assert_eq!(name, "on_update");
        assert_eq!(args, vec!["user.id", "user.name"]);
    }

    #[test]
    fn click_idx_arg() {
        let (name, args) = parse_click_call("on_select(idx)");
        assert_eq!(name, "on_select");
        assert_eq!(args, vec!["idx"]);
    }

    #[test]
    fn click_style_arg() {
        let (name, args) = parse_click_call("on_style(user.id, style)");
        assert_eq!(name, "on_style");
        assert_eq!(args, vec!["user.id", "style"]);
    }

    #[test]
    fn click_no_parens() {
        let (name, args) = parse_click_call("toggle");
        assert_eq!(name, "toggle");
        assert!(args.is_empty());
    }
}
