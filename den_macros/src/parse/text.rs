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
