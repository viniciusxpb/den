use crate::types::{PipeCall, TextSegment};

/// Quebra texto raw em segmentos de literal e `{{ expr | pipe | pipe(arg) }}`.
///
/// Expressões podem ter uma cadeia de pipes (unidirecional, da esquerda pra direita):
/// - `{{ self.name | upper }}` → expr `self.name` + pipes `[upper]`.
/// - `{{ self.price | currency(br) | truncate(10) }}` → expr `self.price` + pipes `[currency(br), truncate(10)]`.
///
/// Pipes são parseados aqui no nível léxico; a validação de tipo sai no codegen.
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
            let full = after_open[..end].trim();
            if !full.is_empty() {
                let (expr, pipes) = split_pipes(full);
                segments.push(TextSegment::Expr { expr, pipes });
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

/// Divide `expr | pipe | pipe(arg, arg)` em `(expr, [PipeCall])`.
///
/// Respeita `|` dentro de parênteses e strings literais (não quebra).
fn split_pipes(input: &str) -> (String, Vec<PipeCall>) {
    let chars: Vec<char> = input.chars().collect();
    let mut parts: Vec<String> = Vec::new();
    let mut current = String::new();
    let mut depth: i32 = 0;
    let mut in_str: Option<char> = None;

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        match in_str {
            Some(q) => {
                current.push(c);
                if c == q {
                    in_str = None;
                }
            }
            None => {
                if c == '"' || c == '\'' {
                    in_str = Some(c);
                    current.push(c);
                } else if c == '(' {
                    depth += 1;
                    current.push(c);
                } else if c == ')' {
                    depth -= 1;
                    current.push(c);
                } else if c == '|' && depth == 0 {
                    parts.push(current.trim().to_string());
                    current.clear();
                } else {
                    current.push(c);
                }
            }
        }
        i += 1;
    }
    if !current.trim().is_empty() {
        parts.push(current.trim().to_string());
    }

    if parts.is_empty() {
        return (String::new(), Vec::new());
    }

    let expr = parts.remove(0);
    let pipes = parts.into_iter().map(parse_pipe_call).collect();
    (expr, pipes)
}

/// Parseia `"truncate(80)"` → `PipeCall { name: "truncate", args: ["80"] }`.
/// `"upper"` → `PipeCall { name: "upper", args: [] }`.
fn parse_pipe_call(raw: String) -> PipeCall {
    let trimmed = raw.trim();
    if let Some(paren_pos) = trimmed.find('(') {
        let name = trimmed[..paren_pos].trim().to_string();
        let args_str = trimmed[paren_pos + 1..]
            .trim_end_matches(')')
            .trim()
            .to_string();
        let args = if args_str.is_empty() {
            Vec::new()
        } else {
            split_args(&args_str)
        };
        PipeCall { name, args }
    } else {
        PipeCall {
            name: trimmed.to_string(),
            args: Vec::new(),
        }
    }
}

/// Divide argumentos separados por vírgula, respeitando strings e parênteses aninhados.
fn split_args(input: &str) -> Vec<String> {
    let chars: Vec<char> = input.chars().collect();
    let mut args = Vec::new();
    let mut current = String::new();
    let mut depth: i32 = 0;
    let mut in_str: Option<char> = None;
    for c in chars {
        match in_str {
            Some(q) => {
                current.push(c);
                if c == q {
                    in_str = None;
                }
            }
            None => {
                if c == '"' || c == '\'' {
                    in_str = Some(c);
                    current.push(c);
                } else if c == '(' {
                    depth += 1;
                    current.push(c);
                } else if c == ')' {
                    depth -= 1;
                    current.push(c);
                } else if c == ',' && depth == 0 {
                    args.push(current.trim().to_string());
                    current.clear();
                } else {
                    current.push(c);
                }
            }
        }
    }
    if !current.trim().is_empty() {
        args.push(current.trim().to_string());
    }
    args
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

    fn expr(seg: &TextSegment) -> (&str, &[PipeCall]) {
        match seg {
            TextSegment::Expr { expr, pipes } => (expr.as_str(), pipes.as_slice()),
            _ => panic!("expected Expr segment"),
        }
    }

    fn lit(seg: &TextSegment) -> &str {
        match seg {
            TextSegment::Literal(s) => s.as_str(),
            _ => panic!("expected Literal segment"),
        }
    }

    // --- parse_text_segments ---

    #[test]
    fn pure_literal() {
        let segs = parse_text_segments("hello world");
        assert_eq!(segs.len(), 1);
        assert_eq!(lit(&segs[0]), "hello world");
    }

    #[test]
    fn pure_expression() {
        let segs = parse_text_segments("{{ self.name }}");
        assert_eq!(segs.len(), 1);
        let (e, p) = expr(&segs[0]);
        assert_eq!(e, "self.name");
        assert!(p.is_empty());
    }

    #[test]
    fn literal_then_expr() {
        let segs = parse_text_segments("Hello, {{ self.name }}!");
        assert_eq!(segs.len(), 3);
        assert_eq!(lit(&segs[0]), "Hello, ");
        let (e, _) = expr(&segs[1]);
        assert_eq!(e, "self.name");
        assert_eq!(lit(&segs[2]), "!");
    }

    #[test]
    fn multiple_expressions() {
        let segs = parse_text_segments("{{ self.a }} - {{ self.b }}");
        assert_eq!(segs.len(), 3);
        let (a, _) = expr(&segs[0]);
        assert_eq!(a, "self.a");
        assert_eq!(lit(&segs[1]), " - ");
        let (b, _) = expr(&segs[2]);
        assert_eq!(b, "self.b");
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
        let (e, _) = expr(&segs[0]);
        assert_eq!(e, "user.name");
    }

    #[test]
    fn pipe_single_no_args() {
        let segs = parse_text_segments("{{ self.name | upper }}");
        let (e, p) = expr(&segs[0]);
        assert_eq!(e, "self.name");
        assert_eq!(p.len(), 1);
        assert_eq!(p[0].name, "upper");
        assert!(p[0].args.is_empty());
    }

    #[test]
    fn pipe_with_args() {
        let segs = parse_text_segments("{{ self.bio | truncate(80) }}");
        let (e, p) = expr(&segs[0]);
        assert_eq!(e, "self.bio");
        assert_eq!(p.len(), 1);
        assert_eq!(p[0].name, "truncate");
        assert_eq!(p[0].args, vec!["80"]);
    }

    #[test]
    fn pipe_chain() {
        let segs = parse_text_segments("{{ self.bio | truncate(80) | upper }}");
        let (e, p) = expr(&segs[0]);
        assert_eq!(e, "self.bio");
        assert_eq!(p.len(), 2);
        assert_eq!(p[0].name, "truncate");
        assert_eq!(p[0].args, vec!["80"]);
        assert_eq!(p[1].name, "upper");
    }

    #[test]
    fn pipe_string_arg() {
        let segs = parse_text_segments(r#"{{ self.name | default("Anônimo") }}"#);
        let (_, p) = expr(&segs[0]);
        assert_eq!(p[0].name, "default");
        assert_eq!(p[0].args, vec!["\"Anônimo\""]);
    }

    #[test]
    fn pipe_multiple_args() {
        let segs = parse_text_segments(r#"{{ self.tags | join(", ") }}"#);
        let (_, p) = expr(&segs[0]);
        assert_eq!(p[0].name, "join");
        assert_eq!(p[0].args, vec!["\", \""]);
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
    fn click_no_parens() {
        let (name, args) = parse_click_call("toggle");
        assert_eq!(name, "toggle");
        assert!(args.is_empty());
    }

    #[test]
    fn click_idx_arg() {
        // parse_click_call ainda aceita args (codegen rejeita; teste garante parser OK).
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
}
