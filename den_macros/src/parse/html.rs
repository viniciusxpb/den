use super::text::parse_text_segments;
use crate::types::{
    RawElement, RawForLoop, RawIfBranch, RawIfChain, RawNode, RawObject,
};

// Toda a lógica de parsing HTML opera em `Vec<char>` para suporte correto a UTF-8.
//
// Sintaxe Den (único prefixo reservado: `@`):
// - `@if(cond) { ... } !cond { ... } ! { ... }` — controle de fluxo condicional
// - `@for(item in expr) { ... } @empty { ... }` — iteração
// - `@object(scope) { ... }` — escopo de binding
// - `@click="..."`, `@bind="..."`, `@goto="..."`, `@with="..."` — atributos de eventos/data
// - `{{ expr | pipe }}` — interpolação com pipes opcionais
// - resto é HTML/CSS puro
//
// Erros de sintaxe são acumulados num `ParseErrors` e propagados via `Result` pra
// quem chamou (o macro transforma em `compile_error!`). Sem `panic!` — parsers
// que podem ser invocados fora da macro (future: LSP, linter, preview tooling)
// precisam falhar de forma recuperável.

/// Erros acumulados durante o parse. A maioria é fatal na primeira ocorrência,
/// mas acumulamos pra mostrar múltiplos num build só.
#[derive(Debug, Default)]
pub struct ParseErrors {
    pub messages: Vec<String>,
}

impl ParseErrors {
    fn push(&mut self, msg: impl Into<String>) {
        self.messages.push(msg.into());
    }

    pub fn into_result<T>(self, ok: T) -> Result<T, String> {
        if self.messages.is_empty() {
            Ok(ok)
        } else {
            Err(self.messages.join("\n"))
        }
    }
}

pub fn parse_html(input: &str) -> Result<Vec<RawNode>, String> {
    let input = input.trim();
    let chars: Vec<char> = input.chars().collect();
    let mut pos = 0;
    let mut errors = ParseErrors::default();
    let nodes = parse_nodes_until(&chars, &mut pos, None, &mut errors);
    errors.into_result(nodes)
}

/// Versão infalível usada em testes (panica se houver erro — equivalente ao comportamento antigo).
#[cfg(test)]
pub fn parse_html_ok(input: &str) -> Vec<RawNode> {
    parse_html(input).expect("parse_html failed in test")
}

/// Parseia filhos até `stop_char` (`}` no caso de blocos `@`) ou fim do input.
/// Também para em closing tag `</...>` deixando `pos` após o `>`.
fn parse_nodes_until(
    chars: &[char],
    pos: &mut usize,
    stop_char: Option<char>,
    errors: &mut ParseErrors,
) -> Vec<RawNode> {
    let mut nodes = Vec::new();
    loop {
        skip_ws(chars, pos);
        if *pos >= chars.len() {
            break;
        }
        if let Some(stop) = stop_char
            && chars[*pos] == stop
        {
            break;
        }
        if *pos + 1 < chars.len() && chars[*pos] == '<' && chars[*pos + 1] == '/' {
            break;
        }
        if chars[*pos] == '@' {
            if let Some(node) = parse_at_directive(chars, pos, errors) {
                nodes.push(node);
            }
            continue;
        }
        if chars[*pos] == '<' {
            if *pos + 3 < chars.len()
                && chars[*pos + 1] == '!'
                && chars[*pos + 2] == '-'
                && chars[*pos + 3] == '-'
            {
                skip_comment(chars, pos);
                continue;
            }
            let tag = peek_tag_name(chars, *pos);
            if tag == "for" || tag == "if" || tag == "else" {
                errors.push(format!(
                    "Den: `<{tag}>` não é mais suportado. Use `@{tag}(...)`/`!` com blocos {{ }}."
                ));
                // Consome a tag pra não travar; continua parsing pra reportar mais erros.
                *pos = skip_until_gt(chars, *pos);
                continue;
            }
            if let Some(el) = parse_element_chars(chars, pos, errors) {
                nodes.push(RawNode::Element(el));
            } else {
                *pos += 1;
            }
            continue;
        }
        *pos += 1;
    }
    nodes
}

/// Dispatch pra `@if`, `@for`, `@object`. Diretiva desconhecida acumula erro e avança.
fn parse_at_directive(
    chars: &[char],
    pos: &mut usize,
    errors: &mut ParseErrors,
) -> Option<RawNode> {
    if chars[*pos] != '@' {
        return None;
    }
    let saved = *pos;
    *pos += 1;
    let name = read_ident(chars, pos);
    match name.as_str() {
        "if" => parse_if_directive(chars, pos, errors),
        "for" => parse_for_directive(chars, pos, errors),
        "object" => parse_object_directive(chars, pos, errors),
        "empty" => {
            errors.push("Den: `@empty` só é válido logo após um `@for(...) { }`");
            None
        }
        other => {
            errors.push(format!(
                "Den: diretiva desconhecida `@{other}` em posição {saved}"
            ));
            None
        }
    }
}

fn skip_until_gt(chars: &[char], start: usize) -> usize {
    let mut p = start;
    while p < chars.len() && chars[p] != '>' {
        p += 1;
    }
    if p < chars.len() {
        p + 1
    } else {
        p
    }
}

/// `@if(cond) { ... } !cond { ... } !cond { ... } ! { ... }`
fn parse_if_directive(
    chars: &[char],
    pos: &mut usize,
    errors: &mut ParseErrors,
) -> Option<RawNode> {
    skip_ws(chars, pos);
    let cond = read_parenthesized(chars, pos)?;
    skip_ws(chars, pos);
    let then_children = read_block(chars, pos, errors)?;

    let mut branches = vec![RawIfBranch {
        condition: cond,
        children: then_children,
    }];
    let mut else_children: Vec<RawNode> = Vec::new();

    loop {
        let save = *pos;
        skip_ws(chars, pos);
        if *pos >= chars.len() || chars[*pos] != '!' {
            *pos = save;
            break;
        }
        *pos += 1;
        let cond_raw = read_until_unbalanced_brace(chars, pos);
        let cond_trim = cond_raw.trim().to_string();
        skip_ws(chars, pos);
        let block = read_block(chars, pos, errors)?;
        if cond_trim.is_empty() {
            else_children = block;
            break;
        } else {
            branches.push(RawIfBranch {
                condition: prefix_self_if_bare(&cond_trim),
                children: block,
            });
        }
    }

    Some(RawNode::IfChain(RawIfChain {
        branches,
        else_children,
    }))
}

/// Retorna conteúdo entre `(` e `)` balanceados, avança `pos` após `)`.
fn read_parenthesized(chars: &[char], pos: &mut usize) -> Option<String> {
    if *pos >= chars.len() || chars[*pos] != '(' {
        return None;
    }
    *pos += 1; // skip '('
    let start = *pos;
    let mut depth: i32 = 1;
    let mut in_str: Option<char> = None;
    while *pos < chars.len() {
        let c = chars[*pos];
        match in_str {
            Some(q) => {
                if c == q {
                    in_str = None;
                }
            }
            None => {
                if c == '"' || c == '\'' {
                    in_str = Some(c);
                } else if c == '(' {
                    depth += 1;
                } else if c == ')' {
                    depth -= 1;
                    if depth == 0 {
                        let s: String = chars[start..*pos].iter().collect();
                        *pos += 1; // skip ')'
                        return Some(s.trim().to_string());
                    }
                }
            }
        }
        *pos += 1;
    }
    None
}

/// Lê chars até encontrar `{` (que NÃO é consumido). Usado pra ler condição de `!COND`.
/// Respeita strings e `{{ }}` de interpolação.
fn read_until_unbalanced_brace(chars: &[char], pos: &mut usize) -> String {
    let start = *pos;
    let mut in_str: Option<char> = None;
    while *pos < chars.len() {
        let c = chars[*pos];
        match in_str {
            Some(q) => {
                if c == q {
                    in_str = None;
                }
                *pos += 1;
            }
            None => {
                if c == '"' || c == '\'' {
                    in_str = Some(c);
                    *pos += 1;
                } else if c == '{' {
                    break;
                } else {
                    *pos += 1;
                }
            }
        }
    }
    chars[start..*pos].iter().collect()
}

/// Se condição começa com identificador bare (sem `self.`, `(`, `"`, dígito), prefixa `self.`.
/// Cobre atalho do doc: `!status == "pending"` → `self.status == "pending"`.
///
/// Não prefixa quando o primeiro token é:
/// - keyword Rust (`self`, `Self`, `true`, `false`, `let`, `if`, `else`, ...)
/// - item de módulo/path (`crate`, `super`)
/// - variante de enum comum (`Some`, `None`, `Ok`, `Err`)
/// - qualquer identificador começando com letra maiúscula (presumivelmente tipo/enum)
fn prefix_self_if_bare(cond: &str) -> String {
    let c = cond.trim();
    if c.is_empty() {
        return c.to_string();
    }
    let first = match c.chars().next() {
        Some(ch) => ch,
        None => return c.to_string(),
    };
    if !(first.is_ascii_alphabetic() || first == '_') {
        return c.to_string();
    }
    // Uppercase → tipo/enum, não prefixa.
    if first.is_ascii_uppercase() {
        return c.to_string();
    }
    let first_word: String = c
        .chars()
        .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
        .collect();
    // Keywords Rust e itens de path que não devem receber `self.`.
    const KEYWORDS: &[&str] = &[
        "self", "Self", "super", "crate", "true", "false", "let", "if", "else", "match",
        "return", "mut", "ref", "const", "static", "async", "await", "move", "fn", "loop",
        "while", "for", "in", "break", "continue", "Some", "None", "Ok", "Err",
    ];
    if KEYWORDS.contains(&first_word.as_str()) {
        return c.to_string();
    }
    format!("self.{c}")
}

/// `@for(item in expr) { ... } @empty { ... }`
fn parse_for_directive(
    chars: &[char],
    pos: &mut usize,
    errors: &mut ParseErrors,
) -> Option<RawNode> {
    skip_ws(chars, pos);
    let inside = read_parenthesized(chars, pos)?;
    let (each_var, iterable_expr) = split_for_header(&inside)?;
    skip_ws(chars, pos);
    let children = read_block(chars, pos, errors)?;

    let mut empty_children: Vec<RawNode> = Vec::new();

    let save = *pos;
    skip_ws(chars, pos);
    if *pos + 5 < chars.len()
        && chars[*pos] == '@'
        && chars[*pos + 1] == 'e'
        && chars[*pos + 2] == 'm'
        && chars[*pos + 3] == 'p'
        && chars[*pos + 4] == 't'
        && chars[*pos + 5] == 'y'
    {
        *pos += 6;
        skip_ws(chars, pos);
        empty_children = read_block(chars, pos, errors)?;
    } else {
        *pos = save;
    }

    Some(RawNode::ForLoop(RawForLoop {
        each_var,
        iterable_expr,
        children,
        empty_children,
    }))
}

/// `item in expr` → `("item", "expr")`. Requer ` in ` literal (com espaços dos dois lados).
fn split_for_header(inside: &str) -> Option<(String, String)> {
    let idx = inside.find(" in ")?;
    let var = inside[..idx].trim().to_string();
    let expr = inside[idx + 4..].trim().to_string();
    if var.is_empty() || expr.is_empty() {
        return None;
    }
    Some((var, expr))
}

/// `@object(scope) { ... }`
fn parse_object_directive(
    chars: &[char],
    pos: &mut usize,
    errors: &mut ParseErrors,
) -> Option<RawNode> {
    skip_ws(chars, pos);
    let scope = read_parenthesized(chars, pos)?;
    skip_ws(chars, pos);
    let children = read_block(chars, pos, errors)?;
    Some(RawNode::Object(RawObject { scope, children }))
}

/// Lê `{ ... }` balanceado, parseando nós dentro. `pos` aponta pra `{` na entrada;
/// sai logo após `}`.
fn read_block(
    chars: &[char],
    pos: &mut usize,
    errors: &mut ParseErrors,
) -> Option<Vec<RawNode>> {
    if *pos >= chars.len() || chars[*pos] != '{' {
        return None;
    }
    *pos += 1;
    let nodes = parse_nodes_until(chars, pos, Some('}'), errors);
    if *pos < chars.len() && chars[*pos] == '}' {
        *pos += 1;
    }
    Some(nodes)
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

/// Salta um comentário HTML `<!-- ... -->`, avançando `pos` até após `-->`.
fn skip_comment(chars: &[char], pos: &mut usize) {
    *pos += 4; // skip '<!--'
    while *pos + 2 < chars.len() {
        if chars[*pos] == '-' && chars[*pos + 1] == '-' && chars[*pos + 2] == '>' {
            *pos += 3;
            return;
        }
        *pos += 1;
    }
    *pos = chars.len();
}

fn parse_element_chars(
    chars: &[char],
    pos: &mut usize,
    errors: &mut ParseErrors,
) -> Option<RawElement> {
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
    let mut goto_page = None;
    let mut goto_with = None;
    skip_ws(chars, pos);
    while *pos < chars.len() && chars[*pos] != '>' && chars[*pos] != '/' {
        if chars[*pos] == '@' {
            *pos += 1;
            let attr = read_ident(chars, pos);
            skip_ws(chars, pos);
            if *pos < chars.len() && chars[*pos] == '=' {
                *pos += 1;
                skip_ws(chars, pos);
                let value = read_quoted(chars, pos);
                match attr.as_str() {
                    "click" => on_click = Some(value),
                    "bind" => bind_expr = Some(value),
                    "goto" => goto_page = Some(value),
                    "with" => goto_with = Some(value),
                    other => errors.push(format!(
                        "Den: atributo `@{other}` desconhecido em <{tag}>"
                    )),
                }
            } else {
                errors.push(format!(
                    "Den: atributo `@{attr}` precisa de valor (`@{attr}=\"...\"`)"
                ));
            }
        } else {
            let attr_name = read_ident(chars, pos);
            skip_ws(chars, pos);
            if *pos < chars.len() && chars[*pos] == '=' {
                *pos += 1;
                skip_ws(chars, pos);
                let value = read_quoted(chars, pos);
                match attr_name.as_str() {
                    "class" => {
                        classes = value.split_whitespace().map(|s| s.to_string()).collect();
                    }
                    "den-bind" => den_bind = Some(value),
                    "placeholder" => placeholder = Some(value),
                    "bind" | "goto" | "with" => {
                        errors.push(format!(
                            "Den: `{attr_name}=\"...\"` foi substituído por `@{attr_name}=\"...\"`"
                        ));
                    }
                    _ => {
                        // atributos desconhecidos (ex: `dev`, `aria-*`): ignora
                    }
                }
            }
        }
        skip_ws(chars, pos);
    }

    // Guarda contra sintaxe legada `(click)="..."`
    // Detectada quando chars[*pos] == '(' (não deveria ocorrer em tag válida).
    // Se chegou aqui com '(' ainda antes do '>', é erro antigo.

    let is_void = tag == "input";
    if (*pos < chars.len() && chars[*pos] == '/') || is_void {
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
            goto_page,
            goto_with,
        });
    }
    if *pos < chars.len() && chars[*pos] == '>' {
        *pos += 1;
    }

    // Lê conteúdo: pode intercalar texto (inclui `{{ }}`) com filhos (tags, `@` blocks).
    let mut raw_text = String::new();
    let mut children = Vec::new();

    while *pos < chars.len() {
        if chars[*pos] == '<' {
            if *pos + 1 < chars.len() && chars[*pos + 1] == '/' {
                // Closing tag deste elemento
                while *pos < chars.len() && chars[*pos] != '>' {
                    *pos += 1;
                }
                if *pos < chars.len() {
                    *pos += 1;
                }
                break;
            }
            // Possivelmente comentário
            if *pos + 3 < chars.len()
                && chars[*pos + 1] == '!'
                && chars[*pos + 2] == '-'
                && chars[*pos + 3] == '-'
            {
                skip_comment(chars, pos);
                continue;
            }
            let tag_name = peek_tag_name(chars, *pos);
            if tag_name == "for" || tag_name == "if" || tag_name == "else" {
                errors.push(format!(
                    "Den: `<{tag_name}>` não é mais suportado. Use `@{tag_name}(...)`/`!` com blocos {{ }}."
                ));
                *pos = skip_until_gt(chars, *pos);
                continue;
            }
            if let Some(el) = parse_element_chars(chars, pos, errors) {
                // raw_text é mantido como segments do pai (comportamento original).
                children.push(RawNode::Element(el));
            } else {
                *pos += 1;
            }
        } else if chars[*pos] == '@' {
            if let Some(node) = parse_at_directive(chars, pos, errors) {
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
        goto_page,
        goto_with,
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

    fn assert_goto_attrs(html: &str, page: &str, with: Option<&str>) {
        let nodes = parse_html_ok(html);
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            RawNode::Element(el) => {
                assert_eq!(el.goto_page.as_deref(), Some(page));
                assert_eq!(el.goto_with.as_deref(), with);
            }
            _ => panic!("expected Element"),
        }
    }

    #[test]
    fn input_bind_parsed() {
        let nodes =
            parse_html_ok(r#"<input @bind="self.name" placeholder="Nome" class="inp" />"#);
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
        let nodes = parse_html_ok(r#"<input class="inp" />"#);
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
        let nodes = parse_html_ok(r#"<input @bind="self.x">"#);
        assert_eq!(nodes.len(), 1);
        if let RawNode::Element(el) = &nodes[0] {
            assert!(el.children.is_empty());
        } else {
            panic!("expected Element");
        }
    }

    #[test]
    fn input_inside_div() {
        let nodes =
            parse_html_ok(r#"<div class="form"><input @bind="self.email" /></div>"#);
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

    #[test]
    fn goto_attr_parsed() {
        assert_goto_attrs(
            r#"<div @goto="TargetPage" class="btn">Abrir</div>"#,
            "TargetPage",
            None,
        );
    }

    #[test]
    fn goto_with_attr_parsed() {
        assert_goto_attrs(
            r#"<div @goto="TargetPage" @with="self.value">Enviar</div>"#,
            "TargetPage",
            Some("self.value"),
        );
    }

    #[test]
    fn click_attr_parsed() {
        let nodes = parse_html_ok(r#"<div class="btn" @click="increment()">x</div>"#);
        if let RawNode::Element(el) = &nodes[0] {
            assert_eq!(el.on_click.as_deref(), Some("increment()"));
        } else {
            panic!();
        }
    }

    #[test]
    fn at_if_simple() {
        let nodes = parse_html_ok(
            r#"@if(self.active) {
                <div class="ok">Yes</div>
            } ! {
                <div class="no">No</div>
            }"#,
        );
        assert_eq!(nodes.len(), 1);
        if let RawNode::IfChain(ic) = &nodes[0] {
            assert_eq!(ic.branches.len(), 1);
            assert_eq!(ic.branches[0].condition, "self.active");
            assert_eq!(ic.branches[0].children.len(), 1);
            assert_eq!(ic.else_children.len(), 1);
        } else {
            panic!("expected IfChain");
        }
    }

    #[test]
    fn at_if_chain_with_bang_conditions() {
        let nodes = parse_html_ok(
            r#"@if(self.status == "active") {
                <div class="green">Ativo</div>
            } !status == "pending" {
                <div class="yellow">Pendente</div>
            } !status == "error" {
                <div class="red">Erro</div>
            } ! {
                <div>Desconhecido</div>
            }"#,
        );
        if let RawNode::IfChain(ic) = &nodes[0] {
            assert_eq!(ic.branches.len(), 3);
            assert_eq!(ic.branches[0].condition, r#"self.status == "active""#);
            assert_eq!(ic.branches[1].condition, r#"self.status == "pending""#);
            assert_eq!(ic.branches[2].condition, r#"self.status == "error""#);
            assert_eq!(ic.else_children.len(), 1);
        } else {
            panic!("expected IfChain");
        }
    }

    #[test]
    fn at_for_simple() {
        let nodes = parse_html_ok(
            r#"@for(tag in self.tags) {
                <div class="tag">{{ tag }}</div>
            }"#,
        );
        assert_eq!(nodes.len(), 1);
        if let RawNode::ForLoop(fl) = &nodes[0] {
            assert_eq!(fl.each_var, "tag");
            assert_eq!(fl.iterable_expr, "self.tags");
            assert_eq!(fl.children.len(), 1);
            assert!(fl.empty_children.is_empty());
        } else {
            panic!("expected ForLoop");
        }
    }

    #[test]
    fn at_for_with_empty() {
        let nodes = parse_html_ok(
            r#"@for(tag in self.tags) {
                <div class="tag">{{ tag }}</div>
            } @empty {
                <div class="none">Nenhuma</div>
            }"#,
        );
        if let RawNode::ForLoop(fl) = &nodes[0] {
            assert_eq!(fl.children.len(), 1);
            assert_eq!(fl.empty_children.len(), 1);
        } else {
            panic!("expected ForLoop");
        }
    }

    #[test]
    fn at_object_wraps_children() {
        let nodes = parse_html_ok(
            r#"@object(self.pessoa) {
                <input @bind="nome" />
                <input @bind="telefone" />
            }"#,
        );
        assert_eq!(nodes.len(), 1);
        if let RawNode::Object(obj) = &nodes[0] {
            assert_eq!(obj.scope, "self.pessoa");
            assert_eq!(obj.children.len(), 2);
        } else {
            panic!("expected Object");
        }
    }

    #[test]
    fn at_if_inside_element() {
        let nodes = parse_html_ok(
            r#"<div class="card">
                @if(self.active) {
                    <span class="on">On</span>
                } ! {
                    <span class="off">Off</span>
                }
            </div>"#,
        );
        if let RawNode::Element(el) = &nodes[0] {
            assert_eq!(el.children.len(), 1);
            assert!(matches!(el.children[0], RawNode::IfChain(_)));
        } else {
            panic!();
        }
    }
}
