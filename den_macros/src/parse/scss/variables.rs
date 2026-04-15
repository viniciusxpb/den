//! Coleta e resolução de variáveis SCSS (`$nome: valor;`).
//!
//! DUPLICAÇÃO: lógica similar em preview.rs e style_editor.rs. Extrair pra
//! `den_core` no futuro (ver PENDING.md).

use super::lexer::{read_identifier, skip_comment, skip_whitespace};
use std::collections::HashMap;

/// Coleta todas as declarações `$nome: valor;` do SCSS.
pub(super) fn collect_variables(input: &str) -> HashMap<String, String> {
    let mut vars = HashMap::new();
    let bytes = input.as_bytes();
    let mut pos = 0;

    while pos < bytes.len() {
        skip_whitespace(bytes, &mut pos);
        if pos >= bytes.len() {
            break;
        }
        if skip_comment(bytes, &mut pos) {
            continue;
        }

        if bytes[pos] == b'$' {
            pos += 1; // skip '$'
            let name = read_identifier(bytes, &mut pos);
            skip_whitespace(bytes, &mut pos);
            if pos < bytes.len() && bytes[pos] == b':' {
                pos += 1; // skip ':'
                skip_whitespace(bytes, &mut pos);
                let start = pos;
                while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b'\n' {
                    pos += 1;
                }
                let value = std::str::from_utf8(&bytes[start..pos])
                    .unwrap_or("")
                    .trim()
                    .to_string();
                if !name.is_empty() && !value.is_empty() {
                    vars.insert(name, value);
                }
            }
        }
        pos += 1;
    }
    vars
}

/// Substitui referências `$nome` pelos valores resolvidos.
pub(super) fn resolve_vars(value: &str, vars: &HashMap<String, String>) -> String {
    if !value.contains('$') {
        return value.to_string();
    }
    let mut result = value.to_string();
    for (name, val) in vars_by_longest_name(vars) {
        result = result.replace(&format!("${name}"), val);
    }
    result
}

/// Ordena variáveis SCSS por nome descrescente para `$text-dim` vencer `$text`.
fn vars_by_longest_name(vars: &HashMap<String, String>) -> Vec<(&String, &String)> {
    let mut ordered: Vec<_> = vars.iter().collect();
    ordered.sort_by(|(a, _), (b, _)| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
    ordered
}
