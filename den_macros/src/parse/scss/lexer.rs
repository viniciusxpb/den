//! Helpers byte-level usados pelo parser SCSS.
//!
//! SCSS identifiers são ASCII-only, então parsing byte-level é seguro aqui
//! (sem custo de decode UTF-8 fora de string literais).

/// Avança `pos` enquanto encontrar whitespace ASCII.
pub(super) fn skip_whitespace(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos].is_ascii_whitespace() {
        *pos += 1;
    }
}

/// Pula comentários `// ...` e `/* ... */`, retornando se consumiu algo.
pub(super) fn skip_comment(bytes: &[u8], pos: &mut usize) -> bool {
    if *pos + 1 >= bytes.len() || bytes[*pos] != b'/' {
        return false;
    }

    if bytes[*pos + 1] == b'/' {
        *pos += 2;
        while *pos < bytes.len() && bytes[*pos] != b'\n' {
            *pos += 1;
        }
        return true;
    }

    if bytes[*pos + 1] == b'*' {
        *pos += 2;
        while *pos + 1 < bytes.len() && !(bytes[*pos] == b'*' && bytes[*pos + 1] == b'/') {
            *pos += 1;
        }
        *pos = (*pos + 2).min(bytes.len());
        return true;
    }

    false
}

/// Avança até o fim de uma declaração inválida sem consumir a chave de fechamento.
pub(super) fn skip_invalid_declaration(bytes: &[u8], pos: &mut usize) {
    while *pos < bytes.len() && bytes[*pos] != b';' && bytes[*pos] != b'}' {
        *pos += 1;
    }
    if *pos < bytes.len() && bytes[*pos] == b';' {
        *pos += 1;
    }
}

pub(super) fn read_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len() && is_ident_char(bytes[*pos]) {
        *pos += 1;
    }
    std::str::from_utf8(&bytes[start..*pos])
        .unwrap_or("")
        .to_string()
}

/// Caractere válido em identificador CSS/SCSS (letra/dígito/`_`/`-`).
pub(super) fn is_ident_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'-'
}

pub(super) fn read_css_identifier(bytes: &[u8], pos: &mut usize) -> String {
    let start = *pos;
    while *pos < bytes.len()
        && (bytes[*pos].is_ascii_alphanumeric() || bytes[*pos] == b'-' || bytes[*pos] == b'_')
    {
        *pos += 1;
    }
    std::str::from_utf8(&bytes[start..*pos])
        .unwrap_or("")
        .to_string()
}
