use crate::types::RgbColor;

/// Parseia uma expressão de cor CSS: hex (`#RGB`, `#RRGGBB`, `#RGBA`, `#RRGGBBAA`),
/// `rgb(r, g, b)`, ou `rgba(r, g, b, a)`. Retorna `(r, g, b, a)` com alpha `255`
/// quando não declarado.
///
/// Mantém o nome `parse_hex_color` por retrocompatibilidade com os call sites
/// (parser SCSS, border shorthand). Despacha internamente pra `rgb()`/`rgba()`
/// quando o valor começa com `rgb`.
pub fn parse_hex_color(raw: &str) -> Option<RgbColor> {
    let trimmed = raw.trim();
    if let Some(inner) = strip_func(trimmed, "rgba") {
        return parse_rgba_args(inner);
    }
    if let Some(inner) = strip_func(trimmed, "rgb") {
        return parse_rgb_args(inner);
    }
    parse_hex(trimmed)
}

/// Extrai `ARGS` de `name(ARGS)` (case-insensitive no prefixo).
/// Retorna `None` se não bater o prefixo ou faltar o `)` final.
fn strip_func<'a>(value: &'a str, name: &str) -> Option<&'a str> {
    if value.len() < name.len() + 2 {
        return None;
    }
    let (head, rest) = value.split_at(name.len());
    if !head.eq_ignore_ascii_case(name) {
        return None;
    }
    let rest = rest.trim_start();
    let inner = rest.strip_prefix('(')?.strip_suffix(')')?;
    Some(inner)
}

/// Parseia os argumentos de `rgb(R, G, B)`. Separadores aceitos: vírgula OU
/// espaço (sintaxe CSS moderna). Canais em 0..=255 (inteiros) ou `X%`.
fn parse_rgb_args(inner: &str) -> Option<RgbColor> {
    let parts = split_color_args(inner);
    if parts.len() != 3 {
        eprintln!(
            "Den: rgb(..) requer exatamente 3 argumentos (r, g, b), recebi {}",
            parts.len()
        );
        return None;
    }
    let r = parse_channel(parts[0])?;
    let g = parse_channel(parts[1])?;
    let b = parse_channel(parts[2])?;
    Some((r, g, b, 255))
}

/// Parseia os argumentos de `rgba(R, G, B, A)`. Alpha em 0..=1 (float CSS).
fn parse_rgba_args(inner: &str) -> Option<RgbColor> {
    let parts = split_color_args(inner);
    if parts.len() != 4 {
        eprintln!(
            "Den: rgba(..) requer exatamente 4 argumentos (r, g, b, a), recebi {}",
            parts.len()
        );
        return None;
    }
    let r = parse_channel(parts[0])?;
    let g = parse_channel(parts[1])?;
    let b = parse_channel(parts[2])?;
    let a = parse_alpha(parts[3])?;
    Some((r, g, b, a))
}

/// Divide argumentos de `rgb()/rgba()` por `,` ou `/` (separador CSS moderno)
/// ou whitespace. Não preserva parênteses aninhados — valores aqui são escalares.
fn split_color_args(inner: &str) -> Vec<&str> {
    inner
        .split(|c: char| c == ',' || c == '/' || c.is_ascii_whitespace())
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect()
}

/// Parseia canal RGB em `0..=255`. Aceita inteiro puro ou `N%`.
fn parse_channel(raw: &str) -> Option<u8> {
    if let Some(pct) = raw.strip_suffix('%')
        && let Ok(v) = pct.trim().parse::<f32>()
    {
        let clamped = (v / 100.0 * 255.0).round().clamp(0.0, 255.0);
        return Some(clamped as u8);
    }
    raw.parse::<u16>().ok().map(|v| v.min(255) as u8)
}

/// Parseia canal alpha em `0.0..=1.0` (ou `0..=100%`) → `u8` em `0..=255`.
fn parse_alpha(raw: &str) -> Option<u8> {
    if let Some(pct) = raw.strip_suffix('%')
        && let Ok(v) = pct.trim().parse::<f32>()
    {
        return Some((v / 100.0 * 255.0).round().clamp(0.0, 255.0) as u8);
    }
    let v = raw.parse::<f32>().ok()?;
    Some((v * 255.0).round().clamp(0.0, 255.0) as u8)
}

/// Parseia `#RGB`, `#RRGGBB`, `#RGBA`, `#RRGGBBAA` (o `#` é opcional, aceita sem).
fn parse_hex(hex: &str) -> Option<RgbColor> {
    let hex = hex.trim_start_matches('#');
    let (rgb_part, alpha_part) = match hex.len() {
        3 => (&hex[0..3], None),
        4 => (&hex[0..3], Some(&hex[3..4])),
        6 => (&hex[0..6], None),
        8 => (&hex[0..6], Some(&hex[6..8])),
        _ => {
            eprintln!(
                "Den: invalid hex color '#{hex}', expected #RGB, #RRGGBB, #RGBA ou #RRGGBBAA"
            );
            return None;
        }
    };

    let expanded: String = if rgb_part.len() == 3 {
        rgb_part.chars().flat_map(|c| [c, c]).collect()
    } else {
        rgb_part.to_string()
    };
    let r = u8::from_str_radix(&expanded[0..2], 16).ok()?;
    let g = u8::from_str_radix(&expanded[2..4], 16).ok()?;
    let b = u8::from_str_radix(&expanded[4..6], 16).ok()?;

    let a = match alpha_part {
        None => 255,
        Some(s) if s.len() == 1 => {
            // #RGBA: single-digit alpha expande dobrando (ex.: "8" -> "88").
            u8::from_str_radix(&format!("{s}{s}"), 16).ok()?
        }
        Some(s) => u8::from_str_radix(s, 16).ok()?,
    };

    Some((r, g, b, a))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_rrggbb() {
        assert_eq!(parse_hex_color("#e94560"), Some((233, 69, 96, 255)));
    }

    #[test]
    fn parses_rgb_shorthand() {
        assert_eq!(parse_hex_color("#fff"), Some((255, 255, 255, 255)));
        assert_eq!(parse_hex_color("#000"), Some((0, 0, 0, 255)));
        assert_eq!(parse_hex_color("#f0a"), Some((255, 0, 170, 255)));
    }

    #[test]
    fn parses_without_hash() {
        assert_eq!(parse_hex_color("ff0000"), Some((255, 0, 0, 255)));
    }

    #[test]
    fn parses_rrggbbaa() {
        assert_eq!(parse_hex_color("#ff000080"), Some((255, 0, 0, 128)));
        assert_eq!(parse_hex_color("#000000ff"), Some((0, 0, 0, 255)));
        assert_eq!(parse_hex_color("#00000000"), Some((0, 0, 0, 0)));
    }

    #[test]
    fn parses_rgba_shorthand() {
        // #RGBA expande cada dígito: "f" -> "ff", "8" -> "88"
        assert_eq!(parse_hex_color("#f008"), Some((255, 0, 0, 136)));
    }

    #[test]
    fn parses_rgb_func() {
        assert_eq!(parse_hex_color("rgb(255, 0, 128)"), Some((255, 0, 128, 255)));
        assert_eq!(parse_hex_color("rgb(10 20 30)"), Some((10, 20, 30, 255)));
        assert_eq!(parse_hex_color("rgb(50%, 50%, 50%)"), Some((128, 128, 128, 255)));
    }

    #[test]
    fn parses_rgba_func() {
        assert_eq!(parse_hex_color("rgba(255, 0, 0, 0.5)"), Some((255, 0, 0, 128)));
        assert_eq!(parse_hex_color("rgba(0, 212, 170, 0.15)"), Some((0, 212, 170, 38)));
        assert_eq!(parse_hex_color("rgba(0, 0, 0, 1)"), Some((0, 0, 0, 255)));
        assert_eq!(parse_hex_color("rgba(0, 0, 0, 0)"), Some((0, 0, 0, 0)));
    }

    #[test]
    fn rgba_alpha_percent() {
        assert_eq!(parse_hex_color("rgba(0, 0, 0, 50%)"), Some((0, 0, 0, 128)));
    }

    #[test]
    fn returns_none_on_invalid() {
        assert_eq!(parse_hex_color("#zzzzzz"), None);
        assert_eq!(parse_hex_color("#12"), None);
        assert_eq!(parse_hex_color("rgb(1, 2)"), None);
        assert_eq!(parse_hex_color("rgba(1, 2, 3)"), None);
    }
}
