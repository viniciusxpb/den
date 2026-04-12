use crate::types::RgbColor;

/// Parse `#RGB` ou `#RRGGBB` → (r, g, b).
pub fn parse_hex_color(hex: &str) -> Option<RgbColor> {
    let hex = hex.trim_start_matches('#');
    let expanded = if hex.len() == 3 {
        hex.chars().flat_map(|c| [c, c]).collect::<String>()
    } else {
        hex.to_string()
    };

    if expanded.len() < 6 {
        eprintln!("Den: invalid hex color '#{hex}', expected #RGB or #RRGGBB");
        return None;
    }

    let r = u8::from_str_radix(&expanded[0..2], 16).ok()?;
    let g = u8::from_str_radix(&expanded[2..4], 16).ok()?;
    let b = u8::from_str_radix(&expanded[4..6], 16).ok()?;
    Some((r, g, b))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_rrggbb() {
        assert_eq!(parse_hex_color("#e94560"), Some((233, 69, 96)));
    }

    #[test]
    fn parses_rgb_shorthand() {
        assert_eq!(parse_hex_color("#fff"), Some((255, 255, 255)));
        assert_eq!(parse_hex_color("#000"), Some((0, 0, 0)));
        assert_eq!(parse_hex_color("#f0a"), Some((255, 0, 170)));
    }

    #[test]
    fn parses_without_hash() {
        assert_eq!(parse_hex_color("ff0000"), Some((255, 0, 0)));
    }

    #[test]
    fn returns_none_on_invalid() {
        assert_eq!(parse_hex_color("#zzzzzz"), None);
        assert_eq!(parse_hex_color("#12"), None);
    }
}
