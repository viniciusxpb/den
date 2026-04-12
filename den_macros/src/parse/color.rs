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
