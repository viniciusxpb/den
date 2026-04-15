//! Parser de expressões de cor CSS para o pipeline de macros do Den.
//!
//! Cobre as três formas que aparecem em `.scss`: hex (`#RGB`, `#RRGGBB`, `#RGBA`,
//! `#RRGGBBAA`, com `#` opcional na forma sem alpha), funções `rgb()`/`rgba()`,
//! e os 148 named colors do CSS Color Module Level 3 (mais `transparent`).
//! Tudo aterrissa no mesmo `(u8, u8, u8, u8)` consumido pelo resto do pipeline.

use crate::types::RgbColor;

/// Parseia qualquer forma CSS de cor:
/// - Hex: `#RGB`, `#RRGGBB`, `#RGBA`, `#RRGGBBAA` (e a forma legada sem `#`).
/// - Funções: `rgb(r, g, b)`, `rgba(r, g, b, a)` — vírgula ou espaço como separador.
/// - Named colors do CSS Color Module Level 3: `black`, `white`, `rebeccapurple`,
///   etc. (148 nomes), case-insensitive. `transparent` vira `(0, 0, 0, 0)`.
///
/// Retorna `(r, g, b, a)` com alpha `255` quando não declarado.
///
/// Ordem do dispatch: funções → hex com `#` → named color → hex legado sem `#`.
pub fn parse_color(raw: &str) -> Option<RgbColor> {
    let trimmed = raw.trim();
    if let Some(inner) = strip_func(trimmed, "rgba") {
        return parse_rgba_args(inner);
    }
    if let Some(inner) = strip_func(trimmed, "rgb") {
        return parse_rgb_args(inner);
    }
    if trimmed.starts_with('#') {
        return parse_hex(trimmed);
    }
    if let Some(color) = parse_named_color(trimmed) {
        return Some(color);
    }
    // Forma legada hex sem `#` (ex.: "ff0000"). Imprime warning se também falhar.
    parse_hex(trimmed)
}

/// Procura `name` no set de named colors do CSS3. Case-insensitive.
/// Retorna `None` quando o nome não é reconhecido — caller decide se vira hex
/// legado ou warning.
fn parse_named_color(name: &str) -> Option<RgbColor> {
    if name.eq_ignore_ascii_case("transparent") {
        return Some((0, 0, 0, 0));
    }
    NAMED_COLORS
        .iter()
        .find(|(n, _)| name.eq_ignore_ascii_case(n))
        .map(|(_, [r, g, b])| (*r, *g, *b, 255))
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

/// CSS Color Module Level 3 named colors — 148 nomes.
///
/// Match case-insensitive feito em `parse_named_color`. `transparent` é tratado
/// como caso especial (alpha 0) fora da tabela. Sinônimos como `aqua`/`cyan`,
/// `gray`/`grey`, `darkgray`/`darkgrey`, etc. são entradas duplicadas com o
/// mesmo RGB — combina com a spec.
///
/// Ordem alfabética por convenção (facilita auditoria); a busca é linear, a
/// ordem só importa pra leitura humana.
#[rustfmt::skip]
const NAMED_COLORS: &[(&str, [u8; 3])] = &[
    ("aliceblue",            [240, 248, 255]),
    ("antiquewhite",         [250, 235, 215]),
    ("aqua",                 [  0, 255, 255]),
    ("aquamarine",           [127, 255, 212]),
    ("azure",                [240, 255, 255]),
    ("beige",                [245, 245, 220]),
    ("bisque",               [255, 228, 196]),
    ("black",                [  0,   0,   0]),
    ("blanchedalmond",       [255, 235, 205]),
    ("blue",                 [  0,   0, 255]),
    ("blueviolet",           [138,  43, 226]),
    ("brown",                [165,  42,  42]),
    ("burlywood",            [222, 184, 135]),
    ("cadetblue",            [ 95, 158, 160]),
    ("chartreuse",           [127, 255,   0]),
    ("chocolate",            [210, 105,  30]),
    ("coral",                [255, 127,  80]),
    ("cornflowerblue",       [100, 149, 237]),
    ("cornsilk",             [255, 248, 220]),
    ("crimson",              [220,  20,  60]),
    ("cyan",                 [  0, 255, 255]),
    ("darkblue",             [  0,   0, 139]),
    ("darkcyan",             [  0, 139, 139]),
    ("darkgoldenrod",        [184, 134,  11]),
    ("darkgray",             [169, 169, 169]),
    ("darkgreen",            [  0, 100,   0]),
    ("darkgrey",             [169, 169, 169]),
    ("darkkhaki",            [189, 183, 107]),
    ("darkmagenta",          [139,   0, 139]),
    ("darkolivegreen",       [ 85, 107,  47]),
    ("darkorange",           [255, 140,   0]),
    ("darkorchid",           [153,  50, 204]),
    ("darkred",              [139,   0,   0]),
    ("darksalmon",           [233, 150, 122]),
    ("darkseagreen",         [143, 188, 143]),
    ("darkslateblue",        [ 72,  61, 139]),
    ("darkslategray",        [ 47,  79,  79]),
    ("darkslategrey",        [ 47,  79,  79]),
    ("darkturquoise",        [  0, 206, 209]),
    ("darkviolet",           [148,   0, 211]),
    ("deeppink",             [255,  20, 147]),
    ("deepskyblue",          [  0, 191, 255]),
    ("dimgray",              [105, 105, 105]),
    ("dimgrey",              [105, 105, 105]),
    ("dodgerblue",           [ 30, 144, 255]),
    ("firebrick",            [178,  34,  34]),
    ("floralwhite",          [255, 250, 240]),
    ("forestgreen",          [ 34, 139,  34]),
    ("fuchsia",              [255,   0, 255]),
    ("gainsboro",            [220, 220, 220]),
    ("ghostwhite",           [248, 248, 255]),
    ("gold",                 [255, 215,   0]),
    ("goldenrod",            [218, 165,  32]),
    ("gray",                 [128, 128, 128]),
    ("green",                [  0, 128,   0]),
    ("greenyellow",          [173, 255,  47]),
    ("grey",                 [128, 128, 128]),
    ("honeydew",             [240, 255, 240]),
    ("hotpink",              [255, 105, 180]),
    ("indianred",            [205,  92,  92]),
    ("indigo",               [ 75,   0, 130]),
    ("ivory",                [255, 255, 240]),
    ("khaki",                [240, 230, 140]),
    ("lavender",             [230, 230, 250]),
    ("lavenderblush",        [255, 240, 245]),
    ("lawngreen",            [124, 252,   0]),
    ("lemonchiffon",         [255, 250, 205]),
    ("lightblue",            [173, 216, 230]),
    ("lightcoral",           [240, 128, 128]),
    ("lightcyan",            [224, 255, 255]),
    ("lightgoldenrodyellow", [250, 250, 210]),
    ("lightgray",            [211, 211, 211]),
    ("lightgreen",           [144, 238, 144]),
    ("lightgrey",            [211, 211, 211]),
    ("lightpink",            [255, 182, 193]),
    ("lightsalmon",          [255, 160, 122]),
    ("lightseagreen",        [ 32, 178, 170]),
    ("lightskyblue",         [135, 206, 250]),
    ("lightslategray",       [119, 136, 153]),
    ("lightslategrey",       [119, 136, 153]),
    ("lightsteelblue",       [176, 196, 222]),
    ("lightyellow",          [255, 255, 224]),
    ("lime",                 [  0, 255,   0]),
    ("limegreen",            [ 50, 205,  50]),
    ("linen",                [250, 240, 230]),
    ("magenta",              [255,   0, 255]),
    ("maroon",               [128,   0,   0]),
    ("mediumaquamarine",     [102, 205, 170]),
    ("mediumblue",           [  0,   0, 205]),
    ("mediumorchid",         [186,  85, 211]),
    ("mediumpurple",         [147, 112, 219]),
    ("mediumseagreen",       [ 60, 179, 113]),
    ("mediumslateblue",      [123, 104, 238]),
    ("mediumspringgreen",    [  0, 250, 154]),
    ("mediumturquoise",      [ 72, 209, 204]),
    ("mediumvioletred",      [199,  21, 133]),
    ("midnightblue",         [ 25,  25, 112]),
    ("mintcream",            [245, 255, 250]),
    ("mistyrose",            [255, 228, 225]),
    ("moccasin",             [255, 228, 181]),
    ("navajowhite",          [255, 222, 173]),
    ("navy",                 [  0,   0, 128]),
    ("oldlace",              [253, 245, 230]),
    ("olive",                [128, 128,   0]),
    ("olivedrab",            [107, 142,  35]),
    ("orange",               [255, 165,   0]),
    ("orangered",            [255,  69,   0]),
    ("orchid",               [218, 112, 214]),
    ("palegoldenrod",        [238, 232, 170]),
    ("palegreen",            [152, 251, 152]),
    ("paleturquoise",        [175, 238, 238]),
    ("palevioletred",        [219, 112, 147]),
    ("papayawhip",           [255, 239, 213]),
    ("peachpuff",            [255, 218, 185]),
    ("peru",                 [205, 133,  63]),
    ("pink",                 [255, 192, 203]),
    ("plum",                 [221, 160, 221]),
    ("powderblue",           [176, 224, 230]),
    ("purple",               [128,   0, 128]),
    ("rebeccapurple",        [102,  51, 153]),
    ("red",                  [255,   0,   0]),
    ("rosybrown",            [188, 143, 143]),
    ("royalblue",            [ 65, 105, 225]),
    ("saddlebrown",          [139,  69,  19]),
    ("salmon",               [250, 128, 114]),
    ("sandybrown",           [244, 164,  96]),
    ("seagreen",             [ 46, 139,  87]),
    ("seashell",             [255, 245, 238]),
    ("sienna",               [160,  82,  45]),
    ("silver",               [192, 192, 192]),
    ("skyblue",              [135, 206, 235]),
    ("slateblue",            [106,  90, 205]),
    ("slategray",            [112, 128, 144]),
    ("slategrey",            [112, 128, 144]),
    ("snow",                 [255, 250, 250]),
    ("springgreen",          [  0, 255, 127]),
    ("steelblue",            [ 70, 130, 180]),
    ("tan",                  [210, 180, 140]),
    ("teal",                 [  0, 128, 128]),
    ("thistle",              [216, 191, 216]),
    ("tomato",               [255,  99,  71]),
    ("turquoise",            [ 64, 224, 208]),
    ("violet",               [238, 130, 238]),
    ("wheat",                [245, 222, 179]),
    ("white",                [255, 255, 255]),
    ("whitesmoke",           [245, 245, 245]),
    ("yellow",               [255, 255,   0]),
    ("yellowgreen",          [154, 205,  50]),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_rrggbb() {
        assert_eq!(parse_color("#e94560"), Some((233, 69, 96, 255)));
    }

    #[test]
    fn parses_rgb_shorthand() {
        assert_eq!(parse_color("#fff"), Some((255, 255, 255, 255)));
        assert_eq!(parse_color("#000"), Some((0, 0, 0, 255)));
        assert_eq!(parse_color("#f0a"), Some((255, 0, 170, 255)));
    }

    #[test]
    fn parses_without_hash() {
        assert_eq!(parse_color("ff0000"), Some((255, 0, 0, 255)));
    }

    #[test]
    fn parses_rrggbbaa() {
        assert_eq!(parse_color("#ff000080"), Some((255, 0, 0, 128)));
        assert_eq!(parse_color("#000000ff"), Some((0, 0, 0, 255)));
        assert_eq!(parse_color("#00000000"), Some((0, 0, 0, 0)));
    }

    #[test]
    fn parses_rgba_shorthand() {
        // #RGBA expande cada dígito: "f" -> "ff", "8" -> "88"
        assert_eq!(parse_color("#f008"), Some((255, 0, 0, 136)));
    }

    #[test]
    fn parses_rgb_func() {
        assert_eq!(parse_color("rgb(255, 0, 128)"), Some((255, 0, 128, 255)));
        assert_eq!(parse_color("rgb(10 20 30)"), Some((10, 20, 30, 255)));
        assert_eq!(parse_color("rgb(50%, 50%, 50%)"), Some((128, 128, 128, 255)));
    }

    #[test]
    fn parses_rgba_func() {
        assert_eq!(parse_color("rgba(255, 0, 0, 0.5)"), Some((255, 0, 0, 128)));
        assert_eq!(parse_color("rgba(0, 212, 170, 0.15)"), Some((0, 212, 170, 38)));
        assert_eq!(parse_color("rgba(0, 0, 0, 1)"), Some((0, 0, 0, 255)));
        assert_eq!(parse_color("rgba(0, 0, 0, 0)"), Some((0, 0, 0, 0)));
    }

    #[test]
    fn rgba_alpha_percent() {
        assert_eq!(parse_color("rgba(0, 0, 0, 50%)"), Some((0, 0, 0, 128)));
    }

    #[test]
    fn returns_none_on_invalid() {
        assert_eq!(parse_color("#zzzzzz"), None);
        assert_eq!(parse_color("#12"), None);
        assert_eq!(parse_color("rgb(1, 2)"), None);
        assert_eq!(parse_color("rgba(1, 2, 3)"), None);
    }

    #[test]
    fn parses_basic_named_colors() {
        assert_eq!(parse_color("black"), Some((0, 0, 0, 255)));
        assert_eq!(parse_color("white"), Some((255, 255, 255, 255)));
        assert_eq!(parse_color("red"), Some((255, 0, 0, 255)));
        assert_eq!(parse_color("lime"), Some((0, 255, 0, 255)));
        assert_eq!(parse_color("blue"), Some((0, 0, 255, 255)));
    }

    #[test]
    fn named_colors_are_case_insensitive() {
        assert_eq!(parse_color("BLACK"), Some((0, 0, 0, 255)));
        assert_eq!(parse_color("Black"), Some((0, 0, 0, 255)));
        assert_eq!(parse_color("RebeccaPurple"), Some((102, 51, 153, 255)));
    }

    #[test]
    fn transparent_is_zero_alpha() {
        assert_eq!(parse_color("transparent"), Some((0, 0, 0, 0)));
        assert_eq!(parse_color("TRANSPARENT"), Some((0, 0, 0, 0)));
    }

    #[test]
    fn css3_extended_named_colors() {
        // Garante que nomes "exóticos" do CSS3 funcionam.
        assert_eq!(parse_color("rebeccapurple"), Some((102, 51, 153, 255)));
        assert_eq!(parse_color("papayawhip"), Some((255, 239, 213, 255)));
        assert_eq!(parse_color("blanchedalmond"), Some((255, 235, 205, 255)));
    }

    #[test]
    fn gray_grey_synonyms_match() {
        // "gray" e "grey" são entradas distintas com mesmo RGB (spec CSS).
        assert_eq!(parse_color("gray"), parse_color("grey"));
        assert_eq!(parse_color("darkgray"), parse_color("darkgrey"));
        assert_eq!(parse_color("slategray"), parse_color("slategrey"));
    }

    #[test]
    fn unknown_name_returns_none_without_panic() {
        // "potato" não é cor — não deve panicar nem virar hex.
        assert_eq!(parse_color("potato"), None);
        assert_eq!(parse_color("notacolor"), None);
    }
}
