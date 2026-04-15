//! Sistema de pipes do Den — transformação de valores em templates.
//!
//! A pipeline é unidirecional: `{{ expr | pipe | pipe(arg) }}` aplica os pipes da
//! esquerda pra direita, passando o resultado de um como entrada do próximo.
//!
//! Cada pipe é um struct que implementa `Pipe<T>`. O compilador Rust valida
//! o tipo de entrada de cada pipe em compile-time (é o argumento matador do Den
//! sobre pipes do Angular — runtime error vira compile error).
//!
//! ## Exemplo de uso no template
//!
//! ```text
//! {{ self.name | upper }}
//! {{ self.bio  | truncate(80) | upper }}
//! {{ self.tags | join(", ") }}
//! ```
//!
//! ## Implementando um pipe custom no app
//!
//! ```ignore
//! use den_layout::Pipe;
//!
//! pub struct Brl;
//!
//! impl Pipe<f64> for Brl {
//!     fn transform(value: f64, _args: &[&str]) -> String {
//!         format!("R$ {:.2}", value).replace('.', ",")
//!     }
//! }
//! ```
//!
//! E exporte sob `crate::pipes::Brl` (módulo `pub mod pipes { pub use crate::Brl; }`)
//! pra que o macro possa resolver `{{ self.x | brl }}` → `crate::pipes::Brl`.

/// Contrato de um pipe — recebe um valor de tipo `T` + argumentos literais textuais
/// e devolve `String` pronta pra interpolação.
///
/// Os argumentos vêm como literais textuais direto do template (`"80"`, `"br"`, `"dd/MM"`),
/// sem parsing — cada pipe escolhe como interpretar.
pub trait Pipe<T> {
    fn transform(value: T, args: &[&str]) -> String;
}

/// `{{ x | upper }}` — uppercase.
pub struct Upper;

impl<T: ToString> Pipe<T> for Upper {
    fn transform(value: T, _args: &[&str]) -> String {
        value.to_string().to_uppercase()
    }
}

/// `{{ x | lower }}` — lowercase.
pub struct Lower;

impl<T: ToString> Pipe<T> for Lower {
    fn transform(value: T, _args: &[&str]) -> String {
        value.to_string().to_lowercase()
    }
}

/// `{{ x | trim }}` — remove whitespace das pontas.
pub struct Trim;

impl<T: ToString> Pipe<T> for Trim {
    fn transform(value: T, _args: &[&str]) -> String {
        value.to_string().trim().to_string()
    }
}

/// `{{ x | truncate(n) }}` — corta em `n` chars, adiciona `…` se truncou.
pub struct Truncate;

impl<T: ToString> Pipe<T> for Truncate {
    fn transform(value: T, args: &[&str]) -> String {
        let s = value.to_string();
        let n: usize = args.first().and_then(|a| a.parse().ok()).unwrap_or(usize::MAX);
        if s.chars().count() <= n {
            s
        } else {
            let truncated: String = s.chars().take(n).collect();
            format!("{truncated}…")
        }
    }
}

/// `{{ x | default("fallback") }}` — se vazio/None, usa o argumento.
///
/// Nome do struct é `OrDefault` pra não colidir com `std::default::Default`;
/// no template continua sendo `| default(...)` (mapeado no codegen).
pub struct OrDefault;

impl Pipe<String> for OrDefault {
    fn transform(value: String, args: &[&str]) -> String {
        if value.is_empty() {
            strip_quotes(args.first().copied().unwrap_or("")).to_string()
        } else {
            value
        }
    }
}

impl Pipe<&String> for OrDefault {
    fn transform(value: &String, args: &[&str]) -> String {
        if value.is_empty() {
            strip_quotes(args.first().copied().unwrap_or("")).to_string()
        } else {
            value.clone()
        }
    }
}

impl Pipe<&str> for OrDefault {
    fn transform(value: &str, args: &[&str]) -> String {
        if value.is_empty() {
            strip_quotes(args.first().copied().unwrap_or("")).to_string()
        } else {
            value.to_string()
        }
    }
}

impl<T: ToString> Pipe<Option<T>> for OrDefault {
    fn transform(value: Option<T>, args: &[&str]) -> String {
        match value {
            Some(v) => v.to_string(),
            None => strip_quotes(args.first().copied().unwrap_or("")).to_string(),
        }
    }
}

impl<T: ToString + Clone> Pipe<&Option<T>> for OrDefault {
    fn transform(value: &Option<T>, args: &[&str]) -> String {
        match value {
            Some(v) => v.to_string(),
            None => strip_quotes(args.first().copied().unwrap_or("")).to_string(),
        }
    }
}

/// `{{ x | currency }}` / `{{ x | currency(br) }}` — formatação monetária simples.
/// Primeiro arg é locale (`br`, `us`, `jp`, ...). Default = `br`.
pub struct Currency;

impl Pipe<f64> for Currency {
    fn transform(value: f64, args: &[&str]) -> String {
        let locale = args.first().copied().unwrap_or("br");
        format_currency(value, locale)
    }
}

impl Pipe<&f64> for Currency {
    fn transform(value: &f64, args: &[&str]) -> String {
        <Currency as Pipe<f64>>::transform(*value, args)
    }
}

impl Pipe<f32> for Currency {
    fn transform(value: f32, args: &[&str]) -> String {
        <Currency as Pipe<f64>>::transform(value as f64, args)
    }
}

impl Pipe<&f32> for Currency {
    fn transform(value: &f32, args: &[&str]) -> String {
        <Currency as Pipe<f32>>::transform(*value, args)
    }
}

/// Alias com a mesma assinatura (`money(locale)`) — nome preferido pelo doc.
pub struct Money;

impl Pipe<f64> for Money {
    fn transform(value: f64, args: &[&str]) -> String {
        <Currency as Pipe<f64>>::transform(value, args)
    }
}

impl Pipe<&f64> for Money {
    fn transform(value: &f64, args: &[&str]) -> String {
        <Currency as Pipe<f64>>::transform(*value, args)
    }
}

impl Pipe<f32> for Money {
    fn transform(value: f32, args: &[&str]) -> String {
        <Currency as Pipe<f32>>::transform(value, args)
    }
}

impl Pipe<&f32> for Money {
    fn transform(value: &f32, args: &[&str]) -> String {
        <Currency as Pipe<f32>>::transform(*value, args)
    }
}

fn format_currency(v: f64, locale: &str) -> String {
    match locale {
        "br" => format!("R$ {:.2}", v).replace('.', ","),
        "us" => format!("$ {:.2}", v),
        "eu" | "de" => format!("€ {:.2}", v).replace('.', ","),
        "jp" => format!("¥ {:.0}", v),
        _ => format!("{:.2}", v),
    }
}

/// `{{ x | number(casas) }}` — decimais fixos. Default = 2.
pub struct Number;

impl Pipe<f64> for Number {
    fn transform(value: f64, args: &[&str]) -> String {
        let casas: usize = args.first().and_then(|a| a.parse().ok()).unwrap_or(2);
        format!("{:.*}", casas, value)
    }
}

impl Pipe<&f64> for Number {
    fn transform(value: &f64, args: &[&str]) -> String {
        <Number as Pipe<f64>>::transform(*value, args)
    }
}

impl Pipe<f32> for Number {
    fn transform(value: f32, args: &[&str]) -> String {
        <Number as Pipe<f64>>::transform(value as f64, args)
    }
}

impl Pipe<&f32> for Number {
    fn transform(value: &f32, args: &[&str]) -> String {
        <Number as Pipe<f32>>::transform(*value, args)
    }
}

impl Pipe<i64> for Number {
    fn transform(value: i64, _args: &[&str]) -> String {
        value.to_string()
    }
}

impl Pipe<&i64> for Number {
    fn transform(value: &i64, _args: &[&str]) -> String {
        value.to_string()
    }
}

impl Pipe<u64> for Number {
    fn transform(value: u64, _args: &[&str]) -> String {
        value.to_string()
    }
}

impl Pipe<&u64> for Number {
    fn transform(value: &u64, _args: &[&str]) -> String {
        value.to_string()
    }
}

/// `{{ x | join(", ") }}` — junta `Vec<T: ToString>` com separador.
pub struct Join;

impl<T: ToString> Pipe<&Vec<T>> for Join {
    fn transform(value: &Vec<T>, args: &[&str]) -> String {
        let sep = strip_quotes(args.first().copied().unwrap_or(", "));
        value
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(sep)
    }
}

impl<T: ToString> Pipe<Vec<T>> for Join {
    fn transform(value: Vec<T>, args: &[&str]) -> String {
        <Join as Pipe<&Vec<T>>>::transform(&value, args)
    }
}

/// `{{ x | date("dd/MM/yyyy") }}` — formatação de data.
///
/// **Não implementado ainda.** Retorna a string do valor sem formatar e loga
/// um warning uma vez por chamada. Integração com `chrono`/`time` tá em
/// `PENDING.md` — por ora é um stub explícito pra não silenciar o caso.
pub struct Date;

impl<T: ToString> Pipe<T> for Date {
    fn transform(value: T, args: &[&str]) -> String {
        let fmt = args.first().copied().unwrap_or("(sem formato)");
        eprintln!(
            "Den pipes: `date({fmt})` ainda não está implementado — \
             retornando valor bruto. Ver PENDING.md."
        );
        value.to_string()
    }
}

fn strip_quotes(s: &str) -> &str {
    let t = s.trim();
    // UTF-8 safety: só fatia bytes-1..len-1 se os bounds caírem em boundaries. `"` e `'`
    // ASCII ocupam 1 byte cada, então buscamos explicitamente as posições via char_indices
    // pra não assumir alinhamento.
    let mut chars = t.char_indices();
    let (_, first) = match chars.next() {
        Some(x) => x,
        None => return t,
    };
    let (last_start, last) = match t.char_indices().next_back() {
        Some(x) => x,
        None => return t,
    };
    let is_matching = (first == '"' && last == '"') || (first == '\'' && last == '\'');
    if !is_matching {
        return t;
    }
    let first_len = first.len_utf8();
    if first_len >= last_start {
        return t; // string de 1 char só
    }
    &t[first_len..last_start]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn upper_from_str() {
        assert_eq!(<Upper as Pipe<&str>>::transform("hi", &[]), "HI");
    }

    #[test]
    fn truncate_respects_length() {
        assert_eq!(
            <Truncate as Pipe<String>>::transform("abcdefg".to_string(), &["3"]),
            "abc…"
        );
        assert_eq!(
            <Truncate as Pipe<String>>::transform("ab".to_string(), &["5"]),
            "ab"
        );
    }

    #[test]
    fn currency_br_format() {
        assert_eq!(
            <Currency as Pipe<f64>>::transform(42.5, &["br"]),
            "R$ 42,50"
        );
        assert_eq!(<Currency as Pipe<f64>>::transform(42.5, &["us"]), "$ 42.50");
    }

    #[test]
    fn default_empty_string() {
        assert_eq!(
            <OrDefault as Pipe<&str>>::transform("", &["\"Anônimo\""]),
            "Anônimo"
        );
        assert_eq!(<OrDefault as Pipe<&str>>::transform("João", &[]), "João");
    }

    #[test]
    fn default_option() {
        let some: Option<String> = Some("Ana".to_string());
        let none: Option<String> = None;
        assert_eq!(
            <OrDefault as Pipe<Option<String>>>::transform(some, &["\"X\""]),
            "Ana"
        );
        assert_eq!(
            <OrDefault as Pipe<Option<String>>>::transform(none, &["\"X\""]),
            "X"
        );
    }

    #[test]
    fn join_vec() {
        let v = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        assert_eq!(<Join as Pipe<Vec<String>>>::transform(v, &["\", \""]), "a, b, c");
    }

    #[test]
    fn number_fixed_decimals() {
        assert_eq!(<Number as Pipe<f64>>::transform(1.23456, &["2"]), "1.23");
        assert_eq!(<Number as Pipe<f64>>::transform(1.23456, &["4"]), "1.2346");
    }
}
