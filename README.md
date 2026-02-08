<div align="center">

# Den

**Write HTML + SCSS. Compile to native desktop UI.**

Den is a Rust framework that transforms familiar web templates into native [egui](https://github.com/emilk/egui) code at compile time. No runtime overhead. No webview. Just fast, native GUI with a developer experience you already know.

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.88%2B-orange.svg)](https://www.rust-lang.org)
[![egui](https://img.shields.io/badge/egui-0.33-purple.svg)](https://github.com/emilk/egui)

*Have you ever coded in Rust, and had to download a billion plugins, where the Den SCSS? Well... Here it is.*

</div>

---

## The Idea

Every Rust GUI framework makes you write UI in Rust. Den doesn't.

You write **HTML** for structure, **SCSS** for styling, and **Rust** for logic — each in its own file, like a proper frontend framework. At compile time, Den's proc macro reads your templates and generates native egui code. The result is a fully native application with zero runtime template parsing.

```
                    compile time
  .html + .scss  ──────────────►  native egui code  ──►  desktop app
                   den_macros
```

## Quick Start

### Prerequisites

- Rust 1.88+ (`rustup update stable`)
- Linux: `sudo apt-get install libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev libxkbcommon-dev libssl-dev`

### Create a project

```bash
cargo init my-app
```

Add Den to your workspace. Your project structure should look like:

```
my-app/
├── Cargo.toml
├── den_macros/              # proc macro crate (the compiler)
└── den_app/                 # your application
    └── src/
        ├── main.rs
        └── pages/
            └── home/
                ├── home.rs      # logic
                ├── home.html    # template
                └── home.scss    # styles
```

### Write your template

**home.html**
```html
<div class="greeting">Hello World, with style</div>
```

**home.scss**
```scss
.greeting {
    color: #e94560;
    font-size: 24;
}
```

**home.rs**
```rust
use eframe::egui;

pub struct HomePage;

impl HomePage {
    pub fn render(ui: &mut egui::Ui) {
        den_macros::den_template!("pages/home/home");
    }
}
```

That's it. Run `cargo run -p den_app` and you get a native desktop window with styled text. No egui boilerplate. No `RichText::new().color().size()` chains. Just HTML and SCSS.

## How It Works

Den uses a Rust [procedural macro](https://doc.rust-lang.org/reference/procedural-macros.html) that runs at compile time:

1. `den_template!("pages/home/home")` is called in your Rust code
2. The macro reads `home.html` and `home.scss` from the same directory
3. It parses the HTML into an element tree (tags, classes, text content)
4. It parses the SCSS into a style map (class selectors with properties)
5. It merges styles with elements by matching class names
6. It generates the equivalent egui Rust code using `quote!`
7. The compiler replaces the macro call with the generated code

**What the compiler sees after macro expansion:**

```rust
// You write:
den_macros::den_template!("pages/home/home");

// Compiler sees:
ui.label(
    egui::RichText::new("Hello World, with style")
        .color(egui::Color32::from_rgb(233, 69, 96))
        .size(24.0)
);
```

Zero runtime cost. The template is gone after compilation — it's just egui calls.

## Supported Properties

| SCSS Property | Maps To                          | Example          |
|---------------|----------------------------------|------------------|
| `color`       | `RichText::color()`              | `#e94560`        |
| `font-size`   | `RichText::size()`               | `24` or `24px`   |
| `background`  | `Frame::fill()`                  | `#1a1a2e`        |
| `padding`     | `Frame::inner_margin()`          | `16` or `16px`   |

## Supported HTML Tags

| Tag                          | Maps To          |
|------------------------------|------------------|
| `<div>`, `<span>`, `<p>`    | `ui.label()`     |
| `<heading>`, `<h1>`–`<h3>`  | `ui.heading()`   |

## Roadmap

Den is in early prototype stage. Here's what's coming:

- [ ] `<horizontal>` and `<vertical>` layout containers
- [ ] `<button on:click="self.method()">` event binding
- [ ] `{{ self.field }}` data interpolation
- [ ] `<input bind="self.field" />` two-way data binding
- [ ] Nested SCSS selectors (`.parent { .child { } }`)
- [ ] `<for item in="self.list">` loop rendering
- [ ] `<if cond="self.flag">` conditional rendering
- [ ] Component system with props
- [ ] Hot reload in development

## Architecture

```
den/
├── den_macros/          # Proc macro crate
│   └── src/lib.rs       # HTML parser + SCSS parser + egui code generator
└── den_app/             # Application crate
    └── src/
        ├── main.rs      # Entry point + eframe setup
        └── pages/       # Component-based page structure
            └── home/
                ├── home.rs
                ├── home.html
                └── home.scss
```

The separation is intentional:
- **`den_macros`** is a `proc-macro` crate — Rust requires proc macros to live in their own crate
- **`den_app`** depends on `den_macros` and contains your application code and templates

## Contributing

Den is open source and contributions are welcome. Whether it's a bug fix, new CSS property support, a new HTML tag mapping, or an idea for the roadmap — open an issue or submit a PR.

## License

MIT
