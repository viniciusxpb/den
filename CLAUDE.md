# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Den

Den is a Rust framework that compiles HTML + SCSS templates into native egui desktop GUI code at compile time via procedural macros. There is zero runtime template parsing — all processing happens during compilation.

## Build & Development Commands

```bash
cargo build                    # Build everything
cargo run -p den_app           # Run the demo application
cargo build -p den_macros      # Build only the proc macro crate
cargo clippy                   # Lint
make dev                       # Hot reload dev mode (watches den_app/src and den_macros/src)
```

There are no tests currently.

## Architecture

**Workspace structure**: Two crates in a Cargo workspace (resolver v3, edition 2024).

- **`den_macros`** — Proc macro crate containing the entire compile-time pipeline in a single file (`src/lib.rs`). Exports `den_template!("path", self)`.
- **`den_app`** — Example application using eframe/egui. Contains pages with `.html` + `.scss` template pairs.

**Compile-time pipeline** (all in `den_macros/src/lib.rs`):
1. `den_template!("pages/home/home", self)` reads `.html` and `.scss` files relative to `den_app/src/`
2. HTML parser → `HtmlElement` tree (hand-rolled, UTF-8 safe via `Vec<char>`)
3. SCSS parser → `HashMap<ClassName, StyleRule>` (ASCII byte-level parsing)
4. Styles merge onto elements by class name (last-wins for overlapping properties)
5. Code generator emits egui Rust code via `quote!`

**Template conventions**:
- `{{ this.field }}` in HTML interpolates component state; `this` maps to `self` in generated code
- SCSS properties map to egui APIs: `color` → `RichText::color()`, `font-size` → `RichText::size()`, `background` → `Frame::fill()`, `padding` → `Frame::inner_margin()`, `display: flex` → `ui.horizontal()`, `border` → `Frame::stroke()`, `border-radius` → `Frame::corner_radius()`, `width` → `ui.set_width()`
- Supported HTML tags: `div`, `span`, `p`, `heading`/`h1`-`h3`

**Page pattern**: Each page is a struct with a `render(&self, ui: &mut egui::Ui)` method that calls `den_template!`. Pages live in `den_app/src/pages/<name>/` with `<name>.rs`, `<name>.html`, and `<name>.scss`.
