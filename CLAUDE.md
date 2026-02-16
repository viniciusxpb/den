# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Den

Den is a Rust framework that compiles HTML + SCSS templates into native egui desktop GUI code at compile time via procedural macros. Zero runtime template parsing — all processing happens during compilation.

## Build & Development Commands

```bash
cargo build                    # Build everything
cargo run -p den_app           # Run the demo application
cargo build -p den_macros      # Build only the proc macro crate
cargo clippy                   # Lint
make dev                       # Hot reload dev mode (requires cargo-watch)
```

`make dev` watches `den_app/src` and `den_macros/src`, excludes `den_macros/src/lib.rs` from triggers (to prevent rebuild loops), touches `lib.rs` to force recompilation, then runs the app.

There are no tests currently.

## Architecture

**Workspace structure**: Two crates in a Cargo workspace (resolver v3, edition 2024, Rust 1.88+).

- **`den_macros`** — Proc macro crate. Entire compile-time pipeline lives in a single file (`src/lib.rs`, ~2000 lines). Exports `den_template!`.
- **`den_app`** — Example application using eframe/egui. Contains pages with `.html` + `.scss` template pairs.

**Compile-time pipeline** (all in `den_macros/src/lib.rs`):
1. `den_template!("pages/home/home", self)` reads `.html` and `.scss` files relative to `den_app/src/`
2. HTML parser → `HtmlElement` tree (hand-rolled, UTF-8 safe via `Vec<char>`)
3. SCSS parser → `HashMap<ClassName, StyleRule>` (ASCII byte-level parsing)
4. Styles merge onto elements by class name (last-wins for overlapping properties)
5. Code generator emits egui Rust code via `quote!`

All errors become `compile_error!` — users see IDE errors immediately.

**Macro invocation**:
- `den_template!("pages/home/home")` — without self, no interpolation or events
- `den_template!("pages/home/home", self)` — with self, enables `{{ this.field }}` interpolation and `(click)` events

**Template conventions**:
- `{{ this.field }}` in HTML interpolates component state; `this` maps to `self` in generated code. Fields must implement `Display`. Using interpolation without `, self` in the macro call is a compile error.
- `(click)="method_name()"` on any element binds a click event that calls `self.method_name()`. Requires `, self` in macro call.
- SCSS `:hover` pseudo-selector is supported; uses egui's temp data store with deterministic element IDs for per-frame hover tracking.
- Style inheritance: only `color` and `font-size` inherit from parent to child elements (not hover, not layout properties).

**Control flow**:
- `<for each="item" in="this.items">...children...</for>` — iterates over a collection. Generates `for (idx, item) in self.items.iter().enumerate()`. The loop variable (`item`) is available in `{{ item }}` or `{{ item.field }}` inside the body. Hover/click elements inside loops get unique IDs per iteration via runtime index salting.
- `<if cond="this.flag">...then...</if>` — conditional rendering. Generates `if self.flag { ... }`.
- `<if>` can be followed by `<else>...children...</else>` for if/else branching.
- Both `<for>` and `<if>` are transparent for styling — they pass inherited styles through to children unchanged.

**SCSS → egui property mapping**:

| SCSS Property    | egui API                    | Values                           |
|------------------|-----------------------------|----------------------------------|
| `color`          | `RichText::color()`         | `#RRGGBB` or `#RGB`             |
| `font-size`      | `RichText::size()`          | `24` or `24px`                   |
| `background`     | `Frame::fill()`             | `#RRGGBB` or `#RGB`             |
| `padding`        | `Frame::inner_margin()`     | `16` or `16px`                   |
| `display: flex`  | `ui.horizontal()`           | only `flex` value supported      |
| `border`         | `Frame::stroke()`           | `1px solid #RRGGBB`             |
| `border-radius`  | `Frame::corner_radius()`    | `8` or `8px`                     |
| `width`          | `ui.set_width()`            | `100%`, `50%`, `200px`, `auto`   |
| `cursor: pointer`| `ctx.set_cursor_icon()`     | only in `:hover` blocks          |

Supported HTML tags: `div`, `span`, `p`, `heading`/`h1`-`h3`. All map to `ui.label()` or `ui.heading()`.

**Page pattern**: Each page is a struct with a `render(&mut self, ui: &mut egui::Ui)` method that calls `den_template!`. Pages live in `den_app/src/pages/<name>/` with `mod.rs`, `<name>.rs`, `<name>.html`, and `<name>.scss`. The `mod.rs` uses `#[allow(clippy::module_inception)]`.
