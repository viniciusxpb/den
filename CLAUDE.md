# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Den

Den is a Rust framework that compiles HTML + SCSS templates into native egui desktop GUI code at compile time via procedural macros. Zero runtime template parsing — all processing happens during compilation.

## Build & Development Commands

```bash
cargo build                    # Build everything
cargo run --bin den_app        # Run the demo application
cargo test                     # Run all tests (27 total: 13 layout + 14 parse)
cargo clippy                   # Lint
make dev                       # Hot reload dev mode (requires cargo-watch)
make preview                   # Generate HTML preview of dev-tagged components
make help                      # List all makefile commands
```

`make dev` watches `den_app/src` and `den_macros/src`, excludes `den_macros/src/lib.rs` from triggers (to prevent rebuild loops), touches `lib.rs` to force recompilation, then runs the app.

## Architecture

**Workspace structure**: Three crates in a Cargo workspace (resolver v3, edition 2024, Rust 1.88+).

- **`den_macros`** — Proc macro crate. 3-phase compile-time pipeline in modular files. Exports `den_template!`.
- **`den_layout`** — Runtime layout system. `LayoutTable` with iterative width resolution, flat list, max 5 passes to fixpoint. Pure data, no GUI dependency.
- **`den_app`** — Example application using eframe/egui. Contains pages with `.html` + `.scss` template pairs, plus dev tool binaries (`preview`, `style_editor`).

**Compile-time pipeline** (3 phases across `den_macros/src/`):

```
  Phase 1: Parse           Phase 2: Resolve          Phase 3: Codegen
  parse/html.rs            resolve.rs                codegen/element.rs
  parse/scss.rs            RawNode + StyleMap         codegen/control_flow.rs
  parse/text.rs            → DenNode tree             codegen/frame.rs
  parse/color.rs           (with DenVisual)           codegen/text.rs
  → RawNode + StyleMap                                codegen/mod.rs
                                                      → TokenStream
```

1. `den_template!("pages/home/home", self)` reads `.html` and `.scss` files relative to `den_app/src/`
2. `parse/html.rs` → `Vec<RawNode>` tree (hand-rolled, UTF-8 safe via `Vec<char>`)
3. `parse/scss.rs` → `StyleMap` (`HashMap<ClassName, StyleRule>`, supports `$variables`)
4. `resolve.rs` → merges styles onto elements by class name, produces `Vec<DenNode>` with `DenVisual` attached
5. `codegen/` → emits egui Rust code via `quote!`, includes layout table initialization

**Key source files**:
- `lib.rs` — Entry point (~60 lines), wires the 3 phases
- `input.rs` — `DenTemplateInput` syn parsing
- `types.rs` — All shared types: `RawNode`, `RawElement`, `DenNode`, `DenElement`, `DenVisual`, `StyleRule`, `TextSegment`

All errors become `compile_error!` — users see IDE errors immediately.

**Macro invocation**:
- `den_template!("pages/home/home")` — without self, no interpolation or events
- `den_template!("pages/home/home", self)` — with self, enables `{{ this.field }}` interpolation and `(click)` events

**Template conventions**:
- `{{ this.field }}` in HTML interpolates component state; `this` maps to `self` in generated code. Fields must implement `Display`. Using interpolation without `, self` in the macro call is a compile error.
- `(click)="method_name()"` on any element binds a click event that calls `self.method_name()`. Requires `, self` in macro call.
- SCSS `:hover` pseudo-selector is supported; uses egui's temp data store with deterministic element IDs for per-frame hover tracking.
- SCSS variables: `$var: value;` at top of file, referenced as `color: $var;` in properties.
- Style inheritance: only `color` and `font-size` inherit from parent to child elements (not hover, not layout properties).

**Control flow**:
- `<for each="item" in="this.items">...children...</for>` — iterates over a collection. Generates `for (idx, item) in self.items.iter().enumerate()`. Hover/click elements inside loops get unique IDs per iteration via runtime index salting.
- `<if cond="this.flag">...then...</if>` — conditional rendering. Generates `if self.flag { ... }`.
- `<if>` can be followed by `<else>...children...</else>` for if/else branching.
- Both `<for>` and `<if>` are transparent for styling and layout — they pass inherited styles through to children unchanged. In the layout flat list, their children belong to the grandparent.

**Layout system** (`den_layout` crate, runtime):
- `LayoutTable` with flat list of `LayoutEntry` (index 0 = invisible body/root)
- `WidthRule`: `Auto` | `Px(f32)` | `Percent(f32)`
- `resolve(available_width)`: iterative passes resolving Px → Percent → Auto (content-based or fill-parent)
- `distribute_flex()`: post-pass that divides remaining space among Auto children of flex containers
- Generated code uses `thread_local! { RefCell<LayoutTable> }` — initialized once, reused every frame
- `Px` widths use `__den_layout.sizes[i]` at render. `Percent` uses `ui.available_width() * pct` inline (because layout system doesn't know about Frame padding). `Auto` doesn't set width (egui decides).

**Scale system**:
- `__den_scale: f32` parameter on `render()`, multiplies all pixel values
- Scales: `font-size`, `padding`, `border-width` (min 1.0), `border-radius`, `width: Npx`
- Does NOT scale: `color`, `background`, `width: N%`, `display`, `cursor`
- Controls: Ctrl+=/Ctrl+-/Ctrl+0/Ctrl+scroll, +/-/% widget at bottom-right

**Flex distribution** (`codegen/element.rs`):
- `parent_is_flex` in `CodegenCtx` tracks if parent has `display: flex`
- `collect_flex_children_info` pre-collects child width rules (mirrors DFS order of `collect_flat_entries`)
- `build_flex_layout` generates runtime `__den_flex_share` calculation
- Auto flex children wrapped in `allocate_ui_with_layout(top_down)` for text wrapping
- **INVARIANT**: `collect_flat_entries`, `collect_flex_children_info`, `skip_descendants` MUST walk in the same DFS order. Update all three when adding new `DenNode` variants.
- **Limitation**: `IfChain` inside flex contributes both branches to child count at compile time, but only one runs at runtime. `__den_flex_share` may be narrower than needed.

**SCSS → egui property mapping**:

| SCSS Property    | egui API                    | Width System          | Values                           |
|------------------|-----------------------------|----------------------|----------------------------------|
| `color`          | `RichText::color()`         | —                    | `#RRGGBB`, `#RGB`, `$variable`  |
| `font-size`      | `RichText::size()`          | scaled               | `24` or `24px`                   |
| `background`     | `Frame::fill()`             | —                    | `#RRGGBB`, `#RGB`, `$variable`  |
| `padding`        | `Frame::inner_margin()`     | scaled               | `16` or `16px`                   |
| `display: flex`  | `ui.horizontal()` + flex dist | layout system      | only `flex` value supported      |
| `border`         | `Frame::stroke()`           | width scaled (min 1) | `1px solid #RRGGBB`             |
| `border-radius`  | `Frame::corner_radius()`    | scaled               | `8` or `8px`                     |
| `width`          | layout system / inline      | Px: layout, %: inline | `100%`, `50%`, `200px`, `auto` |
| `cursor: pointer`| `ctx.set_cursor_icon()`     | —                    | only in `:hover` blocks          |

Supported HTML tags: `div`, `span`, `p`, `heading`/`h1`-`h3`. All map to `ui.label()` or `ui.heading()`.

**Page pattern**: Each page is a struct with a `render(&mut self, ui: &mut egui::Ui, __den_scale: f32)` method that calls `den_template!`. The `__den_scale` parameter name is required — the generated code references it directly. Pages live in `den_app/src/pages/<name>/` with `mod.rs`, `<name>.rs`, `<name>.html`, and `<name>.scss`.

**Dev tools** (binaries in `den_app/src/bin/`):
- `preview.rs` — Generates `preview/index.html` with all `dev`-tagged elements as static HTML with real CSS. Has its own HTML/SCSS parsers (duplicated from `den_macros`; see PENDING.md for `den_core` extraction plan).
- `style_editor.rs` — Separate egui window with visual controls per SCSS class. Writes back to `.scss` files with surgical byte-offset replacement and 300ms debounce. Note: resolves `$variables` to literals on write-back (see PENDING.md).
