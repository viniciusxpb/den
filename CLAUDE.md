# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Den

Den is a Rust framework that compiles HTML + SCSS templates into native egui desktop GUI code at compile time via procedural macros. Zero runtime template parsing — all processing happens during compilation.

## Build & Development Commands

```bash
cargo build                    # Build everything
cargo run --bin den_app        # Run the demo application (F2 toggles Node Editor)
cargo test                     # Run all tests (33 total: 13 layout + 20 parse)
cargo clippy                   # Lint
make dev                       # Hot reload dev mode (requires cargo-watch)
make preview                   # Generate HTML preview of dev-tagged components
make help                      # List all makefile commands
```

`make dev` watches `den_app/src` and `den_macros/src`, excludes `den_macros/src/lib.rs` from triggers (to prevent rebuild loops), touches `lib.rs` to force recompilation, then runs the app.

## Architecture

**Workspace structure**: Three crates in a Cargo workspace (resolver v3, edition 2024, Rust 1.88+).

- **`den_macros`** — Proc macro crate. 3-phase compile-time pipeline in modular files. Exports `den_template!`.
- **`den_layout`** — Runtime library. `LayoutTable` (iterative width resolution), `DenElementStyle` (visual properties exposed to click handlers). Pure data, no GUI dependency.
- **`den_app`** — Example application using eframe/egui. Contains pages with `.html` + `.scss` template pairs, a node editor, and dev tool binaries (`preview`, `style_editor`).

### Compile-time pipeline (3 phases across `den_macros/src/`)

```
  Phase 1: Parse           Phase 2: Resolve          Phase 3: Codegen
  parse/html.rs            resolve.rs                codegen/element.rs
  parse/scss.rs            RawNode + StyleMap         codegen/control_flow.rs
  parse/text.rs            → DenNode tree             codegen/frame.rs
  parse/color.rs           (with DenVisual)           codegen/text.rs
  → RawNode + StyleMap                                codegen/click.rs
                                                      codegen/flex.rs
                                                      codegen/input.rs
                                                      codegen/mod.rs
                                                      → TokenStream
```

1. `den_template!("pages/home/home", self)` reads `.html` and `.scss` files relative to `den_app/src/`
2. `parse/html.rs` → `Vec<RawNode>` tree (hand-rolled, UTF-8 safe via `Vec<char>`)
3. `parse/scss.rs` → `StyleMap` (`HashMap<ClassName, StyleRule>`, supports `$variables`)
4. `resolve.rs` → merges styles, produces `Vec<DenNode>` with `DenVisual`. Also parses `on_click` into func_name + args via `parse_click_call()`.
5. `codegen/` → emits egui Rust code via `quote!`, includes layout table initialization and click arg cloning

**Key source files**:
- `lib.rs` — Entry point (~60 lines), wires the 3 phases
- `input.rs` — `DenTemplateInput` syn parsing
- `types/` — Shared types split into submodules:
  - `raw.rs` — `RawNode`, `RawElement` (Phase 1 output)
  - `resolved.rs` — `DenNode`, `DenElement`, `DenVisual` (Phase 2 output)
  - `style.rs` — `StyleRule`, `TextSegment`
  - `walk.rs` — `walk_den_nodes()` (canonical DFS traversal)

All errors become `compile_error!` — users see IDE errors immediately.

### Template syntax

**Macro invocation**:
- `den_template!("pages/home/home")` — without self, no interpolation or events
- `den_template!("pages/home/home", self)` — with self, enables `{{ self.field }}` interpolation and `(click)` events

**Interpolation**: `{{ self.field }}` — generates `self.field` directly (no `this.` translation). Fields must implement `Display`.

**Event binding**:
- `(click)="handler()"` — no args, generates `self.handler()`
- `(click)="handler(expr1, expr2)"` — with args, **requires `den-bind`** on the element. Args are auto-cloned before the render scope.
- `den-bind="var"` attribute — declares which loop variable this element is bound to. Required when `(click)` has arguments.
- Special keywords in args: `idx` → `__den_idx_N` (loop index), `style` → `DenElementStyle` struct from SCSS

**SCSS variables**: `$var: value;` at top of file, referenced as `color: $var;` in properties.

**Style inheritance**: only `color` and `font-size` inherit from parent to child (not hover, not layout).

**Control flow**:
- `<for each="item" in="self.items">...children...</for>` — generates `for (idx, item) in self.items.iter().enumerate()`. Transparent for layout (children belong to grandparent in layout table).
- `<if cond="self.flag">...then...</if>` with optional `<else>...else...</else>`.

### Layout system (`den_layout` crate, runtime)

- `LayoutTable` with flat list of `LayoutEntry` (index 0 = invisible body/root)
- `WidthRule`: `Auto` | `Px(f32)` | `Percent(f32)`
- `resolve(available_width)`: iterative passes. Px → immediate, Percent → from parent, Auto → fill parent (leaf) or embrace children (container)
- `distribute_flex()`: post-pass, divides remaining space among Auto children of flex containers. Px and Percent children treated as fixed.
- Generated code uses `thread_local! { RefCell<LayoutTable> }` — initialized once, reused every frame
- **Width at render**: `Px` uses `__den_layout.sizes[i] * __den_scale`. `Percent` uses `ui.available_width() * pct` inline (layout doesn't know about padding). `Auto` doesn't set width.

### Scale system

- `__den_scale: f32` parameter on `render()`, multiplies all pixel values
- Scales: `font-size` (min 6.0), `padding`, `border-width` (min 1.0), `border-radius`, `width: Npx`
- Does NOT scale: `color`, `background`, `width: N%`, `display`, `cursor`
- Controls: Ctrl+=/Ctrl+-/Ctrl+0/Ctrl+scroll, +/-/% widget at bottom-right
- Per-view zoom: Home and NodeEditor have independent scale values

### Flex distribution (`codegen/flex.rs` + `codegen/element.rs`)

- `parent_is_flex` in `CodegenCtx` tracks flex parent context
- `collect_flex_children_info` uses `walk_den_nodes` to pre-collect child width rules
- `build_flex_layout` generates runtime `__den_flex_share` (remaining space ÷ Auto children)
- Auto flex children wrapped in `allocate_ui_with_layout(top_down)` for text wrapping
- **Limitation**: `IfChain` inside flex contributes both branches at compile time

### DFS traversal invariant

`walk_den_nodes()` in `types/walk.rs` is the **single source of truth** for DFS order. All functions that assign layout indices or iterate elements MUST use it. Currently used by:
- `collect_flat_entries` (codegen/mod.rs)
- `collect_flex_children_info` (codegen/element.rs)
- `generate_element` increments `ctx.layout_index` in the same DFS order

### Click handler codegen (`codegen/click.rs`)

When `(click)` has arguments:
1. Validate `den-bind` exists → `compile_error!` if missing
2. Generate `let __den_click_arg_N = (expr).clone();` before UI borrow (`style` args skip clone — already owned)
3. If `style` keyword in args: generate `let __den_element_style = DenElementStyle { ... }` from `DenVisual`
4. Click handler: `self.handler(__den_click_arg_0, __den_click_arg_1, ...)`
5. `translate_click_arg()`: `idx` → `__den_idx_N`, `style` → `__den_element_style`, else passthrough

**Known limitation**: `<for>` + `(click)` with `&mut self` has borrow conflict (iterating `self.items` while calling `self.handler()`). Pre-existing issue. See PENDING.md.

### SCSS → egui property mapping

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

**Page pattern**: Each page is a struct with `render(&mut self, ui: &mut egui::Ui, __den_scale: f32)` that calls `den_template!`. The `__den_scale` parameter name is required — generated code references it directly.

### Node Editor (`den_app/src/node_editor/`)

Visual node graph editor using `egui::Painter` directly (zero widgets):
- `theme.rs` — 50+ named constants, zero magic numbers
- `types.rs` — `NodeData`, `PortData`, `WireData`, `FieldData`, `PortType`, `DragState`, `WireDragState`
- `node.rs` — `draw_node` (shadow, header, accent, ports with triangle/diamond/circle shapes, fields with dashed separator)
- `wire.rs` — `draw_wire` with cubic bezier, `get_port_position` returning `Option<Pos2>`
- `canvas.rs` — `NodeEditorCanvas` with drag & drop (node drag + wire drag from output ports), hit testing, z-order
- `demo.rs` — 4 NDNM nodes (Hermes/Atlas/Argus/Athena) + 6 wires fixture
- F2 toggles between Home view and Node Editor

### Dev tools (binaries in `den_app/src/bin/`)

- `preview.rs` — Generates `preview/index.html` with all `dev`-tagged elements as static HTML. Has its own HTML/SCSS parsers (**duplicated** from `den_macros` — see PENDING.md for `den_core` extraction plan).
- `style_editor.rs` — Separate egui window with visual controls per SCSS class. Writes back with surgical byte-offset replacement and 300ms debounce. Resolves `$variables` to literals on write-back (see PENDING.md).

### Known duplications (PENDING.md)

- `collect_scss_vars` exists in 3 places: `parse/scss.rs`, `preview.rs`, `style_editor.rs`
- `StyleRule::merge_from` and `DenVisual::merge_from` have identical logic — update both when adding CSS properties
- HTML parser helpers duplicated between `den_macros` and `preview.rs`
- Fix: extract `den_core` crate with shared parsers and types
