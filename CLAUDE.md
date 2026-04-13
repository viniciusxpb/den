# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Den

Den is a Rust framework that compiles HTML + SCSS templates into native egui desktop GUI code at compile time via procedural macros. Zero runtime template parsing — all processing happens during compilation.

## Build & Development Commands

```bash
cargo build                    # Build everything
cargo run --bin den_app        # Run the demo application
cargo test                     # Run all tests (39 total: 17 layout + 22 parse)
cargo clippy                   # Lint
make dev                       # Hot reload dev mode (requires cargo-watch)
make preview                   # Generate HTML preview of dev-tagged components
make help                      # List all makefile commands
```

`make dev` watches `den_app/src` and `den_macros/src`, excludes `den_macros/src/lib.rs` from triggers (to prevent rebuild loops), touches `lib.rs` to force recompilation, then runs the app.

## Architecture

**Workspace structure**: Three crates in a Cargo workspace (resolver v3, edition 2024, Rust 1.88+).

- **`den_macros`** — Proc macro crate. 3-phase compile-time pipeline in modular files. Exports `den_template!`.
- **`den_layout`** — Runtime library. Backend-agnostic layout primitives split by concern (`dimension`, `spacing`, `display`, `flex`, `entry`, `table`), `DenRouteState`, `DenElementStyle`, and typed router traits.
- **`den_app`** — Example application using eframe/egui. Contains pages with `.html` + `.scss` template pairs, a static Nodes page, and dev tool binaries (`preview`, `style_editor`).

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
- `DimensionRule`: `Auto` | `Px(f32)` | `Percent(f32)`
- `resolve_in_viewport(width, height)`: recalculates full rects in CSS pixels every frame.
- Block layout stacks children vertically using parent content width, padding, margin, gap, and explicit height rules. Margins are currently non-collapsing.
- Flex layout places children horizontally; `flex: 1` / `flex-grow: 1` Auto children split the remaining width after fixed widths, margins, and gaps are reserved.
- `distribute_flex()` is now a compatibility no-op because flex is resolved inside `layout_children()`.
- Generated code uses `thread_local! { RefCell<LayoutTable> }` — initialized once, reused every frame
- **Width at render**: `Px` and `Percent` use `__den_layout.sizes[i] * __den_scale`, already resolved from the parent content box. `Auto` fills block context unless it is a content-sized flex child.
- `den_layout` no longer depends on `egui`; `DenPage<Route, Ui>` is generic over the UI backend type.
- Generated route/page glue references `crate::DenUi`; the demo app aliases it to `egui::Ui`.

### Route state (`den_layout::DenRouteState`)

- Each generated `AppPages` host stores one `DenRouteState` per declared route.
- Route state is reset when navigation flushes into that route.
- Page render methods receive `__den_route_state: &mut DenRouteState` after `__den_router`.
- `DenRouteState` currently groups `DenInputState` and `DenDebugState`; it is the runtime hook for the planned generic renderer/tree pipeline.
- `DEN_DEBUG_ROUTE_STATE=1` emits a one-time state dump per route render path.

### Scale system

- `__den_scale: f32` parameter on `render()`, multiplies all pixel values
- Scales: `font-size` (min 6.0), `padding`, `margin`, `border-width` (min 1.0), `border-radius`, `width: Npx`
- Does NOT scale: `color`, `background`, `width: N%`, `display`, `cursor`
- Controls: Ctrl+=/Ctrl+-/Ctrl+0/Ctrl+scroll, +/-/% widget at bottom-right
- Zoom is currently global for the demo app.

### Flex distribution (`codegen/flex.rs` + `codegen/element.rs`)

- `parent_is_flex` in `CodegenCtx` tracks flex parent context
- `build_flex_layout` maps flex containers to `ui.horizontal()` and applies SCSS `gap`
- Runtime width distribution lives in `den_layout::LayoutTable::layout_flex_children`
- `flex: 1` Auto children are wrapped in `allocate_ui_with_layout(top_down)` for text wrapping
- **Limitation**: `IfChain` inside flex contributes both branches at compile time

### DFS traversal invariant

`walk_den_nodes()` in `types/walk.rs` is the **single source of truth** for DFS order. All functions that assign layout indices or iterate elements MUST use it. Currently used by:
- `collect_flat_entries` (codegen/mod.rs)
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
| `margin`         | layout + `Frame::outer_margin()` | scaled          | `16` or `16px`                   |
| `display: flex`  | `ui.horizontal()` + flex dist | layout system      | only `flex` value supported      |
| `border`         | `Frame::stroke()`           | width scaled (min 1) | `1px solid #RRGGBB`             |
| `border-radius`  | `Frame::corner_radius()`    | scaled               | `8` or `8px`                     |
| `width`          | layout system               | Px/%/Auto resolved runtime | `100%`, `50%`, `200px`, `auto` |
| `height`         | layout system               | Px/%/Auto resolved runtime | `100%`, `200px`, `auto` |
| `gap`            | layout + egui spacing       | scaled               | `8` or `8px`                     |
| `cursor: pointer`| `ctx.set_cursor_icon()`     | —                    | only in `:hover` blocks          |

Supported HTML tags: `div`, `span`, `p`, `heading`/`h1`-`h3`. All map to `ui.label()` or `ui.heading()`.

**Page pattern**: Each page is a struct with `render(&mut self, ui: &mut egui::Ui, __den_scale: f32, __den_router: &mut DenRouter<AppRoute>, __den_route_state: &mut DenRouteState)` that calls `den_template!`. The `__den_scale`, `__den_router`, and `__den_route_state` names are framework-reserved by convention.

### Nodes page (`den_app/src/pages/nodes/`)

The previous direct `egui::Painter` node editor was removed. The current Nodes view is a Den template pair (`nodes.html` + `nodes.scss`) that renders static Hermes/Atlas/Argus/Athena cards through the normal HTML + SCSS pipeline. New visual work in `den_app` should continue through Den templates, not handwritten painter calls.

### Dev tools (binaries in `den_app/src/bin/`)

- `preview.rs` — Generates `preview/index.html` with all `dev`-tagged elements as static HTML. Has its own HTML/SCSS parsers (**duplicated** from `den_macros` — see PENDING.md for `den_core` extraction plan).
- `style_editor.rs` — Separate egui window with visual controls per SCSS class. Writes back with surgical byte-offset replacement and 300ms debounce. Resolves `$variables` to literals on write-back (see PENDING.md).

### Known duplications (PENDING.md)

- `collect_scss_vars` exists in 3 places: `parse/scss.rs`, `preview.rs`, `style_editor.rs`
- `StyleRule::merge_from` and `DenVisual::merge_from` have identical logic — update both when adding CSS properties
- HTML parser helpers duplicated between `den_macros` and `preview.rs`
- Fix: extract `den_core` crate with shared parsers and types
