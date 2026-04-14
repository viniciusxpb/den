<div align="center">

# Den

**Write HTML + SCSS. Compile to native desktop UI. No Chrome. No runtime surprises.**

Every Rust GUI framework makes you write UI in Rust. Den doesn't.

You write templates in `.html`, styles in `.scss`, and logic in `.rs` — each in its own file, like a proper frontend framework. At compile time, Den's proc macro reads your templates and turns them into native [egui](https://github.com/emilig/egui) code. The result: a desktop app that runs in ~100MB of RAM instead of Tauri's 300MB+ Chromium shell, inspired by Angular but without the billion-plugin setup.

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.88%2B-orange.svg)](https://www.rust-lang.org)
[![egui](https://img.shields.io/badge/egui-0.33-purple.svg)](https://github.com/emilig/egui)

*Have you ever coded in Rust, and had to download a billion plugins, where the Den SCSS? Well... Here it is.*

</div>

---

## Why Den

**1. Silent front-end errors are architecturally impossible.**
Every `.html` template is compiled by the same macro that feeds rustc. If you type `bind="self.naem"` and `naem` doesn't exist on your struct — **the project does not compile**. If you call `(click)="handlerr()"` and no such method exists — **it does not compile**. If you `goto="PgaeInexistente"` — **it does not compile**. No runtime `undefined`, no "why isn't this styled?", no "this route doesn't exist but the build passed." Template errors are compiler errors, every time.

**2. Native binaries, no WebView.**
Tauri still bundles Chromium/WebKit (~300MB RAM minimum on cold start). Electron is worse. Flutter is ~150MB with its own runtime. Den compiles to a single native binary that runs in ~100MB because there's no browser involved — just egui's painter drawing your resolved layout tree.

**3. HTML + SCSS, not yet another DSL.**
Dioxus asks you to learn RSX. Slint has its own language. egui asks you to write UI inline in Rust. Den asks you to write the HTML and SCSS you already know. The framework translates, not you.

```
                      compile time
  .html + .scss  ────────────────►  native Rust + egui  ──►  desktop app
                    parse → resolve → codegen             (Linux / macOS / Windows)
```

The target audience is the frontend developer who wants a native app without learning a new UI language, and who got tired of paying Chromium's RAM tax to ship a dashboard.

---

## Quick Start

### Prerequisites

- Rust 1.88+ (`rustup update stable`)
- Linux: `sudo apt-get install libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev libxkbcommon-dev libssl-dev`
- `cargo install cargo-watch` (for hot reload)

### Project layout

```
my-app/
├── Cargo.toml
├── den_macros/              # proc macro crate (the compiler)
├── den_layout/              # runtime layout system (backend-agnostic rect engine)
└── den_app/                 # your application
    └── src/
        ├── main.rs
        └── pages/
            └── home/
                ├── home.rs      # logic
                ├── home.html    # template
                └── home.scss    # styles
```

### Write a page

**home.html**
```html
<div class="greeting">Hello {{ self.name }}!</div>
<div class="btn" (click)="greet()">Greet</div>
```

**home.scss**
```scss
$accent: #e94560;

.greeting {
    color: $accent;
    font-size: 24;
}

.btn {
    background: #16213e;
    padding: 12;
    border-radius: 6;
    cursor: pointer;
}

.btn:hover {
    background: $accent;
}
```

**home.rs**
```rust
use crate::AppRoute;
use den_layout::{DenRouteState, DenRouter};
use eframe::egui;

pub struct HomePage {
    pub name: String,
}

impl HomePage {
    pub fn render(
        &mut self,
        ui: &mut egui::Ui,
        __den_scale: f32,
        __den_router: &mut DenRouter<AppRoute>,
        __den_route_state: &mut DenRouteState,
    ) {
        den_macros::den_template!("pages/home/home", self);
    }

    fn greet(&mut self) {
        self.name = "world".to_string();
    }
}
```

At the crate root, define the backend UI alias used by the route/page macros:

```rust
use eframe::egui;

pub type DenUi = egui::Ui;
```

That's it. `cargo run --bin den_app` and you get a native desktop window with styled text, hover, and a working event handler. No egui boilerplate. No `RichText::new().color().size()` chains. Just HTML and SCSS.

---

## Compile-time Safety

This is the core of Den. Because the macro reads your `.html` and generates Rust that rustc then validates, the following **fail the build**, not the runtime:

| You write | What happens | Why |
|-----------|--------------|-----|
| `{{ self.naem }}` (typo) | Compile error: no field `naem` on `HomePage` | Macro emits `self.naem`, rustc rejects |
| `bind="self.missing"` | Compile error | Generated `self.missing = ...` doesn't compile |
| `(click)="typo()"` | Compile error: method not found | Generated `self.typo()` fails resolution |
| `with="self.user"` but target page expects `Account` | Compile error: type mismatch | Rust's type system enforces route params |
| `goto="PageThatDoesNotExist"` | Compile error: unknown route | Router macro validates target at build |

Everything above works today. On the roadmap (see [PENDING.md](PENDING.md)):

- `class="typo-name"` where the class doesn't exist in any `.scss` → compile error (planned)
- Dead CSS detection: `.foo` declared but never used in any `.html` → warning (planned)
- Error spans mapped back to `.html` line:column so your IDE underlines inside the HTML file, not inside the macro expansion (planned)

No other HTML-based UI framework — Angular included — catches all of this at build time. Angular ships `strictTemplates` and the Language Service as a separate LSP tool; Den gets it for free because the template compiler *is* the Rust compiler.

---

## Features

### Data Interpolation

Use `{{ self.field }}` to bind component state. Fields must implement `Display`.

```html
<div class="greeting">Hello {{ self.name }}, you are {{ self.age }} years old!</div>
```

### Two-way Input Binding

`bind="self.field"` on `<input>` is Den's equivalent of Angular's `[(ngModel)]`. Nested paths work:

```html
<input bind="self.usuario.nome" placeholder="Your name..." class="text-input" />
```

### Event Binding

```html
<div class="button" (click)="on_button_click()">Click me</div>
```

Click handlers without arguments are fully supported. Handlers with arguments inside `<for>` are reserved (see [PENDING.md](PENDING.md)).

### Navigation with Typed Data

```html
<div class="btn" goto="UsuarioPage" with="self.usuario">Open user</div>
```

`with="..."` passes a cloned value to the target page — equivalent to Angular's `@Input()`, but enforced by Rust's type system at compile time.

### Conditional Rendering

```html
<if cond="self.logged_in">
    <div class="welcome">Welcome back!</div>
</if>
<else>
    <div class="login-prompt">Please log in.</div>
</else>
```

### Loop Rendering

```html
<for each="item" in="self.items">
    <div class="item">{{ item }}</div>
</for>
```

The loop index salts `node_id` hashes so hover/focus state is stable per item across frames.

### Hover States

SCSS `:hover` works out of the box:

```scss
.button {
    background: #16213e;
    padding: 8;
}

.button:hover {
    background: #e94560;
    cursor: pointer;
}
```

### SCSS Variables

```scss
$primary: #0f3460;
$accent:  #e94560;

.title {
    color: $primary;
    font-size: 28;
}
```

### Zoom / Scale

All pixel values (`font-size`, `line-height`, `letter-spacing`, `padding`, `margin`, `border-width`, `border-radius`, `width: Npx`) are multiplied by `__den_scale` at render time. Colors, percentages, and layout modes are unaffected.

Built-in controls:
- **Ctrl+scroll**, **Ctrl+=** / **Ctrl+-**: zoom in/out (10% steps)
- **Ctrl+0**: reset to 100%
- Range: 50% to 300%

### Layout System

`den_layout` is a runtime crate with zero egui dependency. It resolves rects from a `RenderTree` + `LayoutIntent` per node:

- `width: 100%` fills parent's available width
- `width: 200px` is a fixed dimension, scales with zoom
- `margin: 12` reserves non-collapsing outer spacing
- `display: flex` distributes children horizontally, `flex: 1` splits remaining space
- Block layout stacks children vertically with `gap` between them

The layout table rebuilds every frame, so resizing the window reflows naturally. Because `den_layout` has no egui dependency, the whole engine is backend-agnostic — swapping egui for iced, wgpu, or a web canvas is possible without touching layout or macro code.

Inspect what the engine resolves:

```bash
DEN_DEBUG_LAYOUT=1 cargo run --bin den_app
```

### Route State

Each route declared with `den_router!` owns a `DenRouteState` (inputs, focus, cursor, hover). The paint function reads/writes this state during each frame, and dispatch mirrors input changes back to both the route state and the page's struct field.

---

## Supported Properties

| SCSS Property    | Maps To                          | Example                       |
|------------------|----------------------------------|-------------------------------|
| `color`          | text color                       | `#e94560`, `$variable`        |
| `font-size`      | font size (scaled)               | `24` or `24px`                |
| `font-family`    | font stack                       | `"Inter", sans-serif`         |
| `font-weight`    | weight (normal/bold/100-900)     | `bold`, `700`                 |
| `font-style`     | italics flag                     | `normal`, `italic`            |
| `font`           | shorthand                        | `italic 600 16px/1.4 Inter`   |
| `line-height`    | line height                      | `20px`, `1.4`, `140%`         |
| `letter-spacing` | letter spacing                   | `0.5px`, `normal`             |
| `text-transform` | case transform                   | `uppercase`, `lowercase`      |
| `text-align`     | text position in rect            | `left`, `center`, `right`     |
| `text-decoration`| underline / line-through         | `underline`                   |
| `background`     | rect fill                        | `#1a1a2e`, `$variable`        |
| `padding`        | inner spacing (scaled)           | `16` or `16px`                |
| `margin`         | outer spacing (scaled)           | `16` or `16px`                |
| `display: flex`  | flex layout                      | `display: flex`               |
| `border`         | rect stroke (scaled)             | `1px solid #e94560`           |
| `border-radius`  | corner radius (scaled)           | `8` or `8px`                  |
| `width`          | dimension                        | `100%`, `50%`, `200px`, `auto`|
| `height`         | dimension                        | `100%`, `200px`, `auto`       |
| `gap`            | flex/block child spacing         | `8` or `8px`                  |
| `cursor: pointer`| pointing-hand cursor on hover    | in `:hover` blocks            |

Properties declared in `.scss` but not in this table are parsed and ignored today. Many are tracked in [PENDING.md](PENDING.md) (position, transform, box-shadow, rgba/alpha, gradients, grid, SVG, etc.). The project's stance is that unsupported properties should eventually fail the build rather than degrade silently — consistent with the compile-time safety north star.

---

## Supported HTML Tags

| Tag                         | Maps To                            |
|-----------------------------|------------------------------------|
| `<div>`, `<span>`, `<p>`    | `RenderKind::Text` or `Container`  |
| `<heading>`, `<h1>`–`<h3>`  | `RenderKind::Text { heading: true }` |
| `<input bind="...">`        | `RenderKind::Input` (two-way bound)|
| `<for each="..." in="...">` | compiled `for` loop                |
| `<if cond="...">` / `<else>`| compiled `if` / `else`             |

---

## How It Works

Three-phase compile-time pipeline:

```
  Phase 1: Parse             Phase 2: Resolve         Phase 3: Codegen
  ─────────────              ────────────────         ────────────────
  HTML → RawNode tree        RawNode + StyleMap       DenNode tree
  SCSS → StyleMap            → DenNode tree           → TokenStream
                             (with DenVisual)          → Rust code that
                                                         builds a RenderTree
                                                         every frame
```

At runtime, each frame does **build → resolve → paint**:

1. Macro-generated code pushes `RenderNode`s into a `RenderTree` (Rust control flow drives `<for>` and `<if>`)
2. `LayoutTable` resolves rects in a single DFS pass
3. `paint_tree` walks the tree, issues `painter.rect_filled` / text galley / `painter.rect_stroke`, collects events
4. Event dispatch matches `PaintEvent::Click` / `Goto` / `InputChanged` to methods on your page struct

All errors become `compile_error!` — you see them in your IDE immediately.

---

## Dev Tools

### Hot Reload

```bash
make dev
```

Watches `den_app/src` and `den_macros/src`, recompiles and reruns on every change.

### HTML Preview

```bash
make preview
```

Generates `preview/preview.html` with every page rendered as static HTML using the real CSS, scoped per page, with copied `@font-face` assets and auto-refresh every 3 seconds. Useful for checking layouts without waiting on Rust rebuilds.

### Style Editor

```bash
cargo run --bin style_editor
```

Separate egui window that parses all `.scss` files and exposes visual controls per class: color pickers, sliders for numeric properties, dropdowns for enums, border controls. Writes back to disk with 300ms debounce. `cargo-watch` picks up the change and recompiles.

---

## Architecture

Three crates in a Cargo workspace (resolver v3, edition 2024, Rust 1.88+):

```
den/
├── den_macros/              # Proc macro crate (compile-time pipeline)
│   └── src/
│       ├── lib.rs           # Entry point, wires the 3 phases
│       ├── input.rs         # Macro input parsing (syn)
│       ├── types/           # Shared types (RawNode, DenNode, DenVisual, StyleRule)
│       ├── parse/           # Phase 1: HTML + SCSS parsers
│       ├── resolve.rs       # Phase 2: style resolution
│       ├── codegen/         # Phase 3: TokenStream emission
│       ├── router.rs        # den_router! macro
│       └── page.rs          # #[den_page] attribute
├── den_layout/              # Runtime layout system (zero egui dependency)
│   └── src/
│       ├── lib.rs           # Public exports
│       ├── table.rs         # LayoutTable + resolve
│       ├── width.rs, height.rs, flex.rs, margin.rs
│       ├── render.rs        # RenderTree, RenderNode, PaintStyle, LayoutIntent
│       ├── state.rs         # DenRouteState
│       └── router.rs        # DenRouter, DenPage trait
└── den_app/                 # Example application
    └── src/
        ├── main.rs          # Entry point + zoom controls
        ├── den_paint.rs     # The ONLY egui-specific render code
        ├── routes.rs        # den_router! invocation
        ├── pages/           # One folder per page (.rs + .html + .scss)
        └── bin/
            ├── preview.rs
            └── style_editor.rs
```

---

## Makefile Commands

```
make help       # List all commands
make dev        # Hot reload (requires cargo-watch)
make preview    # Generate HTML preview
make review     # Copy diff + review prompt to clipboard
make yoink      # Copy last commit diff to clipboard
make commit     # AI-generated commit message (interactive)
make push       # AI-generated commit + push
```

---

## Status & Roadmap

Shipped:

- [x] `{{ self.field }}` data interpolation
- [x] `(click)="method()"` event binding (no-arg handlers)
- [x] `:hover` pseudo-selector with cursor support
- [x] `display: flex`, `border`, `border-radius`, `width`, `height`, `gap`, `margin`
- [x] CSS text/font properties through TextBox (`font-family`, `line-height`, `text-transform`, etc.)
- [x] `<for each="item" in="self.list">` loop rendering
- [x] `<if cond="self.flag">` / `<else>` conditional rendering
- [x] SCSS variables (`$var: value`)
- [x] Zoom / scale system (50%–300%)
- [x] Backend-agnostic layout system (`den_layout` has zero egui dependency)
- [x] Router with typed `with="..."` data passing (compile-time type-checked)
- [x] `<input bind="self.field" />` two-way binding, including nested paths
- [x] HTML preview generator
- [x] Live style editor with visual controls
- [x] Compile-time validation of fields, methods, and routes referenced in templates

Next priorities (see [PENDING.md](PENDING.md) for details):

- [ ] **Compile-time validation of CSS class names** (`class="typo"` → build error)
- [ ] **Error spans pointing back to `.html` line:column** (IDE underlines inside HTML)
- [ ] **Reusable sub-page components with typed props** (`<StatCard label="..." value="..." />`)
- [ ] Dead CSS detection (unused `.class` → warning)
- [ ] Language Server / autocomplete for fields, classes, handlers, routes inside `.html`
- [ ] Click handlers with arguments inside `<for>` (via `den-bind`)
- [ ] Root `<panel>` element mapping to egui CentralPanel
- [ ] Expanded CSS subset: `opacity` / `rgba`, `box-shadow`, gradients, individual borders, `position: absolute`, SVG icons, real `grid-template-columns`
- [ ] Native `@font-face` registration in egui
- [ ] Advanced input model (selection, clipboard, IME)

---

## Contributing

Den is open source and contributions are welcome. Whether it's a bug fix, new CSS property support, a new HTML tag mapping, or an idea for the roadmap — open an issue or submit a PR. [PENDING.md](PENDING.md) is the living backlog.

## License

MIT
