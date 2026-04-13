<div align="center">

# Den

**Write HTML + SCSS. Compile to native desktop UI.**

Den is a Rust framework that transforms familiar web templates into native [egui](https://github.com/emilg/egui) code at compile time. No runtime overhead. No webview. Just fast, native GUI with a developer experience you already know.

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/rust-1.88%2B-orange.svg)](https://www.rust-lang.org)
[![egui](https://img.shields.io/badge/egui-0.33-purple.svg)](https://github.com/emilg/egui)

*Have you ever coded in Rust, and had to download a billion plugins, where the Den SCSS? Well... Here it is.*

</div>

---

## The Idea

Every Rust GUI framework makes you write UI in Rust. Den doesn't.

You write **HTML** for structure, **SCSS** for styling, and **Rust** for logic -- each in its own file, like a proper frontend framework. At compile time, Den's proc macro reads your templates and generates native egui code. The result is a fully native application with zero runtime template parsing.

```
                      compile time
  .html + .scss  ────────────────►  native egui code  ──►  desktop app
                    parse → resolve → codegen
```

## Quick Start

### Prerequisites

- Rust 1.88+ (`rustup update stable`)
- Linux: `sudo apt-get install libxcb-render0-dev libxcb-shape0-dev libxcb-xfixes0-dev libxkbcommon-dev libssl-dev`
- `cargo install cargo-watch` (for hot reload)

### Create a project

```bash
cargo init my-app
```

Add Den to your workspace. Your project structure should look like:

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

### Write your template

**home.html**
```html
<div class="greeting">Hello World, with style</div>
```

**home.scss**
```scss
$accent: #e94560;

.greeting {
    color: $accent;
    font-size: 24;
}
```

**home.rs**
```rust
use crate::AppRoute;
use den_layout::{DenRouteState, DenRouter};
use eframe::egui;

pub struct HomePage;

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
}
```

At the crate root, define the backend UI alias used by the route/page macros:

```rust
use eframe::egui;

pub type DenUi = egui::Ui;
```

That's it. Run `cargo run --bin den_app` and you get a native desktop window with styled text. No egui boilerplate. No `RichText::new().color().size()` chains. Just HTML and SCSS.

## How It Works

Den uses a 3-phase compile-time pipeline:

```
  Phase 1: Parse         Phase 2: Resolve        Phase 3: Codegen
  ─────────────         ────────────────         ────────────────
  HTML → RawNode tree   RawNode + StyleMap       DenNode tree
  SCSS → StyleMap       → DenNode tree           → TokenStream
                        (with DenVisual)          → egui Rust code
```

1. **Parse**: `den_template!("pages/home/home")` reads `.html` and `.scss`, parses them into `RawNode` tree and `StyleMap`
2. **Resolve**: Merges styles onto elements by class name (last-wins), builds `DenNode` tree with `DenVisual` attached to each element
3. **Codegen**: Walks the `DenNode` tree and emits egui Rust code via `quote!`

All errors become `compile_error!` -- you see them in your IDE immediately.

## Features

### Data Interpolation

Use `{{ self.field }}` to bind component state. Fields must implement `Display`.

```html
<div class="greeting">Hello {{ self.name }}, you are {{ self.age }} years old!</div>
```

```rust
use crate::AppRoute;
use den_layout::{DenRouteState, DenRouter};
use eframe::egui;

pub struct HomePage {
    pub name: String,
    pub age: u32,
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
}
```

### Event Binding

Bind click events with `(click)="method_name()"`:

```html
<div class="button" (click)="on_button_click()">Click me</div>
```

### Conditional Rendering

Use `<if>` and `<else>` for conditional UI:

```html
<if cond="self.logged_in">
    <div class="welcome">Welcome back!</div>
</if>
<else>
    <div class="login-prompt">Please log in.</div>
</else>
```

### Loop Rendering

Use `<for>` to iterate over collections:

```html
<for each="item" in="self.items">
    <div class="item">{{ item }}</div>
</for>
```

### Hover States

SCSS `:hover` pseudo-selector works out of the box:

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

Define variables at the top of your `.scss` file:

```scss
$primary: #0f3460;
$accent:  #e94560;

.title {
    color: $primary;
    font-size: 28;
}

.highlight {
    color: $accent;
}
```

### Zoom / Scale System

All pixel values (`font-size`, `padding`, `margin`, `border-width`, `border-radius`, `width: Npx`) are multiplied by `__den_scale` at render time. Colors, percentages, and layout modes are unaffected.

Built-in controls:
- **Ctrl+scroll** or **Ctrl+=** / **Ctrl+-**: zoom in/out (10% steps)
- **Ctrl+0**: reset to 100%
- **+/-/% widget** in the bottom-right corner
- Range: 50% to 300%

### Layout System

Den includes a runtime layout system (`den_layout` crate) with backend-agnostic rect resolution:

- **`width: 100%`**: fills parent's available width (respects padding)
- **`width: 200px`**: fixed width, scales with zoom
- **`margin: 12`**: reserves non-collapsing outer spacing in block/flex layout and maps to egui outer margin
- **`display: flex`**: horizontal layout with automatic width distribution among children
- **Auto (no width)**: content-sized by default

The layout table is built once and recalculated every frame, so resizing the window reflows everything automatically. The crate itself no longer depends on egui. Generated route/page glue references `crate::DenUi`; the demo app binds that alias to `egui::Ui`, while `den_template!` still emits egui renderer calls.

### Route State

Generated app hosts keep a `DenRouteState` for each route declared with `den_router!`. Page render methods receive that state after the router:

```rust
fn render(
    &mut self,
    ui: &mut egui::Ui,
    __den_scale: f32,
    __den_router: &mut DenRouter<AppRoute>,
    __den_route_state: &mut DenRouteState,
)
```

`DenRouteState` currently groups input and debug state, and is the runtime hook for moving Den toward a generic renderer that consumes a resolved HTML/CSS element tree.

## Dev Tools

### Hot Reload

```bash
make dev
```

Watches `den_app/src` and `den_macros/src`, recompiles and reruns on every change.

### HTML Preview (`make preview`)

Generates `preview/index.html` with all elements tagged `dev` in your templates:

```html
<div dev class="my-component">...</div>
```

Components render as static HTML with real CSS, placeholder values for `{{ expr }}`, and auto-refresh every 3 seconds.

### Style Editor (`cargo run --bin style_editor`)

Separate egui window that parses all `.scss` files and shows visual controls per class:
- Color properties: color picker
- Numeric properties (font-size, padding, etc.): sliders
- Enum properties (display, cursor): dropdowns
- Border: width slider + color picker

Changes write back to disk with 300ms debounce. The `cargo-watch` in `make dev` picks up the change and recompiles.

## Supported Properties

| SCSS Property    | Maps To                    | Example              |
|------------------|----------------------------|----------------------|
| `color`          | `RichText::color()`        | `#e94560`            |
| `font-size`      | `RichText::size()`         | `24` or `24px`       |
| `background`     | `Frame::fill()`            | `#1a1a2e`            |
| `padding`        | `Frame::inner_margin()`    | `16` or `16px`       |
| `margin`         | `Frame::outer_margin()` + layout | `16` or `16px` |
| `display: flex`  | `ui.horizontal()`          | `display: flex`      |
| `border`         | `Frame::stroke()`          | `1px solid #e94560`  |
| `border-radius`  | `Frame::corner_radius()`   | `8` or `8px`         |
| `width`          | `ui.set_width()`           | `100%`, `200px`      |
| `height`         | `ui.set_height()`          | `100%`, `200px`      |
| `gap`            | egui item spacing + layout | `8` or `8px`         |
| `cursor: pointer`| `CursorIcon::PointingHand` | in `:hover` blocks   |

## Supported HTML Tags

| Tag                          | Maps To          |
|------------------------------|------------------|
| `<div>`, `<span>`, `<p>`    | `ui.label()`     |
| `<heading>`, `<h1>`--`<h3>` | `ui.heading()`   |
| `<for>`                      | `for` loop       |
| `<if>` / `<else>`           | `if` / `else`    |

## Architecture

```
den/
├── den_macros/              # Proc macro crate (compile-time pipeline)
│   └── src/
│       ├── lib.rs           # Entry point (~60 lines)
│       ├── input.rs         # Macro input parsing (syn)
│       ├── types/           # Shared types (RawNode, DenNode, DenVisual, StyleRule)
│       │   ├── raw.rs, style.rs, resolved.rs, walk.rs
│       ├── resolve.rs       # Phase 2: style resolution
│       ├── parse/           # Phase 1: HTML + SCSS parsers
│       │   ├── html.rs, scss.rs, text.rs, color.rs
│       └── codegen/         # Phase 3: egui code generation
│           ├── element.rs, click.rs, flex.rs, input.rs
│           ├── control_flow.rs, frame.rs, text.rs
├── den_layout/              # Runtime layout system
│   └── src/
│       ├── lib.rs           # LayoutTable, rect-based layout runtime
│       ├── display.rs, dimension.rs, spacing.rs, flex.rs
│       ├── entry.rs, geometry.rs, table.rs
│       ├── router.rs        # DenRouter and generic DenPage trait
│       └── state.rs         # DenRouteState and per-route runtime state
└── den_app/                 # Application crate
    └── src/
        ├── main.rs          # Entry point + zoom controls
        ├── app_config.rs    # Window size, scale constants
        ├── bin/
        │   ├── preview.rs   # HTML preview generator
        │   └── style_editor.rs  # Live SCSS editor
        └── pages/
            └── home/
                ├── home.rs, home.html, home.scss
```

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

## Roadmap

- [x] `{{ self.field }}` data interpolation
- [x] `(click)="method()"` event binding
- [x] `:hover` pseudo-selector with cursor support
- [x] `display: flex`, `border`, `border-radius`, `width`, `height`, `gap`, `margin` CSS properties
- [x] `<for each="item" in="self.list">` loop rendering
- [x] `<if cond="self.flag">` / `<else>` conditional rendering
- [x] SCSS variables (`$var: value`)
- [x] Zoom / scale system (50%--300%)
- [x] Layout system with backend-agnostic rect resolution
- [x] HTML preview generator (`make preview`)
- [x] Live style editor with visual controls
- [x] Modular architecture (parse / resolve / codegen)
- [x] `<input bind="self.field" />` two-way data binding
- [ ] Root `<panel>` element mapping to egui CentralPanel
- [ ] Nested SCSS selectors (`.parent { .child { } }`)
- [ ] Component system with props
- [ ] Hot reload in development (without full recompilation)

## Contributing

Den is open source and contributions are welcome. Whether it's a bug fix, new CSS property support, a new HTML tag mapping, or an idea for the roadmap -- open an issue or submit a PR.

## License

MIT
