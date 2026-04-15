# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Den

Den is a Rust framework that compiles HTML + SCSS templates into native egui desktop GUI code at compile time via procedural macros. Zero runtime template parsing. The runtime model is **render tree → layout → paint**: the macro generates code that builds a `RenderTree` every frame, the layout engine resolves rects, and a single paint function draws everything via `egui::Painter` — no egui widgets (no `ui.label`, no `Frame::show`, no `TextEdit`).

**Filosofia**: "Angular in Rust" — convention over configuration (DHH), complexidade no compile-time (Svelte), sintaxe que o dev web já sabe sem ler doc (Krug). Prefixo universal `@` pra tudo que é Den; `{{ }}` pra interpolação; resto é HTML/CSS puro.

## Build & Development Commands

```bash
cargo build                    # Build everything
cargo run --bin den_app        # Run the demo application
cargo test --workspace         # Run all workspace tests
cargo test -p den_layout       # Layout engine + RenderTree tests only
cargo test -p den_macros       # Parser tests only
cargo test test_name           # Run a single test by name
cargo clippy --workspace --all-targets -- -W clippy::all
cargo fmt --check              # Verify formatting
make dev                       # Hot reload dev mode (requires cargo-watch)
make preview                   # Generate preview/preview.html with all pages
make help                      # List all makefile commands
```

Debug environment variables:
- `DEN_DEBUG_LAYOUT=1 cargo run --bin den_app` — one-time dump of resolved LayoutTable per template (labels, CSS rules, sizes, rects)
- `DEN_DEBUG_ROUTE_STATE=1 cargo run --bin den_app` — one-time dump of route state per render path

## Architecture

**Workspace structure**: Three crates in a Cargo workspace (resolver v3, edition 2024, Rust 1.88+).

- **`den_macros`** — Proc macro crate. Pipeline Parse → Resolve → Codegen (emits RenderTree build). Exports `den_template!`, `den_router!`, `#[den_page]`.
- **`den_layout`** — Runtime library. Zero egui dependency. Owns `LayoutTable`, `RenderTree`/`RenderNode`/`PaintStyle`/`LayoutIntent`/`Interact`, `DenRouteState` (inputs + focus + cursor + hover), `DenRouter`.
- **`den_app`** — Example app using eframe/egui. Owns `den_paint.rs` (the ONLY egui-specific render code). Defines `pub type DenUi = egui::Ui;` at crate root.

### Runtime pipeline (per frame)

```
  BUILD                 RESOLVE                 PAINT
  ─────                 ───────                 ─────
  macro-generated       layout.entries =        paint_tree walks the
  code pushes           tree.to_layout_         RenderTree. For each node:
  RenderNodes into      entries();              rect_filled → content
  __den_tree. For       measure_tree_text       (text/input) → rect_stroke.
  loops/ifs run         via ui.fonts_mut;       Collects events.
  Rust control flow     then resolve in
  to include the        viewport.
  right nodes.
                                                DISPATCH
                                                ────────
                                                match click/goto slot →
                                                self.handler() or router.goto.
                                                InputChanged → mirror to
                                                route_state + self.field.
```

### Compile-time pipeline (in `den_macros/src/`)

```
  Phase 1: Parse           Phase 2: Resolve         Phase 3: Codegen
  parse/html.rs            resolve.rs               codegen/render_tree.rs
  parse/scss.rs            RawNode + StyleMap       codegen/text.rs
  parse/text.rs            → DenNode tree           codegen/mod.rs
  parse/color.rs           (with DenVisual)         → TokenStream that
  → RawNode + StyleMap                                builds RenderTree
                                                      + dispatches events
```

1. `den_template!("pages/home/home", self)` reads `.html` and `.scss` files relative to `den_app/src/`
2. `parse/html.rs` → `Vec<RawNode>` tree (hand-rolled, UTF-8 safe via `Vec<char>`)
3. `parse/scss.rs` → `StyleMap` (supports `$variables`)
4. `resolve.rs` → merges styles, produces `Vec<DenNode>` with `DenVisual`
5. `codegen/render_tree.rs` → emits `__den_tree.push(RenderNode { ... })` calls
6. `codegen/mod.rs` → wraps build + paint call + event dispatch match arms

**Key source files**:
- `lib.rs` — Entry point, wires the 3 phases
- `input.rs` — `DenTemplateInput` syn parsing
- `types/` — Shared types (`RawNode`, `DenNode`, `DenElement`, `DenVisual`, `StyleRule`)
- `codegen/render_tree.rs` — `emit_build_node()` — main emitter, walks `DenNode` directly; `BuildCtx` tracks `handlers` / `goto_slots` / `input_mirrors` tables
- `codegen/mod.rs` — `generate()` — orchestrates build/paint/dispatch wrapper

All errors become `compile_error!` — users see IDE errors immediately.

### The generated code pattern

For a template `den_template!("pages/home/home", self)`, the macro expands to roughly:

```rust
{
    let mut __den_tree = den_layout::RenderTree::new();
    {
        let __den_parent: usize = usize::MAX;
        // One block per element:
        {
            let __den_li = __den_tree.nodes.len() + 1;
            let __den_node_id = den_layout::DenNodeId::new(<hash>);
            let __den_node = RenderNode {
                node_id: __den_node_id,
                layout_index: __den_li,
                kind: RenderKind::Text { content: format!(..., self.name), heading: false },
                style: PaintStyle { color: Some((..)), background: Some((..)), .. },
                hover_style: Some(PaintStyle { .. }),          // resolved hover override
                interact: Interact { click_handler: Some(0u32), .. },
                layout: LayoutIntent { width_rule: .., display: .., .. },
                children: Vec::new(),
            };
            let __den_idx = __den_tree.push(__den_node);
            if __den_parent == usize::MAX { __den_tree.roots.push(__den_idx); }
            else { __den_tree.nodes[__den_parent].children.push(__den_idx); }
            { let __den_parent: usize = __den_idx; /* children pushes here */ }
        }
        // <for each="tag" in="self.tags"> becomes real Rust:
        for (__den_idx_0, tag) in (self.tags).iter().enumerate() { /* child pushes */ }
    }

    thread_local! { static STORE: RefCell<LayoutTable> = ...; }
    let __den_events = STORE.with(|tl| {
        let mut layout = tl.borrow_mut();
        crate::den_paint::paint_tree(ui, __den_scale, &mut __den_tree, &mut layout, __den_route_state)
    });

    for __ev in __den_events {
        match __ev {
            PaintEvent::Click { handler } => match handler {
                0u32 => { self.increment_count(); }
                _ => {}
            },
            PaintEvent::Goto { slot } => match slot {
                0u32 => { __den_router.goto(crate::__den_route_UsuarioPage()); }
                _ => {}
            },
            PaintEvent::InputChanged { node_id, value } => {
                __den_route_state.inputs_mut().set(node_id, value.clone());
                if node_id == DenNodeId::new(<hash>) { self.name = value.clone(); }
            }
        }
    }
}
```

### Template syntax

**Prefixo `@`** — toda sintaxe Den começa com `@` (eventos, bindings, controle de fluxo, escopo). Interpolação usa `{{ }}`. Três conceitos, zero surpresa.

| Sintaxe | Significado |
|---------|-------------|
| `{{ }}` | Mostrar valor (interpolação, com pipes opcionais) |
| `@` | Den faz algo (controle, binding, evento, navegação, escopo) |
| resto | HTML/CSS puro |

**Macro invocation**:
- `den_template!("pages/home/home")` — without self, no interpolation or events
- `den_template!("pages/home/home", self)` — enables `{{ self.field }}` and `@click` events

**Interpolação + pipes**: `{{ self.field }}` — gera `self.field` direto. Campos devem implementar `Display`.
- Com pipes: `{{ self.name | upper }}`, `{{ self.bio | truncate(80) | upper }}`, `{{ self.price | currency(br) }}`
- Pipes são unidirecionais (pipeline, não árvore). Valor entra, valor sai, próximo pipe recebe.
- Built-in (vêm com `den_layout::pipes`): `upper`, `lower`, `trim`, `truncate(n)`, `currency(locale)`/`money(locale)`, `number(casas)`, `join(sep)`, `default(val)`, `date(format)`.
- Pipe custom: implementa `Pipe<T>` (den_layout) e exporta sob `crate::pipes::NomePipe`. Type-safe em compile-time.

**Event binding** (`@click`):
- `@click="handler()"` — registered in the template's click handler table, dispatched via `PaintEvent::Click { handler: slot }`
- `@click="handler(arg1, arg2)"` — **not yet supported in the renderer**; the codegen returns a compile error. See PENDING.md.

**Input binding** (`@bind`): `<input @bind="self.field" placeholder="..." class="style" />` — the framework owns the input (no egui `TextEdit`):
- First render: hydrates `DenRouteState.inputs` from `self.field`
- Subsequent frames: `self.field` reads from route state
- Paint function handles focus, cursor movement, keyboard events (`Text`, `Backspace`, `Delete`, `ArrowLeft/Right`, `Home`, `End`, `Escape`/`Enter` to blur)
- Caret is a painted `line_segment` with blink on `ctx.input().time % 1.0 < 0.5`
- On change: emits `PaintEvent::InputChanged` → macro dispatch writes to both route state and `self.field`

**Navigation** (`@goto` / `@with`):
- `@goto="PageName"` — registered in goto slots table, dispatched via `PaintEvent::Goto { slot }` → `__den_router.goto(crate::__den_route_PageName(...))`
- `@with="expr1, expr2"` — passes cloned arguments to the target page constructor
- `@goto` and `@click` cannot coexist on the same element

**Escopo de binding** (`@object`): `@object(self.pessoa) { <input @bind="nome" /> }` — dentro do bloco, `@bind` sem prefixo `self.` é resolvido contra o scope. Elimina repetição de `self.pessoa.` em cada input.
- Scope é aplicado no resolve; `@bind="self.x"` explícito (com prefix) escapa do escopo.
- Scope não afeta `{{ }}` nem `@click`/`@goto` (expressões Rust completas são esperadas).

**SCSS variables**: `$var: value;` at top of file, referenced as `color: $var;`.

**Style inheritance**: inheritable text CSS propagates parent → child (`color`, font family/size/weight/style, line-height, letter-spacing, text-transform, text-align). `text-decoration` is transported to `PaintStyle`, but not inherited as a resolved child property. Hover and layout rules do not inherit.

**Control flow** (`@if`/`!`/`@for`/`@empty`):
- `@if(self.cond) { ... } ! { ... }` — se/senão. `!` sem condição = catch-all (else).
- Cadeia: `@if(self.status == "active") { ... } !status == "pending" { ... } !status == "error" { ... } ! { ... }`. Em `!COND`, identificadores bare são prefixados com `self.` automaticamente.
- `@for(item in self.items) { ... }` — gera `for (idx, item) in self.items.iter().enumerate() { /* push children */ }`. Loop index sala o `node_id` pra hover/focus estáveis por item.
- `@for(...) { ... } @empty { ... }` — ramo `@empty` renderizado quando a iterável está vazia.
- Zero `@else`/`@else if`/`@switch`/`@case`. Só `@if` e `!`.

**GhostService™** — async sem async:
- `#[derive(DenGhost)]` em structs: gera `DenGhost::ghost()` com mocks por campo.
- `#[ghost("valor")]` por campo customiza o mock (tipos numéricos parseados, String copiada, expressão Rust em fallback).
- `DenGhostService<T>` (den_layout) wrap o valor: começa `loading: true` com ghost, roda fetch em thread, vira real no próximo `tick()`.
- No template: `@if(self.user.loading) { <skeleton /> } ! { <div>{{ self.user.nome }}</div> }`. Zero `await`, `spawn`, `subscribe`.

### Router and page macros

- `den_router!` — declares routes and generates the `AppRoute` enum, `AppPages` host struct, route helpers (`__den_route_PageName`), and the render dispatch. Defined in `routes.rs` of the app crate.
- `#[den_page]` — attribute macro for pages with typed route data (generates `DenPage<Route, DenUi>` impl).
- Pages with data: `HelloPage { usuario: Usuario }` in `den_router!` declares that navigation to `HelloPage` requires passing a `Usuario`.

### Layout system (`den_layout` crate, runtime)

- `LayoutTable` — flat `Vec<LayoutEntry>` with parent/children indices. Entry 0 is the invisible body. Lives in a `thread_local!` to reuse allocations.
- `RenderTree::to_layout_entries()` — builds the entries Vec from the current render tree each frame.
- `DimensionRule`: `Auto` | `Px(f32)` | `Percent(f32)`.
- `DisplayMode`: `Block` | `Flex` | `Grid` (Grid falls back to Block for now).
- `resolve_in_viewport(width, height)` — single DFS pass.
- Block layout: stacks children vertically using content width minus padding/margin, with gaps between.
- Flex layout: horizontal distribution. `flex: 1` / `flex-grow: 1` Auto children split remaining width after fixed/intrinsic widths, margins, gaps.
- All values in CSS pixels. The paint function multiplies by `__den_scale` at draw time.
- **`layout_index` is runtime-assigned**: `__den_tree.nodes.len() + 1` at push time. Invariant: parent.layout_index < child.layout_index, body = 0.

### Paint function (`den_app/src/den_paint.rs`)

The only egui-specific render code. Signature:

```rust
pub fn paint_tree(
    ui: &mut Ui,
    scale: f32,
    tree: &mut RenderTree,      // mut: intrinsic sizes are filled in here
    layout: &mut LayoutTable,   // mut: entries + rects populated in this call
    state: &mut DenRouteState,
) -> Vec<PaintEvent>
```

Steps:
1. **Measure**: build a TextBox/galley for each `RenderKind::Text` and `Input` using CSS-declared font/text properties, then write `LayoutIntent::intrinsic_{width,height}` in CSS pixels.
2. **Populate**: `layout.entries = tree.to_layout_entries()`, resize `sizes`/`rects`.
3. **Resolve**: `layout.resolve_in_viewport(available / scale)`.
4. **Paint**: DFS walk. For each node:
   - `ui.interact(rect, Id::new(node_id.raw()), click_or_hover_sense)` — egui detects click/hover
   - Pick `active_style` (`hover_style` when `resp.hovered()`, else base)
   - `painter.rect_filled` (background) → content (text galley / input) → `painter.rect_stroke` (border)
   - Recurse children (painted on top)
   - Emit `PaintEvent::Click` or `PaintEvent::Goto` on `resp.clicked()`
5. **Input**: focus transitions on click / click-elsewhere, keyboard event processing, caret painted as `line_segment` with blink, emits `InputChanged` on mutation.

### Route state (`den_layout::DenRouteState`)

One per active route in the `AppPages` host. Cleared on navigation. Holds:
- `inputs: DenInputState` — `HashMap<DenNodeId, String>` for input values
- `focus: Option<DenNodeId>` — currently focused input
- `cursor: HashMap<DenNodeId, usize>` — byte-offset caret per input
- `hover: HashSet<DenNodeId>` — populated by paint each frame
- `debug: DenDebugState` — debug dump tracking

The paint function owns read/write access; dispatch also writes to `inputs` when mirroring.

### Scale system

- `__den_scale: f32` parameter on `render()`, multiplies all pixel values at paint time
- Scales: `font-size` (min 6.0 screen px), `line-height`, `letter-spacing`, `padding`, `margin`, `border-width` (min 1.0), `border-radius`, `width: Npx`, `height: Npx`, `gap`
- Does NOT scale: `color`, `background`, `%` widths, `display`, `cursor`
- Controls: Ctrl+=/Ctrl+-/Ctrl+0/Ctrl+scroll, +/-/% widget at bottom-right
- Zoom is global for the demo app

### SCSS → paint property mapping

| SCSS Property    | Paint operation                          | Scaled | Values                            |
|------------------|------------------------------------------|--------|-----------------------------------|
| `color`          | `painter.galley` color                   | —      | `#RRGGBB`, `#RGB`, `$variable`    |
| `font-size`      | TextBox `FontId` size                    | yes    | `24` or `24px`                    |
| `font-family`    | TextBox `FontFamily` stack               | —      | `"Inter", sans-serif`             |
| `font-weight`    | `PaintStyle.font_weight`                 | —      | `normal`, `bold`, `100`-`1000`    |
| `font-style`     | `TextFormat.italics`                     | —      | `normal`, `italic`, `oblique`     |
| `font`           | shorthand for style/weight/size/line/family | mixed | `italic 600 16px/1.4 Inter`       |
| `line-height`    | TextBox line height                      | yes    | `20px`, `1.4`, `140%`             |
| `letter-spacing` | TextBox extra letter spacing             | yes    | `0.5px`, `normal`                 |
| `text-transform` | text transform before measure/paint      | —      | `uppercase`, `lowercase`, etc.    |
| `text-align`     | text x-position inside node rect         | —      | `left`, `center`, `right`         |
| `text-decoration`| `TextFormat` underline/strikethrough     | yes    | `underline`, `line-through`, `none`|
| `background`     | `painter.rect_filled`                    | —      | `#RRGGBB`, `#RGB`, `$variable`    |
| `padding`        | `LayoutIntent.padding`                   | yes    | `16` or `16px`                    |
| `margin`         | `LayoutIntent.margin`                    | yes    | `16` or `16px`                    |
| `display: flex`  | `LayoutIntent.display = Flex`            | —      | only `flex` value supported       |
| `border`         | `painter.rect_stroke`                    | yes    | `1px solid #RRGGBB`               |
| `border-radius`  | rect corner radius                       | yes    | `8` or `8px`                      |
| `width`          | `LayoutIntent.width_rule`                | Px/Auto| `100%`, `50%`, `200px`, `auto`    |
| `height`         | `LayoutIntent.height_rule`               | Px/Auto| `100%`, `200px`, `auto`           |
| `gap`            | `LayoutIntent.gap`                       | yes    | `8` or `8px`                      |
| `cursor: pointer`| `CursorIcon::PointingHand` on hover      | —      | only in `:hover` blocks           |

Supported HTML tags: `div`, `span`, `p`, `heading`/`h1`-`h3`, `input`, `for`, `if`/`else`. Visual tags become `RenderKind::Text` or `RenderKind::Container`.

**Page pattern**: Each page is a struct with `render(&mut self, ui: &mut egui::Ui, __den_scale: f32, __den_router: &mut DenRouter<AppRoute>, __den_route_state: &mut DenRouteState)` that calls `den_template!`. The `__den_scale`, `__den_router`, and `__den_route_state` names are framework-reserved by convention.

### Dev tools (binaries in `den_app/src/bin/`)

- `preview.rs` — Generates a single `preview/preview.html` containing all pages as static HTML. Page CSS is scoped per preview section to avoid class collisions. Relative `url(...)` font assets are copied to `preview/fonts/`. Has its own HTML/SCSS helpers (duplicated from `den_macros` — see PENDING.md).
- `style_editor.rs` — Separate egui window with visual controls per SCSS class. Writes back with surgical byte-offset replacement and 300ms debounce. Resolves `$variables` to literals on write-back.

### Config modules

- `den_app/src/paint_config.rs` — painter constants (minimum font/border sizes and input text padding).
- `den_app/src/bin/preview_config/mod.rs` — preview output names, viewport width, refresh interval, simulated loop count, and unitless-px properties.
- `den_app/src/bin/style_editor_config/mod.rs` — style editor debounce/scan intervals, slider bounds, defaults, and UI dimensions.
- `den_macros/src/codegen/config.rs` — compile-time intrinsic text/input estimates used before runtime galley measurement.

### Known limitations

- **`(click)` with arguments**: compile error in the renderer. Simple `(click)="handler()"` works. See PENDING.md for the planned dispatch-by-node-id fix.
- **Text wrapping**: TextBox measurement is single-line for now. Long text may overflow its container. Layout engine still reserves width from the measured galley.
- **No text selection / no IME range** in inputs — keyboard editing is basic (insert, backspace, arrows, home/end).
- **Grid layout** declared but not implemented; falls back to Block.
- **Margin collapse** not implemented; each margin is fully reserved.

### Known duplications

- `collect_scss_vars` exists in 3 places: `parse/scss.rs`, `preview.rs`, `style_editor.rs`
- HTML parser helpers duplicated between `den_macros` and `preview.rs`
- Fix: extract `den_core` crate with shared parsers and types (see PENDING.md)
