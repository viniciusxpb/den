# Pendências

Itens intencionalmente deixados pra depois. Apaga quando resolver.

> **Norte do projeto** — Den é Angular inspirado em Rust (AOT compile de `.html` + `.scss` pra nativo), não browser engine. O diferencial é **eliminar erro silencioso no front** via macro + rustc: template que referencia campo/classe/método que não existe **não compila**. A régua pra priorizar pendências: "isso fortalece a checagem compile-time?" > "isso cobre mais CSS?" > "isso é polimento visual?".

---

## `DenGhostService::tick()` automático nas pages

Hoje cada page com `DenGhostService<T>` precisa chamar `self.field.tick()` manualmente em `render` ([home.rs:46](den_app/src/pages/home/home.rs#L46)) — se o dev esquece, o ghost fica `loading: true` pra sempre, sem aviso. Falha silenciosa clássica.

**Fix futuro**: trait `DenPage` com `fn tick(&mut self) {}` default vazio, ou (melhor) macro `#[den_page]` que escaneia campos `DenGhostService<_>` da struct e gera o tick automático no início do `render`. Combina com a filosofia "convention over configuration".

**Impacto**: zero boilerplate por page; impossível esquecer.

---

## `DenGhostService::fetch` deveria aceitar `Result<T, E>` em vez de `T`

Hoje a closure de `fetch` retorna `T`. Pra surgir um erro de HTTP/parse, o demo do home grava a mensagem DENTRO do field do model ([home.rs:73-78](den_app/src/pages/home/home.rs#L73-L78)) — `CatFact { fact: "(falha no fetch: …)" }`. Funciona porque `loading` vira `false` e a UI mostra o texto, mas é semanticamente errado: erro de fetch deveria ser separado do dado bem-sucedido.

`GhostError::FetchPanicked` JÁ existe pro caso de panic, mas erros de aplicação (HTTP 500, JSON inválido) hoje só conseguem ser surfaceados injetando a mensagem no próprio `T`.

**Fix futuro**: `fetch<F: FnOnce() -> Result<T, String>>` em vez de `FnOnce() -> T`. `tick()` distingue `Ok` (vira valor real) de `Err` (vira `GhostError::FetchFailed(msg)`). Template pode então fazer `@if(self.cat.error()) { ... } !if(self.cat.loading) { ... } ! { ... }`.

---

## JSON parsing manual no demo de cat fact

[home.rs](den_app/src/pages/home/home.rs) tem `extract_string_field`/`extract_number_field` baseado em `find()`. Funciona pro schema `{"fact":"...","length":N}` mas é frágil:
- Não decoda escapes (`\"`, `\n`, `\u00e1`).
- Se o JSON tiver `"fact"` numa key aninhada antes do top-level, pega o errado.
- Não trata float, exponential, null.

Fiz manual pra evitar dep de `serde_json` só por dois campos. **Fix futuro**: adicionar `serde_json` quando aparecer a segunda API que precise parse JSON.

---

## Validação compile-time de classes CSS

Hoje, `class="naem-errado"` em HTML simplesmente não aplica estilo nenhum em runtime — falha silenciosa, o bug #1 de todo dev front. O parser já tem acesso ao HTML e ao SCSS na mesma fase de macro ([resolve.rs:48-50](den_macros/src/resolve.rs)), só que o match por classe hoje ignora sem erro quando a classe não existe no StyleMap.

**Fix futuro**: no resolve, varrer `el.classes` e emitir `compile_error!` pra toda classe usada em HTML que não aparece no SCSS daquela página (considerando herança e classes definidas em SCSS compartilhados se existirem). Precisa permitir whitelist de classes dinâmicas / externas via atributo (`class:dynamic="..."` ou similar) pra não bloquear casos legítimos.

**Impacto**: é o diferencial mais vendável pra dev front. Rename de `.btn-primary` pra `.btn-main` passa a ser refactor seguro — se HTML não foi atualizado, build quebra.

---

## Dead CSS detection (subproduto da validação de classes)

Com o mesmo cruzamento HTML × SCSS da pendência acima, sai de graça a detecção inversa: classe declarada em `.scss` que ninguém usa em `.html`.

**Fix futuro**: warning (não erro) de "classe `.foo` declarada em `pages/home/home.scss` mas nunca referenciada". Opcional: modo `--strict` que promove pra erro. Dá resultado parecido com PurgeCSS/Tailwind JIT mas direto do macro, sem tooling externo.

**Decisão pendente**: escopo — só warn de classe não usada na própria página, ou cruza com todos os `.html` do projeto?

---

## Spans de erro apontando pro `.html` original

Hoje quando um `@bind="self.naem"` falha, o rustc aponta pro token dentro da expansão do `den_template!`, não pra linha:coluna do `.html`. O erro aparece, mas o dev não vê sublinhado dentro do arquivo que ele editou.

**Fix futuro**: parser HTML carrega `(line, col)` de cada atributo/texto. Codegen usa `proc_macro2::Span` customizado (ou `syn::spanned::Spanned` com mapeamento) pra que `compile_error!` aponte pro `.html`. Isso fecha o loop: IDE sublinha dentro do HTML o campo inexistente, a classe inexistente, o método inexistente.

**Impacto**: esse é o momento "cai o queixo" do dev front. Sem isso, a validação funciona mas não parece mágica. Com isso, fica comparável ao Angular Language Service — com a diferença de ser feature do próprio macro, não um LSP de 50k linhas.

---

## Language Server / autocomplete em `.html`

Extensão natural do item acima: quando o HTML tá válido e a struct da page tá acessível, o IDE pode sugerir:
- campos da struct ao digitar `{{ self.` ou `@bind="self.`
- classes declaradas no `.scss` correspondente ao digitar `class="`
- nomes de handlers disponíveis ao digitar `@click="`
- rotas declaradas no router ao digitar `@goto="`
- pipes built-in e custom ao digitar `| ` dentro de `{{ }}`

**Decisão pendente**: implementar como LSP custom em Rust, ou começar com extensão VS Code que lê metadados emitidos pelo macro (tipo um `.den-meta.json` gerado no build com lista de classes/campos/rotas por página)? O segundo é 10× mais simples e entrega 80% do valor.

---

## Componentes reutilizáveis sub-página (com props)

Hoje a unidade reutilizável é a **page**: `@with="self.usuario"` + `@goto="..."` funciona como `@Input` na fronteira de navegação. Mas dentro de uma mesma page, não dá pra ter `<StatCard label="CLICKS" value="{{ self.count }}" color="blue" />` — precisa copy-paste. [home.html:12-28](den_app/src/pages/home/home.html) tem três `stat-card` quase idênticos, é o cheiro clássico.

**Fix futuro**: mecanismo tipo `den_template!` mas sem precisar de rota — um `den_component!("components/stat_card/stat_card")` que aceita struct de props, ou macro `#[den_component]` aplicada a uma struct que ganha `.render_inline(ui, scale, props)`. Resolve por tag customizada (`<StatCard />`) ou por atributo (`<div component="StatCard" label="..." />`).

**Decisão pendente**: sintaxe — tag PascalCase estilo React (`<StatCard>`), atributo explícito, ou macro invocável. Todas as três são factíveis dado o pipeline atual.

---

## Click handlers com argumentos dentro de `@for`

O renderer genérico usa tabela de slots por template: cada `@click="f()"` vira um `Interact::click_handler: Some(slot)`, e o match de dispatch roteia `PaintEvent::Click{handler:slot} → self.f()`. Funciona pra handlers SEM args.

Com args, hoje o codegen retorna erro explícito. O bloqueio: args dentro de `@for(...)` referenciam variáveis do escopo do loop (`user.id`, `idx`), que não existem no ponto de dispatch (fora da closure de build). O atributo `den-bind` já tá reservado no parser justamente pra isso — auto-clone de vars do loop pro dispatch.

**Fix futuro**: dispatch via node_id em vez de slot. Cada nó clicável guarda sua ação como closure clonada em runtime. Alternativa: gerar uma tabela `HashMap<DenNodeId, Box<dyn FnOnce(&mut Self)>>` preenchida no build; o dispatch lê do map por node_id, e `den-bind` declara explicitamente quais vars do escopo precisam ser capturadas/clonadas.

---

## `tree_path: Vec<usize>` → `Vec<TreeSegment>` tipado

O `tree_path` acumulado no codegen ([render_tree.rs:36](den_macros/src/codegen/render_tree.rs#L36)) entra no hash do `node_id`. Hoje é `Vec<usize>` e branches de `@if`/`@empty` dependem de salts numéricos mágicos (`EMPTY_BRANCH_SALT=10_000`, `ELSE_BRANCH_SALT=9_000_000`, `IF_BRANCH_SALT_STRIDE=1_000` em [control_flow.rs:12-14](den_macros/src/codegen/control_flow.rs#L12-L14)) pra evitar colisão entre nós em branches diferentes.

**Fix futuro**: trocar por `enum TreeSegment { Child(usize), IfBranch(usize), ElseBranch, EmptyBranch, LoopIter }`. Elimina os salts mágicos, o hash fica auto-explicativo, e abre caminho pra um `DEN_DEBUG_NODE_IDS=1` que imprime `Child(0) → IfBranch(1) → Child(2)` legível.

**Impacto**: só mexe no codegen (não vaza pra runtime). Constantes de salt saem; parser não muda.

---

## `date` pipe com formatação real

`den_layout::pipes::Date` ([pipes.rs](den_layout/src/pipes.rs)) é um stub: recebe qualquer `ToString`, retorna o valor bruto, loga `eprintln!` uma vez avisando "não implementado". Doc lista `| date("dd/MM/yyyy")` como built-in, mas nada acontece de fato.

**Fix futuro**: integrar com `chrono` ou `time`. Aceitar `i64` (unix timestamp), `chrono::DateTime<_>`, ou `time::OffsetDateTime`. Formato `"dd/MM/yyyy HH:mm"` via strftime-like.

**Decisão pendente**: adicionar `chrono` como dep opcional (`features = ["chrono"]`) ou sempre embutir? `chrono` é pesado; `time` é mais leve. Provavelmente `time` atrás de feature.

---

## Preservar mensagem de panic em `DenGhostService::fetch`

Quando a closure do `fetch` entra em panic, a thread captura via `catch_unwind` ([ghost.rs](den_layout/src/ghost.rs)) mas NÃO envia nada pelo channel. O receiver detecta via `Disconnected` e seta `error = FetchPanicked("fetch closure panicked or dropped without sending")`. A mensagem real do panic é perdida.

**Fix futuro**: trocar o tipo do channel de `Sender<T>` pra `Sender<Result<T, String>>`. Na thread, extrair `panic_info` do `catch_unwind` Err (via downcast) e enviar `Err(msg)`. Custo: toda `tick()` precisa desempacotar `Result` (barato) e todo fetch path aceita tipo ligeiramente mais complexo.

**Impacto**: melhora DX de dev sem backend. Hoje um panic no mock silencia com mensagem genérica; o fix mostra o stack real do bug.

---

## Elemento raiz `<panel>` nos templates Den

Templates deveriam ter um elemento raiz explícito (`<panel>` ou similar) que mapeia pro `CentralPanel` no egui e pro container do preview no browser.

Hoje `home.html` começa direto com os filhos. O correto seria:
```html
<panel>
    <h1 class="title">...</h1>
    ...
</panel>
```

No egui, `<panel>` mapearia para `ScrollArea::vertical()` ou o contexto do `CentralPanel`. No preview, seria um `<div>` que preenche a largura do browser. Benefício: `width: 100%` em qualquer filho resolve relativo ao painel pai, sincronizando preview e egui sem hardcodar largura.

`EGUI_WINDOW_WIDTH` em `preview.rs` continua sendo o encaixe atual, mas a decisão de API (`<panel>`, atributo no template, ou configuração de app) ainda precisa ser tomada.

---

## Style editor resolve variáveis SCSS em literal ao escrever de volta

Quando o usuário edita um valor no style editor (ex: slider numa propriedade `color: $primary`), o editor resolve `$primary` → `#0f3460` pra exibir o color picker, mas ao escrever de volta emite o literal em vez de preservar a referência.

Comportamento intencional por ora (usuário pode querer "desconectar" do token), mas também apaga variáveis sem aviso quando só arrasta e solta no mesmo valor.

**Fix futuro**: comparar `to_scss_string()` com o valor original antes de emitir. Se o resolvido for igual ao original resolvido, manter a string original (com variável). Só substituir quando o valor mudou de fato.

---

## Extração pra `den_core` (elimina parsers duplicados)

Os parsers HTML/SCSS estão duplicados entre `den_macros`, `preview.rs` e `style_editor.rs`. Criar um crate `den_core` com parsers + types compartilhados eliminaria a triplicação de `collect_scss_vars`, `vars_by_longest_name` e helpers de HTML.

**Decisão pendente**: `den_core` como crate pública do workspace (API reutilizável por apps, e base pro futuro LSP) ou interna (só divide responsabilidade entre macro, preview e style editor)? A pendência do LSP acima favorece a opção pública.

---

## Registro nativo de fontes `@font-face`

O parser já transporta propriedades textuais (`font-family`, `font`, `font-weight`, etc.) até o `PaintStyle`, e o painter mede/pinta via TextBox. O preview HTML já copia URLs relativas de `@font-face` para `preview/fonts/`. No backend egui nativo, falta registrar automaticamente os bytes dessas fontes em `egui::FontDefinitions`.

**Decisão pendente**: onde vive o registro de fontes:
1. o macro coleta `@font-face` no SCSS e gera uma função/constante de assets por página;
2. `den_core` coleta fontes e o app chama um bootstrap único antes de renderizar;
3. o app declara manualmente um mapa de fontes, e o CSS só referencia nomes já registrados.

Também falta decidir como mapear peso/estilo para faces reais. Egui 0.33 não seleciona peso automaticamente só pelo `TextFormat`, então Den precisa escolher política própria (mapa `(família, peso, itálico) → FontId`).

---

## Política para `display: grid`

Hoje o parser aceita `display: grid`, mas `den_layout::LayoutTable` trata como fluxo block ([table.rs:69](den_layout/src/table.rs#L69)). Ignorar silenciosamente conflita com o norte do projeto (falha silenciosa é o inimigo).

**Decisão pendente**: escolher em ordem de preferência:
1. **Fail loud**: `compile_error!` pra `display: grid` até grid real existir. Coerente com a identidade "zero silent failure".
2. Implementar grid mínimo no layout engine (`grid-template-columns` com `fr` + `px` + `%`). Desbloqueia layouts tipo sidebar + stats tabelados, que dev front escreve sem pensar.
3. Manter fallback para block e documentar como experimental (menos preferido — contraria o norte).

A opção 1 é o que "Angular in Rust" faria. A opção 2 é investimento maior mas resolve um padrão muito comum.

---

## Modelo avançado de input

O input atual é pintado manualmente e cobre foco, caret, texto, backspace/delete, setas, home/end e blur. Ainda faltam seleção, clipboard, tab focus, mouse positioning e composição IME.

**Decisão pendente**: continuar evoluindo o input manual (preserva painter puro, consistência visual, controle total), ou introduzir ponte controlada pra widget egui nativo em inputs complexos (ganha rápido mas quebra a pureza do modelo "só painter")?

---

## Quebra de módulos grandes

Arquivos ainda grandes:
- `den_app/src/bin/preview.rs` (~1.0k linhas)
- `den_app/src/bin/style_editor.rs` (~770 linhas)
- `den_app/src/den_paint.rs` (~760 linhas)
- `den_layout/src/table.rs` (~600 linhas)
- `den_macros/src/parse/html.rs` (~520 linhas)

**Fix futuro**: dividir por responsabilidade sem mudar comportamento. Sugestão inicial:
- `preview`: discovery, scss_to_css, html_convert, render_html.
- `style_editor`: model, parser, writer, controls, app.
- `den_paint`: tree, node, text, input, geometry.

---

## Backend alternativo além do egui

O `paint_tree` mora em `den_app/src/den_paint.rs` como função concreta. Pra trocar backend (iced, wgpu direto, canvas web, Skia), seria preciso:
1. Definir o alias `DenUi` no app target com o tipo adequado (já existe esse hook).
2. Implementar um `paint_tree` equivalente que aceite a `RenderTree` + `LayoutTable` e desenhe no backend escolhido.
3. O macro gera `crate::den_paint::paint_tree(...)` — trocar o módulo no crate root troca o backend.

Nenhum código de `den_layout` / `den_macros` precisa mudar. Isso é o que permite iOS/Android futuro sem reescrever o framework.

---

## Expansão do subset CSS suportado (ex-"gap visual ndnm")

A tela `ndnm` funciona como *teste de limite* do que o runtime não cobre hoje. Reframing: não é paridade com React, é expansão priorizada do subset CSS que dev front escreve por default em app moderno.

### ✅ Implementado

- `opacity`, `rgba(...)`, alpha em hex (`#RRGGBBAA`, `#RGBA`), `transparent`
- Named colors CSS3 completo (148 nomes: `black`, `rebeccapurple`, `papayawhip`, etc.)
- `box-shadow` com múltiplas sombras, `inset`, blur simulado por rects concêntricos
- Bordas individuais: `border-<side>`, `border-<side>-width`, `border-<side>-color`, `border-width`, `border-color` uniformes
- `white-space: nowrap|pre|pre-wrap|normal|pre-line`, `text-overflow: ellipsis|clip` com truncamento real + "…"
- **Flex completo**: `flex-direction: row|column`, `align-items: stretch|flex-start|center|flex-end`, `justify-content: flex-start|center|flex-end|space-between|space-around|space-evenly`. Abstração main/cross axis isola a lógica.
- `position: static|relative|absolute|fixed`, `top/right/bottom/left`, `z-index`, `inset` shorthand
- `min-width`/`max-width`/`min-height`/`max-height`

### ⏳ Alta prioridade (próxima leva)

- **`background: linear-gradient(...)` / `radial-gradient(...)`** — grid de pontos do canvas, scanlines CRT, gradientes de cards. Sem isso o ndnm ainda perde o fundo.
- **`transform: rotate(Ndeg)` / `scale(f)` / `translate(x, y)`** — wires angulados; sem isso seguem como retângulos horizontais.
- **`overflow: hidden|visible|scroll`** com clipping real — ports que "saem" do node precisam; scroll vertical de listas também.
- **`@font-face` + font loader** — "DenMonospace", "JetBrains Mono", "Inter" silenciosamente caem pra default (ver pendência "Registro nativo de fontes").

### ⏳ Média prioridade

- **`display: grid` + `grid-template-columns`** com `fr` + `px` + `%` (ver pendência dedicada abaixo).
- **`flex-wrap: wrap`** — listas longas que quebram linha.
- **`flex-shrink`**, **`flex-basis`**, **`flex: 1 1 auto`** shorthand completo.
- **`row-reverse`/`column-reverse`** — hoje warn + cai no eixo sem reverse.
- **Múltiplos backgrounds**: `background: url(a), linear-gradient(...)` em camadas.
- **`filter: blur() / brightness() / ...`** — necessário pra shadows com blur GPU e efeitos tipo backdrop.
- **SVG pra ícones** (lucide-react style) — dev front espera `<Icon name="database" />` funcionando.

### ⏳ Baixa prioridade (nice to have)

- **`@keyframes` / `animation` / `transition`** — status dots pulsando, queue progress animado, hover suave. Precisa runtime timer + diff de frames.
- **3D transforms**, **`perspective`**, **`backdrop-filter`**.
- **`repeating-linear-gradient`**, **gradientes cônicos**.
- **`text-selection`** (input ranges, clipboard), **IME** pra idiomas com composição.
- **`calc()`** em valores (`width: calc(100% - 20px)`).
- **CSS custom properties** (`--var: foo; color: var(--var)`) além dos `$scss-vars` atuais.

### Diretriz pra novos CSS rules

- **Falha alta**: valor desconhecido ignora + `eprintln!` warning, nunca aceita silenciosamente. Exceção: valores listados como "não suportado hoje mas planejado" podem cair pra default mais próximo (ex: `row-reverse → row`) com warning explícito.
- **Toda prop que chega em `StyleRule` ou `DenVisual` é `Option<T>`** — cascade preservado (regra 6 do REVIEW_PROMPT.md).
- **Teste de cascade obrigatório**: quando a prop suporta override por `:hover`, inclua teste que cria base + hover + faz `merge_from` e valida que o override passa.

**Resumo**: engine vai evoluindo; a tela ndnm é o benchmark. Alto-prio restante: `gradients`, `transform`, `overflow`, `@font-face`.
