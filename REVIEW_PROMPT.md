# Code Review — Den Framework

## O que é o Den

Den é um framework Rust que compila templates HTML + SCSS em código egui nativo em compile time via proc macros. O dev escreve HTML pra estrutura, SCSS pra estilo, e Rust pra lógica. Cada coisa no seu arquivo. Em compile time, a proc macro `den_template!` lê os templates e gera código egui puro. Zero parsing em runtime.

```
  .html + .scss  ───► compile time ───► código egui nativo ───► desktop app
```

O workspace tem 3 crates:

- **`den_macros`** — proc macro. Pipeline de 3 fases: parse (HTML+SCSS) → resolve (merge styles) → codegen (emite Rust/egui via `quote!`).
- **`den_layout`** — runtime. Resolução iterativa de larguras (LayoutTable).
- **`den_app`** — aplicação demo. Páginas com templates, node editor visual, e dev tools (preview HTML, style editor).

## Regras inegociáveis

### 1. Tudo passa pelo framework HTML + SCSS + Rust

O Den existe pra não ter que escrever egui na mão. Qualquer componente visual dentro de `den_app` DEVE ser construído usando o pipeline Den:

- Estrutura → arquivo `.html`
- Estilo → arquivo `.scss`
- Lógica → arquivo `.rs` chamando `den_template!("caminho/do/template", self)`

Se encontrar código que usa `egui::Painter` direto (`painter.rect_filled`, `painter.circle_stroke`, `painter.text`, etc.) pra renderizar UI dentro do `den_app`, isso é uma violação. O framework foi criado exatamente pra substituir isso.

**Exceção**: código utilitário do próprio framework (dentro de `den_macros` e `den_layout`) não usa templates porque é o framework.

### 2. Toda função deve ter documentação

- Toda função `pub` ou `pub(super)` DEVE ter `///` doc comment.
- Todo arquivo de módulo DEVE ter `//!` module-level doc comment.
- Doc comments em português (convenção do projeto).
- Se a função é óbvia demais pra documentar, pelo menos uma linha descritiva. Sem exceção.

### 3. Clippy limpo

- `cargo clippy` DEVE passar sem warnings.
- Nenhum `#[allow(clippy::...)]` novo sem justificativa escrita em comentário no código.
- Os `#[allow]` existentes já têm justificativa (ex: `module_inception` em `pages/home/mod.rs` é intencional pela estrutura de pastas).

### 4. Sem magic numbers

- Valores numéricos literais em código de renderização devem ser constantes nomeadas.
- O `theme.rs` do node editor é o padrão a seguir: 60+ constantes com nomes descritivos e doc comments.
- Sliders, tamanhos de UI, offsets, timeouts — tudo nomeado.

### 5. Sem `unwrap()` injustificado

- Todo `.unwrap()` deve ter um comentário explicando por que é seguro, ou ser substituído por tratamento de erro adequado.
- `.unwrap_or_default()` que silencia erros de I/O é igualmente problemático — pelo menos um `eprintln!` no caminho de erro.

### 6. ⚠️ Toda propriedade CSS é `Option<T>` em `StyleRule` e `DenVisual`

**A regra mais quebrada do projeto.** Já foi violada 4 vezes (`display`, `position` no `DenVisual`, `flex_direction`, `align_items`, `justify_content`, e os bools `cursor_pointer`/`flex_grow`). Doc completo em [`den_macros/src/types/style.rs`](den_macros/src/types/style.rs) topo do arquivo.

**Regra:** propriedades CSS sobreescrevíveis (qualquer cosa que aparece em `StyleRule` ou `DenVisual`) DEVEM ser `Option<T>`, **inclusive** enums com Default e `bool`s. O default só é aplicado **uma vez**, no codegen, via `.unwrap_or_default()`.

**Por quê:** se for `T` direto, `merge_from` precisa comparar com default pra decidir se sobreescreve, e isso quebra cascade: `:hover` que volta pro default explicitamente é silenciosamente descartado.

```scss
.col          { display: flex; flex-direction: column; }
.col:hover    { flex-direction: row; }    /* row é o default — IGNORADO se enum direto */
```

**Procura por:**
- Campo novo de `StyleRule`/`DenVisual` que não é `Option<T>`.
- `merge_from` com `if other.x != Default::default()` ou `if other.x != EnumKind::Variant` — bug garantido.
- `if other.x { ... }` com `other.x: bool` em merge — não dá pra unsetar.

Severidade: **CRÍTICO** sempre, mesmo que o teste não pegue (cascade só falha em condições específicas que normalmente não estão cobertas).

## O que revisar

Faça code review do diff/código fornecido verificando:

1. **Componentes visuais escritos em egui manual em vez de usar o framework Den** — isso é o problema mais grave.
2. **Funções sem doc comment** (`///` pra pub, `//!` pra módulos).
3. **Warnings do clippy** — rode mentalmente ou aponte possíveis issues.
4. **Magic numbers** — literais numéricos que deveriam ser constantes nomeadas.
5. **`unwrap()` sem justificativa** — panic silencioso em produção.
6. **Nomes de variáveis genéricos** — `x`, `val`, `tmp` sem contexto.
7. **Arquivos monolíticos** — módulos acima de ~300 linhas devem ser avaliados pra split.
8. **Documentação desatualizada** — README.md, CLAUDE.md, PENDING.md devem refletir o estado real do código.
9. **⚠️ Propriedades CSS que NÃO são `Option<T>` em `StyleRule`/`DenVisual`** — leia regra 6 acima. Bug latente garantido em cascade/hover.

## Contexto adicional

- Variáveis geradas pelo framework são prefixadas com `__den_` pra evitar colisão com código do usuário.
- O parâmetro `__den_scale: f32` é hardcoded no código gerado — render methods DEVEM usar esse nome exato.
- `walk_den_nodes()` em `types/walk.rs` é a fonte única de verdade pra ordem DFS. Qualquer função que atribua layout indices DEVE usar ela.
- `StyleRule::merge_from` e `DenVisual::merge_from` são espelhos — atualizar um sem o outro é bug.
- Comentários e docs em português. Código (nomes de variáveis, funções, tipos) em inglês.

## Formato da review

Pra cada issue encontrada:

1. **Arquivo e linha** (ou região)
2. **Severidade**: crítico / médio / baixo / sugestão
3. **O que está errado**
4. **Como corrigir** (com código se aplicável)

Não precisa elogiar o que tá bom. Foca nos problemas.

---

Abaixo segue o código alterado para review:
