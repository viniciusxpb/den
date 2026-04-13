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
