# Pendências

Itens intencionalmente deixados pra depois. Apaga quando resolver.

---

## Renderer genérico baseado em árvore resolvida

O próximo passo arquitetural é reduzir o codegen direto de widgets egui por elemento e caminhar para um renderer genérico: HTML + SCSS vira uma árvore Den resolvida, com retângulos calculados pelo motor de layout, e o frontend egui apenas renderiza essa árvore.

O `den_layout` já foi separado em módulos de motor (`dimension`, `spacing`, `display`, `flex`, `entry`, `table`) e não depende mais diretamente de `egui`. O glue gerado por `den_router!`/`#[den_page]` também usa `crate::DenUi` em vez de citar egui diretamente.

O acoplamento que ainda resta está no `den_template!`: `codegen/egui_backend.rs` concentra os tokens egui, enquanto `codegen/element.rs` e `codegen/input.rs` ainda decidem qual widget/render path usar por elemento. O próximo corte é fazer o macro gerar uma árvore Den resolvida e mover essas decisões para um renderer egui separado.

`DenRouteState` já existe como ponto de encaixe runtime por rota. Ele ainda só guarda estado de inputs/debug, mas deve evoluir para carregar ou observar dados da árvore ativa quando a renderização for centralizada.

---

## Elemento raiz `<panel>` nos templates Den

Ideia: templates deveriam ter um elemento raiz explícito (`<panel>` ou similar) que mapeia pro `CentralPanel` no egui e pro container do preview no browser.

Hoje `home.html` começa direto com os filhos. O correto seria:
```html
<panel>
    <h1 class="title">...</h1>
    ...
</panel>
```

No egui, `<panel>` mapearia para `ScrollArea::vertical()` ou simplesmente o contexto do `CentralPanel`. No preview, seria um `<div>` que preenche a largura do browser.

Benefício: `width: 100%` em qualquer filho sempre resolve relativo ao painel pai, sincronizando preview e egui naturalmente — sem hardcodar largura.

`EGUI_WINDOW_WIDTH` em `preview.rs` está reservado pra quando isso for implementado.

---

## Style editor resolve variáveis SCSS em literal ao escrever de volta

Quando o usuário edita um valor no style editor (ex: slider de cor numa propriedade `color: $primary`), o editor resolve `$primary` → `#0f3460` pra exibir o color picker, mas ao escrever de volta emite o valor literal (`#0f3460`) em vez de preservar a referência (`$primary`).

Isso é um comportamento intencional por ora (o usuário pode intencionalmente querer "desconectar" do token), mas também apaga variáveis sem aviso quando o usuário apenas arrasta e solta no mesmo valor.

**Fix futuro**: comparar `to_scss_string()` com o valor original do arquivo antes de emitir. Se o resultado resolvido for igual ao original resolvido, manter a string original (com variável). Só substituir quando o valor mudou de fato.

---

## Borrow conflict: `<for>` + `(click)` com `&mut self`

`<for each="user" in="self.users">` gera `for (_, user) in self.users.iter()` que faz borrow imutável de `self`. Se o click handler dentro do loop chama `self.on_edit(...)` (borrow mutável), o borrow checker reclama.

O clone dos argumentos via `den-bind` resolve o conflito dos DADOS (o clone captura o valor antes do `&mut self`), mas NÃO resolve o conflito do ITERADOR — `self.users.iter()` mantém o borrow ativo durante todo o loop body.

Problema pré-existente: handlers sem args dentro de `<for>` (`(click)="toggle()"`) têm o mesmo conflito.

**Fix futuro**: coletar os items antes do loop quando o `<for>` contém `(click)`:

```rust
// Ao invés de:
for (idx, user) in self.users.iter().enumerate() { self.on_edit(...); }

// Gerar:
let __den_items: Vec<_> = self.users.iter().enumerate().collect();
for (idx, user) in __den_items { self.on_edit(...); }
```

O Vec temporário libera o borrow de `self.users` antes do loop body. Custo: uma alocação por frame. Alternativa: indexar por posição (`self.users[idx]`) ao invés de iterar por referência.

---

## `build_inner` com 7 parâmetros — codegen/element.rs

Tá no limite. Próxima propriedade CSS nova que exigir mais um parâmetro deve triggar a criação de `BuildInnerCtx`.
