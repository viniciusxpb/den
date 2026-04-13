# Pendências

Itens intencionalmente deixados pra depois. Apaga quando resolver.

---

## `DenVisual::inheritable()` — types.rs:243

Método existe mas não é chamado ainda. Vai ser usado pelo sistema de scale/zoom.

Quando o scale chegar, o codegen vai ler `visual.font_size` e gerar:
```rust
// Hoje:
egui::RichText::new("texto").size(24.0)

// Com scale:
egui::RichText::new("texto").size(24.0 * __den_scale)
```

`__den_scale: f32` será passado via `DenContext` ou como variável local antes de chamar `den_template!`.
`inheritable()` propaga o scale pros filhos da mesma forma que `color` e `font-size` são propagados hoje.

**Warning atual**: `warning: method 'inheritable' is never used` em `den_macros`.

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

## `PortType` mistura direção e semântica — types.rs

`PortType` tem 4 variantes: `Exec`, `Data`, `Input`, `Output`. As primeiras duas são tipos de dados (o que flui pelo wire). As últimas duas são direções (por onde entra/sai). O snap check em `canvas.rs` faz `port_type == wd.port_type`, o que significa que um port `PortType::Input` só conecta com outro `PortType::Input` — semanticamente errado.

**Fix futuro**: separar em `direction: PortDirection { Input, Output }` + `data_type: PortDataType { Exec, Data }`. O snap check compararia só `data_type`, e `direction` determinaria o lado do node (esquerda/direita).

---

## Node IDs como String — types.rs

`NodeData::id`, `WireData::from_node`, etc. são `String`. Cada frame faz `.find(|n| n.id == ...)` com comparação de string. Com 4 nodes é irrelevante, mas escala mal.

**Fix futuro**: migrar pra `u64` ou `usize`. Eliminaria `.clone()` nos drag handlers e aceleraria hit tests.

---

## `build_inner` com 7 parâmetros — codegen/element.rs

Tá no limite. Próxima propriedade CSS nova que exigir mais um parâmetro deve triggar a criação de `BuildInnerCtx`.
