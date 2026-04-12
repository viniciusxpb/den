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
