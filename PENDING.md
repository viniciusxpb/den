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
