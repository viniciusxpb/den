# Pendências

Itens intencionalmente deixados pra depois. Apaga quando resolver.

---

## Click handlers com argumentos dentro de `<for>`

O renderer genérico usa tabela de slots por template: cada `(click)="f()"` vira um `Interact::click_handler: Some(slot)`, e o match de dispatch roteia `PaintEvent::Click{handler:slot} → self.f()`. Funciona pra handlers SEM args.

Com args, hoje o codegen retorna erro explícito. O bloqueio: args dentro de `<for>` referenciam variáveis do escopo do loop (`user.id`, `idx`), que não existem no ponto de dispatch (fora da closure de build).

**Fix futuro**: dispatch via node_id em vez de slot. Cada nó clicável guarda sua ação como closure clonada em runtime (ou o dispatch re-executa o build com lookup por node_id). Alternativa: gerar uma tabela `HashMap<DenNodeId, Box<dyn FnOnce(&mut Self)>>` preenchida no build; o dispatch lê do map por node_id.

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

`EGUI_WINDOW_WIDTH` em `preview.rs` continua sendo o encaixe atual, mas a decisão de API (`<panel>`, atributo no template, ou configuração de app) ainda precisa ser tomada.

---

## Style editor resolve variáveis SCSS em literal ao escrever de volta

Quando o usuário edita um valor no style editor (ex: slider de cor numa propriedade `color: $primary`), o editor resolve `$primary` → `#0f3460` pra exibir o color picker, mas ao escrever de volta emite o valor literal (`#0f3460`) em vez de preservar a referência (`$primary`).

Isso é um comportamento intencional por ora (o usuário pode intencionalmente querer "desconectar" do token), mas também apaga variáveis sem aviso quando o usuário apenas arrasta e solta no mesmo valor.

**Fix futuro**: comparar `to_scss_string()` com o valor original do arquivo antes de emitir. Se o resultado resolvido for igual ao original resolvido, manter a string original (com variável). Só substituir quando o valor mudou de fato.

---

## Extração pra `den_core` (elimina parsers duplicados)

Os parsers HTML/SCSS estão duplicados entre `den_macros`, `preview.rs` e `style_editor.rs`. Criar um crate `den_core` com parsers + types compartilhados eliminaria a triplicação de `collect_scss_vars`, `vars_by_longest_name` e helpers de HTML.

**Decisão pendente**: definir se `den_core` deve ser uma crate pública do workspace (API reutilizável por apps) ou apenas uma crate interna para dividir responsabilidades entre macro, preview e style editor.

---

## Registro nativo de fontes `@font-face`

O parser já transporta propriedades textuais vindas do CSS (`font-family`, `font`, `font-weight`, `font-style`, `line-height`, `letter-spacing`, `text-transform`, `text-align`, `text-decoration`) até o `PaintStyle`, e o painter mede/pinta usando uma TextBox antes do layout final.

O preview HTML já copia URLs relativas de fontes declaradas em `@font-face` para `preview/fonts/` e deixa o browser resolver a família como CSS normal. No backend egui nativo, porém, ainda falta registrar automaticamente os bytes dessas fontes em `egui::FontDefinitions`.

**Decisão pendente**: escolher onde vive o registro de fontes:
1. o macro coleta `@font-face` no SCSS e gera uma função/constante de assets por página;
2. um `den_core` compartilhado coleta fontes e o app chama um bootstrap único antes de renderizar;
3. o app declara manualmente um mapa de fontes, e o CSS só referencia nomes já registrados.

Também falta decidir como mapear peso/estilo para faces reais (`font-weight: 700`, itálico, variável). O egui 0.33 não seleciona peso automaticamente só pelo `TextFormat`, então o Den precisa escolher uma política própria.

---

## Política para `display: grid`

Hoje o parser aceita `display: grid`, mas `den_layout::LayoutTable` trata `Grid` como fluxo block (`table.rs:69`). Isso é útil para experimentação, porém pode induzir usuário a achar que grid real já existe.

**Decisão pendente**: escolher entre:
1. gerar erro/aviso forte para `display: grid` até existir grid real;
2. manter fallback para block e documentar como experimental;
3. implementar um grid mínimo no layout engine (mínimo viável: `grid-template-columns` com `fr` + `px` + `%`, necessário para a sidebar da tela ndnm).

---

## Modelo avançado de input

O input atual é pintado manualmente e cobre foco, caret, texto, backspace/delete, setas, home/end e blur. Ainda faltam seleção, clipboard, tab focus, mouse positioning e composição IME.

**Decisão pendente**: continuar evoluindo o input manual para preservar o painter puro do Den, ou introduzir uma ponte controlada para widgets egui nativos em inputs complexos.

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

O `paint_tree` mora em `den_app/src/den_paint.rs` como função concreta. Pra trocar backend (iced, wgpu direto, canvas web), seria preciso:
1. Definir o alias `DenUi` no app target com o tipo adequado (já existe esse hook).
2. Implementar um `paint_tree` equivalente que aceite a `RenderTree` + `LayoutTable` e desenhe no backend escolhido.
3. O macro gera `crate::den_paint::paint_tree(...)` — trocar o módulo no crate root troca o backend.

Nenhum código de `den_layout` / `den_macros` precisa mudar.

---

## Gap visual entre o runtime nativo e a tela React (página `ndnm`)

O HTML/CSS da tela ndnm já está dentro do projeto (`den_app/src/pages/ndnm/`) e a rota inicial já aponta pra ela. O preview no browser consegue reproduzir a tela porque delega tudo ao CSS real; o runtime Den nativo, porém, ignora a maioria das propriedades que essa tela depende. Lista do que falta pra paridade visual:

**Posicionamento e transformações**
- `position: absolute`, `left`, `top`, `z-index` e posicionamento livre de nós no canvas. Hoje o CSS tá no arquivo, o browser usa, mas o runtime Den ignora — todo nó cai no fluxo block/flex.
- `transform: rotate(...)` pra desenhar wires como linhas inclinadas.
- `pointer-events`, `cursor` granular por região, drag, pan e zoom interativos no canvas.

**Gráficos vetoriais**
- SVG / canvas / path real pra wires Bezier. No HTML/CSS atual deixei wires como divs estáticas aproximadas.
- Ícones SVG do `lucide-react`. Por enquanto convertidos para marcadores textuais curtos tipo `DB`, `LY`, `SH`.

**Pintura avançada**
- `background-image`, `radial-gradient` e `repeating-linear-gradient` (usados em grid de pontos e scanline).
- `box-shadow`, glow e sombras coloridas em nós/ports.
- `opacity`, `rgba(...)`, `transparent` e alpha real nas cores.
- `overflow: hidden`/`visible`, clipping e scroll interno por nó.

**Layout**
- `flex-direction`, `align-items`, `justify-content`, `flex-shrink` e `min-width: 0` pra flex compatível com o que o browser faz.
- `grid-template-columns` (mínimo `fr` + `px` + `%`) pros stats da sidebar — conecta com a seção de `display: grid` acima.

**Tipografia e animação**
- Seletores/propriedades CSS mais completas: `border-left`, `border-top-width`, `background-color` (separado de `background`), `white-space`, `text-overflow`.
- Fontes externas / Google Fonts e seleção real por peso/estilo. Hoje uso uma stack mono compatível com o que já existe — conecta com a seção `@font-face` acima.
- Animações / `@keyframes` (pulse nos ports, motion nos wires).

**Resumo**: a estrutura e o CSS da tela já vivem no projeto. O gap é quase todo no motor visual/layout do runtime nativo — principalmente posicionamento absoluto, transforms, sombras/alpha e wires vetoriais.
