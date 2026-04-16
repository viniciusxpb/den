//! Configuração do painter egui do Den.

/// Tamanho mínimo de fonte antes de desenhar, evitando texto ilegível.
pub(crate) const MIN_FONT_SIZE_PX: f32 = 6.0;

/// Largura mínima de borda em pixels de tela, evitando bordas invisíveis.
pub(crate) const MIN_BORDER_WIDTH_PX: f32 = 1.0;

/// Padding horizontal interno dos inputs, em CSS pixels.
pub(crate) const INPUT_TEXT_PADDING_X: f32 = 6.0;

/// Padding vertical interno dos inputs, em CSS pixels.
pub(crate) const INPUT_TEXT_PADDING_Y: f32 = 4.0;

/// Número de retângulos concêntricos usados pra simular o `blur` de `box-shadow`.
/// egui não tem blur nativo (sem GPU shader); aproximamos com N rects de alpha
/// decrescente. Mais alto = blur mais suave + mais draws por shadow por frame.
/// 6 dá um halo aceitável sem custo perceptível.
pub(crate) const SHADOW_BLUR_SAMPLES: usize = 6;

/// Fator de escala do alpha em cada layer do blur. Layer 0 = fator 1, layer N =
/// fator (1 - N * decay) clamped 0..1. Maior decay = halo mais agressivo nas
/// bordas; menor = halo mais espalhado e fraco.
pub(crate) const SHADOW_BLUR_ALPHA_DECAY: f32 = 0.18;

/// Largura mínima (em CSS pixels) do stroke usado pra simular `box-shadow inset`.
/// O `spread` da sombra define a largura "natural" do anel interno; quando o spread
/// declarado é zero, este piso garante que algo seja visível em vez de sumir.
pub(crate) const MIN_INSET_SHADOW_SPREAD_PX: f32 = 1.0;
