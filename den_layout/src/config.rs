//! Constantes de configuração do motor de layout Den.
//!
//! Tudo que é valor fixo — variáveis de ambiente, capacities default, limites
//! de passes, geometria de lados — vive aqui. Ponto único de mudança.

// ─── Debug ────────────────────────────────────────────────────────────────

/// Variável de ambiente que habilita dump textual do layout resolvido.
/// Set pra `"1"` ou `"true"` pra ligar.
pub(crate) const LAYOUT_DEBUG_ENV: &str = "DEN_DEBUG_LAYOUT";

/// Variável de ambiente que habilita dumps de estado por rota (`DenRouteState`).
pub(crate) const ROUTE_STATE_DEBUG_ENV: &str = "DEN_DEBUG_ROUTE_STATE";

/// Valor textual que liga o debug quando usado nas variáveis de ambiente.
pub(crate) const DEBUG_ON: &str = "1";

// ─── Layout engine ────────────────────────────────────────────────────────

/// Número máximo de passes reservado para algoritmos iterativos futuros
/// (resolver larguras com dependência circular, etc.). Hoje usado só
/// como placeholder no `LayoutTable::new`.
pub(crate) const DEFAULT_MAX_PASSES: usize = 5;

/// Quantidade de lados de um eixo (topo+base ou esquerda+direita).
/// Usado pra converter valor uniforme (`padding: 10`) em extent total (`20`).
pub(crate) const SIDES_PER_AXIS: f32 = 2.0;

// ─── Route state ──────────────────────────────────────────────────────────

/// Capacidade inicial do `HashMap` de valores de input por rota.
/// 4 é suficiente pra maioria das telas sem forçar re-alocação.
pub(crate) const DEFAULT_INPUT_STATE_CAPACITY: usize = 4;
