//! Estado runtime mantido por rota durante a renderização Den.

use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
};

/// Variável de ambiente que habilita dumps de estado por rota.
const ROUTE_STATE_DEBUG_ENV: &str = "DEN_DEBUG_ROUTE_STATE";

/// Valor textual que liga o debug quando usado na variável de ambiente.
const ROUTE_STATE_DEBUG_ON: &str = "1";

/// Capacidade inicial para inputs controlados por uma rota.
const DEFAULT_INPUT_STATE_CAPACITY: usize = 4;

/// Identificador estável de um nó Den durante o runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DenNodeId(u64);

impl DenNodeId {
    /// Identificador reservado para o body/root invisível.
    pub const ROOT: Self = Self(0);

    /// Cria um identificador de nó a partir do valor bruto gerado pelo framework.
    pub const fn new(raw: u64) -> Self {
        Self(raw)
    }

    /// Retorna o valor numérico bruto deste identificador.
    pub const fn raw(self) -> u64 {
        self.0
    }
}

/// Estado de inputs renderizados por uma rota Den.
#[derive(Debug, Clone)]
pub struct DenInputState {
    values: HashMap<DenNodeId, String>,
}

impl DenInputState {
    /// Cria um estado vazio para inputs de uma rota.
    pub fn new() -> Self {
        Self {
            values: HashMap::with_capacity(DEFAULT_INPUT_STATE_CAPACITY),
        }
    }

    /// Retorna o valor armazenado para um input, quando existir.
    pub fn get(&self, node_id: DenNodeId) -> Option<&str> {
        self.values.get(&node_id).map(String::as_str)
    }

    /// Atualiza o valor armazenado para um input.
    pub fn set(&mut self, node_id: DenNodeId, value: impl Into<String>) {
        self.values.insert(node_id, value.into());
    }

    /// Remove o valor armazenado para um input.
    pub fn remove(&mut self, node_id: DenNodeId) -> Option<String> {
        self.values.remove(&node_id)
    }

    /// Remove todos os valores de input desta rota.
    pub fn clear(&mut self) {
        self.values.clear();
    }

    /// Retorna quantos inputs possuem valor armazenado.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Retorna se nenhum input possui valor armazenado.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

impl Default for DenInputState {
    fn default() -> Self {
        Self::new()
    }
}

/// Estado de debug associado a uma rota Den.
#[derive(Debug, Clone)]
pub struct DenDebugState {
    enabled: bool,
    dumped_pages: HashSet<String>,
}

impl DenDebugState {
    /// Cria um estado de debug respeitando a variável de ambiente do Den.
    pub fn new() -> Self {
        Self {
            enabled: route_state_debug_enabled(),
            dumped_pages: HashSet::new(),
        }
    }

    /// Retorna se o dump de estado está habilitado.
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// Habilita ou desabilita o dump de estado em runtime.
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Registra e emite um dump único para uma página.
    pub fn dump_once(&mut self, page_name: &str, input_count: usize) {
        if !self.enabled {
            return;
        }

        if self.dumped_pages.insert(page_name.to_string()) {
            eprintln!("DenRouteState[{page_name}]: inputs={input_count}");
        }
    }

    /// Esquece quais páginas já tiveram dump emitido.
    pub fn reset_dumps(&mut self) {
        self.dumped_pages.clear();
    }
}

impl Default for DenDebugState {
    fn default() -> Self {
        Self::new()
    }
}

/// Estado runtime agregado para uma rota ativa.
#[derive(Debug, Clone, Default)]
pub struct DenRouteState {
    inputs: DenInputState,
    debug: DenDebugState,
    focus: Option<DenNodeId>,
    cursor: HashMap<DenNodeId, usize>,
    hover: HashSet<DenNodeId>,
}

impl DenRouteState {
    /// Cria um estado runtime vazio para uma rota.
    pub fn new() -> Self {
        Self::default()
    }

    /// Retorna o estado de inputs desta rota.
    pub fn inputs(&self) -> &DenInputState {
        &self.inputs
    }

    /// Retorna o estado mutável de inputs desta rota.
    pub fn inputs_mut(&mut self) -> &mut DenInputState {
        &mut self.inputs
    }

    /// Retorna o estado de debug desta rota.
    pub fn debug(&self) -> &DenDebugState {
        &self.debug
    }

    /// Retorna o estado mutável de debug desta rota.
    pub fn debug_mut(&mut self) -> &mut DenDebugState {
        &mut self.debug
    }

    /// Nó focado atualmente (tipicamente um input).
    pub fn focus(&self) -> Option<DenNodeId> {
        self.focus
    }

    /// Define o nó focado. Passar `None` limpa o foco.
    pub fn set_focus(&mut self, node: Option<DenNodeId>) {
        self.focus = node;
    }

    /// Posição do caret em bytes para o input informado.
    pub fn cursor_of(&self, node: DenNodeId) -> Option<usize> {
        self.cursor.get(&node).copied()
    }

    /// Define a posição do caret em bytes para o input informado.
    pub fn set_cursor(&mut self, node: DenNodeId, byte_offset: usize) {
        self.cursor.insert(node, byte_offset);
    }

    /// Esquece a posição do caret deste input (ex.: quando perde o foco).
    pub fn clear_cursor(&mut self, node: DenNodeId) {
        self.cursor.remove(&node);
    }

    /// Conjunto de nós considerados em hover neste frame.
    pub fn hover(&self) -> &HashSet<DenNodeId> {
        &self.hover
    }

    /// Acesso mutável ao conjunto de hover — o painter popula a cada frame.
    pub fn hover_mut(&mut self) -> &mut HashSet<DenNodeId> {
        &mut self.hover
    }

    /// Limpa dados voláteis mantidos por esta rota.
    pub fn clear(&mut self) {
        self.inputs.clear();
        self.debug.reset_dumps();
        self.focus = None;
        self.cursor.clear();
        self.hover.clear();
    }

    /// Emite um dump único do estado desta rota quando debug estiver habilitado.
    pub fn dump_once(&mut self, page_name: &str) {
        self.debug.dump_once(page_name, self.inputs.len());
    }
}

/// Retorna se o debug de estado por rota está habilitado no ambiente.
fn route_state_debug_enabled() -> bool {
    static CACHED: OnceLock<bool> = OnceLock::new();

    *CACHED.get_or_init(|| match std::env::var(ROUTE_STATE_DEBUG_ENV) {
        Ok(value) => value == ROUTE_STATE_DEBUG_ON || value.eq_ignore_ascii_case("true"),
        Err(std::env::VarError::NotPresent) => false,
        Err(err) => {
            eprintln!("Den: falha ao ler {ROUTE_STATE_DEBUG_ENV}: {err}");
            false
        }
    })
}
