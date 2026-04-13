//! Runtime de rotas tipadas para aplicações Den.

/// Contrato base para páginas Den navegáveis.
pub trait DenPage<Route>: Sized {
    /// Retorna o nome estático da página usado em declarações de rota.
    fn page_name() -> &'static str;

    /// Tenta construir a página a partir da rota atual.
    fn from_route(route: &Route) -> Option<Self>;

    /// Renderiza a página usando egui.
    fn render(&mut self, ui: &mut egui::Ui, __den_scale: f32, __den_router: &mut DenRouter<Route>);
}

/// Router simples com rota atual e uma navegação pendente.
#[derive(Debug)]
pub struct DenRouter<Route> {
    current: Route,
    pending: Option<Route>,
}

impl<Route> DenRouter<Route> {
    /// Cria um router iniciando na rota informada.
    pub fn new(initial: Route) -> Self {
        Self {
            current: initial,
            pending: None,
        }
    }

    /// Agenda navegação para uma nova rota.
    pub fn goto(&mut self, route: Route) {
        self.pending = Some(route);
    }

    /// Retorna a rota ativa no momento.
    pub fn current(&self) -> &Route {
        &self.current
    }

    /// Aplica a navegação pendente, se existir.
    pub fn flush(&mut self) -> bool {
        if let Some(next) = self.pending.take() {
            self.current = next;
            true
        } else {
            false
        }
    }
}
