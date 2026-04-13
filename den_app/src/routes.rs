//! Configuração central de rotas e renderização das páginas do app demo.

use crate::models::Usuario;
use crate::node_editor::NodeEditorCanvas;
use crate::pages::{HelloPage, HomePage, NodesPage, UsuarioPage};
use den_layout::{DenPage, DenRouter};
use eframe::egui;

den_macros::den_router! {
    HomePage,
    NodesPage,
    UsuarioPage,
    HelloPage { usuario: Usuario },
    NodeEditor,
}

/// Retorna a rota inicial do app.
pub fn initial_route() -> AppRoute {
    AppRoute::HomePage
}

/// Retorna a próxima rota usada pelo atalho de demonstração.
pub fn demo_next_route(current: &AppRoute) -> AppRoute {
    let usuario = Usuario::demo();
    match current {
        AppRoute::HomePage => AppRoute::UsuarioPage,
        AppRoute::UsuarioPage => AppRoute::HelloPage { usuario },
        AppRoute::HelloPage { .. } => AppRoute::NodesPage,
        AppRoute::NodesPage => AppRoute::NodeEditor,
        AppRoute::NodeEditor => AppRoute::HomePage,
    }
}

/// Estado das páginas instanciadas pelo router.
pub struct AppPages {
    home: HomePage,
    nodes_page: NodesPage,
    usuario_page: Option<UsuarioPage>,
    hello_page: Option<HelloPage>,
    node_editor: NodeEditorCanvas,
}

impl AppPages {
    /// Cria o conjunto de páginas do app.
    pub fn new() -> Self {
        Self {
            home: HomePage::default(),
            nodes_page: NodesPage,
            usuario_page: None,
            hello_page: None,
            node_editor: NodeEditorCanvas::new(),
        }
    }

    /// Sincroniza páginas stateful quando uma rota nova entra.
    pub fn sync_from_route(&mut self, route: &AppRoute) {
        match route {
            AppRoute::UsuarioPage => {
                self.usuario_page = Some(UsuarioPage::new());
            }
            AppRoute::HelloPage { usuario } => {
                self.hello_page = HelloPage::from_route(&AppRoute::HelloPage {
                    usuario: usuario.clone(),
                });
            }
            _ => {}
        }
    }

    /// Renderiza a página correspondente à rota atual.
    pub fn render_current(
        &mut self,
        ui: &mut egui::Ui,
        scale: f32,
        router: &mut DenRouter<AppRoute>,
    ) {
        let current = router.current().clone();
        match current {
            AppRoute::HomePage => {
                self.home.render(ui, scale, router);
            }
            AppRoute::NodesPage => {
                self.nodes_page.render(ui, scale, router);
            }
            AppRoute::UsuarioPage => {
                if self.usuario_page.is_none() {
                    self.usuario_page = Some(UsuarioPage::new());
                }
                if let Some(page) = &mut self.usuario_page {
                    page.render(ui, scale, router);
                }
            }
            AppRoute::HelloPage { usuario } => {
                if self.hello_page.is_none() {
                    self.hello_page = HelloPage::from_route(&AppRoute::HelloPage { usuario });
                }
                if let Some(page) = &mut self.hello_page {
                    page.render(ui, scale, router);
                }
            }
            AppRoute::NodeEditor => {
                self.node_editor.render(ui);
            }
        }
    }

    /// Retorna o zoom atual do editor de nodes.
    pub fn node_editor_scale(&self) -> f32 {
        self.node_editor.scale
    }

    /// Atualiza o zoom do editor de nodes.
    pub fn set_node_editor_scale(&mut self, scale: f32) {
        self.node_editor.scale = scale;
    }

    /// Reseta o pan do editor de nodes.
    pub fn reset_node_editor_pan(&mut self) {
        self.node_editor.pan_offset = egui::Vec2::ZERO;
    }
}

impl Default for AppPages {
    fn default() -> Self {
        Self::new()
    }
}
