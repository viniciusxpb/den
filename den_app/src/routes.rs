//! Configuração central de rotas e renderização das páginas do app demo.

use crate::models::Usuario;
use crate::pages::{Footer, HelloPage, HomePage, NdnmPage, NodesPage, PostLogsPage, UsuarioPage};

den_macros::den_router! {
    NodesPage,
    HomePage,
    NdnmPage,
    UsuarioPage,
    HelloPage { usuario: Usuario },
    PostLogsPage,
    Footer,
}
