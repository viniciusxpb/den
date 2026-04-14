//! Configuração central de rotas e renderização das páginas do app demo.

use crate::models::Usuario;
use crate::pages::{HelloPage, HomePage, NdnmPage, NodesPage, UsuarioPage};

den_macros::den_router! {
    NdnmPage,
    HomePage,
    NodesPage,
    UsuarioPage,
    HelloPage { usuario: Usuario },
}
