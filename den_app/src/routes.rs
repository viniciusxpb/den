//! Configuração central de rotas e renderização das páginas do app demo.

use crate::models::Usuario;
use crate::pages::{HelloPage, HomePage, NodesPage, UsuarioPage};

den_macros::den_router! {
    HomePage,
    NodesPage,
    UsuarioPage,
    HelloPage { usuario: Usuario },
}
