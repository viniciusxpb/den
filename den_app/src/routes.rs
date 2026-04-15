//! Configuração central de rotas e renderização das páginas do app demo.

use crate::models::Usuario;
use crate::pages::{Footer, HelloPage, HomePage, NdnmPage, UsuarioPage};

den_macros::den_router! {
    HomePage,
    NdnmPage,
    UsuarioPage,
    HelloPage { usuario: Usuario },
    Footer,
}
