//! Modelos usados pelo app demo.

/// Usuário de exemplo usado nas rotas tipadas.
#[derive(Debug, Clone)]
pub struct Usuario {
    pub nome: String,
}

impl Usuario {
    /// Cria um usuário vazio para preenchimento em formulário.
    pub fn vazio() -> Self {
        Self {
            nome: String::new(),
        }
    }

    /// Cria o usuário padrão usado no fluxo demo.
    pub fn demo() -> Self {
        Self {
            nome: "Vini".to_string(),
        }
    }
}
