//! Modelos usados pelo app demo.

/// Usuário de exemplo usado nas rotas tipadas.
#[derive(Debug, Clone)]
pub struct Usuario {
    pub nome: String,
}

impl Usuario {
    /// Cria o usuário padrão usado no fluxo demo.
    pub fn demo() -> Self {
        Self {
            nome: "Vini".to_string(),
        }
    }
}
