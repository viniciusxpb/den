//! Modelos usados pelo app demo.

use den_macros::DenGhost;

/// Usuário de exemplo usado nas rotas tipadas.
///
/// `DenGhost` permite que o GhostService mostre valores "João DasIdeia, 30 anos"
/// enquanto a resposta real não chega, zerando a fricção de desenvolvimento
/// front sem backend.
#[derive(Debug, Clone, DenGhost)]
pub struct Usuario {
    #[ghost("João")]
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

/// Cat fact da API pública `https://catfact.ninja/fact` — primeiro consumer
/// real do `GhostService`.
///
/// O `#[ghost("...")]` injeta um fato fake no boot pra que a UI tenha
/// conteúdo no primeiro frame, antes do request HTTP completar. Quando o
/// `fetch` retorna, o ghost é substituído pelo fato real. Ver
/// `HomePage::fetch_cat` pro lado HTTP.
#[derive(Debug, Clone, DenGhost)]
pub struct CatFact {
    #[ghost("Cats sleep 70% of their lives.")]
    pub fact: String,
    #[ghost(40)]
    pub length: u32,
}
