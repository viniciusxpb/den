//! GhostService™ — async sem async, mocks sem config.
//!
//! Um "fantasma" é um valor default de um tipo com a flag `loading: true`. Ele
//! aparece na tela enquanto o GhostService busca o dado real em background.
//! Quando a resposta chega, o ghost vira real: mesmos campos, valores
//! preenchidos, `loading: false`. Sem await, sem spawn, sem channels expostos.
//!
//! Para o dev: o template só consulta `self.usuario.loading` ou `self.usuario.nome`
//! normalmente. O polling acontece dentro do `tick()` que é chamado antes do
//! render a cada frame.
//!
//! ## Exemplo
//!
//! ```ignore
//! use den_layout::{DenGhost, DenGhostService};
//!
//! #[derive(DenGhost, Clone)]
//! struct Usuario {
//!     #[ghost("João")]
//!     nome: String,
//!     #[ghost("DasIdeia")]
//!     sobrenome: String,
//! }
//!
//! struct MinhaPage {
//!     usuario: DenGhostService<Usuario>,
//! }
//!
//! impl MinhaPage {
//!     fn new() -> Self {
//!         Self { usuario: DenGhostService::new() }
//!     }
//!
//!     fn fetch(&mut self) {
//!         self.usuario.fetch(|| {
//!             // Em prod: request HTTP real. Em dev: retorna o ghost.
//!             std::thread::sleep(std::time::Duration::from_secs(2));
//!             Usuario { nome: "Ana".into(), sobrenome: "Silva".into() }
//!         });
//!     }
//! }
//! ```

use std::any::Any;
use std::sync::mpsc::{Receiver, TryRecvError, channel};

/// Extrai a mensagem de panic do payload do `catch_unwind`. Cobre os dois casos
/// padrão (`panic!("msg")` vira `&'static str`; `panic!("{x}")` vira `String`)
/// e cai num placeholder genérico pra payloads exóticos.
fn panic_msg(payload: Box<dyn Any + Send>) -> String {
    if let Some(s) = payload.downcast_ref::<String>() {
        return s.clone();
    }
    if let Some(s) = payload.downcast_ref::<&'static str>() {
        return (*s).to_string();
    }
    "unknown panic payload".to_string()
}

/// Contrato do tipo que vira um ghost: fornece um valor default "mockado" pra
/// renderização inicial enquanto a resposta real não chega.
///
/// Implementado via `#[derive(DenGhost)]`. Cada campo usa `#[ghost("valor")]`
/// pra customizar o mock, ou `Default::default()` quando ausente.
pub trait DenGhost {
    /// Retorna o valor "fantasma" do tipo — um mock renderizável no primeiro frame.
    fn ghost() -> Self;
}

/// Estado de erro do último fetch. `None` = sem erro.
#[derive(Debug, Clone)]
pub enum GhostError {
    /// A closure do fetch entrou em panic. `tick()` setou `loading=false` e salvou
    /// a mensagem original do panic aqui (downcast de `&str`/`String`). Quando o
    /// downcast falha, vira `"unknown panic payload"`.
    FetchPanicked(String),
    /// O channel desconectou antes de enviar um valor — thread morreu sem completar
    /// o `catch_unwind` (raro, mas possível em casos de OOM ou abort externo).
    Disconnected,
}

/// Container reativo de um valor assíncrono. Começa em `loading: true` com um
/// ghost; quando o fetch retorna, vira o valor real.
///
/// A tela renderiza normalmente — o template checa `.loading` se quiser skeleton
/// ou acessa os campos do wrapped diretamente (através do `Deref`).
///
/// Em caso de panic ou thread morta, `loading` vira `false` e `error()` retorna
/// `Some(GhostError)` pro template poder renderizar fallback.
pub struct DenGhostService<T: DenGhost + Send + 'static> {
    value: T,
    pub loading: bool,
    rx: Option<Receiver<Result<T, String>>>,
    error: Option<GhostError>,
}

impl<T: DenGhost + Send + 'static> DenGhostService<T> {
    /// Cria o service em estado loading com o ghost inicial.
    pub fn new() -> Self {
        Self {
            value: T::ghost(),
            loading: true,
            rx: None,
            error: None,
        }
    }

    /// Dispara um fetch em background. `f` roda numa thread separada; o resultado
    /// vira o novo valor no próximo `tick()` que detectar a resposta.
    ///
    /// Chamar `fetch` de novo antes de completar cancela efetivamente o anterior
    /// (descarta o receiver). Se a closure entrar em panic, o `catch_unwind`
    /// captura o payload, extrai a mensagem original (`&str`/`String`) e envia
    /// como `Err(msg)`; o próximo `tick()` vira isso em `FetchPanicked(msg)`.
    pub fn fetch<F>(&mut self, f: F)
    where
        F: FnOnce() -> T + Send + 'static,
    {
        let (tx, rx) = channel::<Result<T, String>>();
        std::thread::spawn(move || {
            let payload = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
                Ok(val) => Ok(val),
                Err(panic) => Err(panic_msg(panic)),
            };
            let _ = tx.send(payload);
        });
        self.rx = Some(rx);
        self.loading = true;
        self.error = None;
    }

    /// Polling não-bloqueante. Chame antes do render a cada frame.
    /// Retorna `true` se o estado mudou (valor real chegou OU erro detectado).
    pub fn tick(&mut self) -> bool {
        let Some(rx) = &self.rx else {
            return false;
        };
        match rx.try_recv() {
            Ok(Ok(val)) => {
                self.value = val;
                self.loading = false;
                self.error = None;
                self.rx = None;
                true
            }
            Ok(Err(msg)) => {
                self.loading = false;
                self.error = Some(GhostError::FetchPanicked(msg));
                self.rx = None;
                true
            }
            Err(TryRecvError::Empty) => false,
            Err(TryRecvError::Disconnected) => {
                eprintln!(
                    "DenGhostService: channel desconectado sem resposta \
                     (thread morreu antes de catch_unwind retornar)"
                );
                self.loading = false;
                self.error = Some(GhostError::Disconnected);
                self.rx = None;
                true
            }
        }
    }

    /// Substitui o valor imediatamente (útil para tests / atalho sem async).
    pub fn set(&mut self, value: T) {
        self.value = value;
        self.loading = false;
        self.error = None;
        self.rx = None;
    }

    /// Acesso imutável ao valor atual (ghost ou real).
    pub fn get(&self) -> &T {
        &self.value
    }

    /// Acesso mutável ao valor atual.
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Estado de erro do último fetch (panic ou disconnect), ou `None`.
    pub fn error(&self) -> Option<&GhostError> {
        self.error.as_ref()
    }
}

impl<T: DenGhost + Send + 'static> Default for DenGhostService<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: DenGhost + Send + 'static> std::ops::Deref for DenGhostService<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T: DenGhost + Send + 'static> std::ops::DerefMut for DenGhostService<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, PartialEq, Debug)]
    struct Fake {
        nome: String,
    }

    impl DenGhost for Fake {
        fn ghost() -> Self {
            Fake {
                nome: "ghost".into(),
            }
        }
    }

    #[test]
    fn starts_loading_with_ghost_value() {
        let svc: DenGhostService<Fake> = DenGhostService::new();
        assert!(svc.loading);
        assert_eq!(svc.nome, "ghost");
    }

    #[test]
    fn set_turns_ghost_into_real() {
        let mut svc: DenGhostService<Fake> = DenGhostService::new();
        svc.set(Fake {
            nome: "real".into(),
        });
        assert!(!svc.loading);
        assert_eq!(svc.nome, "real");
    }

    #[test]
    fn fetch_resolves_via_tick() {
        let mut svc: DenGhostService<Fake> = DenGhostService::new();
        svc.fetch(|| Fake {
            nome: "async".into(),
        });
        for _ in 0..200 {
            if svc.tick() {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
        }
        assert!(!svc.loading);
        assert_eq!(svc.nome, "async");
        assert!(svc.error().is_none());
    }

    #[test]
    fn fetch_panic_surfaces_as_error_and_clears_loading() {
        let mut svc: DenGhostService<Fake> = DenGhostService::new();
        svc.fetch(|| -> Fake { panic!("fake panic") });
        for _ in 0..400 {
            if svc.tick() {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
        }
        assert!(!svc.loading, "panic deve zerar loading em vez de girar pra sempre");
        match svc.error() {
            Some(GhostError::FetchPanicked(msg)) => {
                assert_eq!(msg, "fake panic", "mensagem original do panic deve ser preservada");
            }
            other => panic!("esperado FetchPanicked, got {other:?}"),
        }
    }

    #[test]
    fn fetch_panic_with_formatted_message_preserves_full_string() {
        let mut svc: DenGhostService<Fake> = DenGhostService::new();
        svc.fetch(|| -> Fake { panic!("erro {}: {}", 42, "boom") });
        for _ in 0..400 {
            if svc.tick() {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
        }
        match svc.error() {
            Some(GhostError::FetchPanicked(msg)) => {
                assert_eq!(msg, "erro 42: boom");
            }
            other => panic!("esperado FetchPanicked com mensagem formatada, got {other:?}"),
        }
    }
}
