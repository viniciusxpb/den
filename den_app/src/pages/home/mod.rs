// Silenciado: estrutura intencional do projeto — cada página tem sua pasta com home.rs, home.html e home.scss
#[allow(clippy::module_inception)]
mod home;
pub use home::HomePage;
