//! Descoberta dos pares `.html` + `.scss` em `src/pages/`.

use std::fs;
use std::path::{Path, PathBuf};

/// Walk recursivo: pra cada `.html` que tem `.scss` irmão, devolve o par.
pub(super) fn find_template_pairs(dir: &Path) -> Vec<(PathBuf, PathBuf)> {
    let mut pairs = Vec::new();
    let Ok(entries) = fs::read_dir(dir) else {
        return pairs;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            pairs.extend(find_template_pairs(&path));
        } else if path.extension().is_some_and(|e| e == "html") {
            let scss = path.with_extension("scss");
            if scss.exists() {
                pairs.push((path, scss));
            }
        }
    }
    pairs
}
