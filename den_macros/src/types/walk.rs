// ============================================================================
// DFS genérico — fonte única de verdade pra ordem de travessia
// ============================================================================

use super::resolved::*;

/// Caminha a árvore de `DenNode` em DFS pré-ordem, chamando `visitor` pra cada
/// `DenElement` encontrado. `ForLoop` e `IfChain` são transparentes: seus filhos
/// pertencem ao pai do control flow.
///
/// `counter` é incrementado pra cada `DenElement` visitado (layout_index).
///
/// Toda função que precise atribuir layout indices ou iterar elementos na mesma
/// ordem do codegen DEVE usar esta função. Isso garante que a ordem de travessia
/// é definida num único lugar.
pub fn walk_den_nodes<F>(
    nodes: &[DenNode],
    parent_index: usize,
    counter: &mut usize,
    visitor: &mut F,
)
where
    F: FnMut(&DenElement, usize, usize), // (element, my_index, parent_index)
{
    for node in nodes {
        match node {
            DenNode::Element(el) => {
                let idx = *counter;
                *counter += 1;
                visitor(el, idx, parent_index);
                walk_den_nodes(&el.children, idx, counter, visitor);
            }
            // ForLoop e IfChain são transparentes: parent_index não muda.
            DenNode::ForLoop(fl) => {
                walk_den_nodes(&fl.children, parent_index, counter, visitor);
            }
            DenNode::IfChain(ic) => {
                walk_den_nodes(&ic.then_children, parent_index, counter, visitor);
                walk_den_nodes(&ic.else_children, parent_index, counter, visitor);
            }
        }
    }
}
