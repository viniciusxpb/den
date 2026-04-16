//! Pass de layout flex bi-direcional.
//!
//! Suporta `flex-direction: row | column`, `align-items: stretch | flex-start
//! | center | flex-end`, e `justify-content` em todas as 6 distribuições.
//!
//! Auto-children sem `flex-grow` empacotam no tamanho intrínseco; com `flex-grow`
//! dividem o espaço remanescente do eixo PRINCIPAL proporcionalmente. Filhos
//! `position: absolute|fixed` ficam de fora desta pass — vão pra
//! [`super::positioned`].
//!
//! ## Abstração main/cross
//!
//! O fluxo CSS de flex sempre opera em dois eixos: principal (`main`, onde
//! children são empilhados em sequência) e cruzado (`cross`, perpendicular).
//! Pra `flex-direction: row`, main = horizontal (width, x), cross = vertical
//! (height, y). Pra `column`, invertidos. Helpers locais (`main_size_of`,
//! `cross_size_of`, etc.) eliminam o `if direction == Row { x } else { y }`
//! repetido — o resto do código é axis-agnostic.
//!
//! Note: importamos `crate::flex` (helpers de cálculo escalar) com path
//! absoluto pra evitar ambiguidade com este submódulo.

use super::LayoutTable;
use crate::{
    AlignItems, BODY_INDEX, DimensionRule, FlexDirection, JustifyContent, LayoutEntry, LayoutRect,
    height, margin, width,
};

impl LayoutTable {
    /// Resolve filhos em fluxo flex (row ou column).
    pub(super) fn layout_flex_children(&mut self, parent_idx: usize) {
        let parent_rect = self.rects[parent_idx];
        let direction = self.entries[parent_idx].flex_direction;
        let align = self.entries[parent_idx].align_items;
        let justify = self.entries[parent_idx].justify_content;

        let padding = self.entries[parent_idx].padding;
        let border_top = self.entries[parent_idx].border_top();
        let border_left = self.entries[parent_idx].border_left();
        let border_x = self.entries[parent_idx].border_x_extent();
        let border_y = self.entries[parent_idx].border_y_extent();
        let gap = self.entries[parent_idx].gap;

        // Content box do pai (border-box menos padding e bordas).
        let content_x = parent_rect.x + padding + border_left;
        let content_y = parent_rect.y + padding + border_top;
        let content_width = (parent_rect.width - padding * 2.0 - border_x).max(0.0);
        let parent_content_height_for_pct = height::parent_content_height_for(
            self.entries[parent_idx].height_rule,
            parent_idx == BODY_INDEX,
            parent_rect.height,
            padding,
            border_y,
        );

        // Em row, main = width; em column, main = height (do content box).
        let (content_main, content_cross) = match direction {
            FlexDirection::Row => (content_width, parent_content_height_for_pct),
            FlexDirection::Column => (parent_content_height_for_pct, content_width),
        };

        let all_children = self.entries[parent_idx].children.clone();
        let in_flow: Vec<usize> = all_children
            .iter()
            .copied()
            .filter(|&c| !self.entries[c].position.is_out_of_flow())
            .collect();
        if in_flow.is_empty() {
            return;
        }

        // Distribuição do eixo principal: soma sizes fixos e contribuições de flex-grow.
        let gap_total = crate::flex::gap_total(gap, in_flow.len());
        let margin_total: f32 = in_flow
            .iter()
            .map(|&child_idx| margin::uniform_extent(self.entries[child_idx].margin))
            .sum();
        let mut fixed_main_total = 0.0;
        let mut grow_total = 0.0;

        for &child_idx in &in_flow {
            let grow = self.entries[child_idx].flex_grow;
            let child = &self.entries[child_idx];
            let main_rule = main_dim_rule(child, direction);
            if grow > 0.0 && main_rule == DimensionRule::Auto {
                grow_total += grow;
            } else if main_rule == DimensionRule::Auto {
                fixed_main_total += main_auto_leaf(child, direction);
            } else {
                fixed_main_total += main_resolve(child, content_main, direction);
            }
        }

        let remaining_main =
            (content_main - fixed_main_total - margin_total - gap_total).max(0.0);
        // Quando há grow, o remaining vira espaço pra distribuir entre growing children
        // (e justify-content recebe 0). Senão, justify-content distribui o remaining.
        let (justify_offset, justify_extra_gap) = if grow_total > 0.0 {
            (0.0, 0.0)
        } else {
            justify_content_distribution(justify, remaining_main, in_flow.len())
        };

        let mut cursor_main = main_start(content_x, content_y, direction) + justify_offset;
        let cross_start = cross_start_pos(content_x, content_y, direction);
        let mut max_cross = 0.0f32;

        for (pos, child_idx) in in_flow.iter().copied().enumerate() {
            let margin = self.entries[child_idx].margin;
            let grow = self.entries[child_idx].flex_grow;
            let child = &self.entries[child_idx];
            let main_rule = main_dim_rule(child, direction);

            // Resolve main size: flex-grow distribui remaining; senão, intrinsic ou fixo.
            let fixed_main = if main_rule == DimensionRule::Auto && grow == 0.0 {
                main_auto_leaf(child, direction)
            } else {
                main_resolve(child, content_main, direction)
            };
            let resolved_main = crate::flex::distribute_flex_width(
                main_rule,
                grow,
                fixed_main,
                remaining_main,
                grow_total,
            );

            // Resolve cross size: stretch preenche cross_size disponível;
            // senão, resolve normalmente (intrinsic ou regra explícita).
            let resolved_cross = resolve_cross_size(child, content_cross, align, direction);

            // Posição cruzada: alignment baseia em cross_start + cross_offset.
            let cross_offset = align_items_offset(align, content_cross, resolved_cross);
            let (rect_x, rect_y, rect_w, rect_h) = match direction {
                FlexDirection::Row => (
                    cursor_main + margin,
                    cross_start + cross_offset + margin,
                    resolved_main,
                    resolved_cross,
                ),
                FlexDirection::Column => (
                    cross_start + cross_offset + margin,
                    cursor_main + margin,
                    resolved_cross,
                    resolved_main,
                ),
            };

            self.sizes[child_idx] = Some(rect_w);
            self.rects[child_idx] = LayoutRect {
                x: rect_x,
                y: rect_y,
                width: rect_w,
                height: rect_h,
            };
            self.layout_children(child_idx);

            // Acumula max cross size pra eventual auto height/width do pai.
            let child_cross = match direction {
                FlexDirection::Row => self.rects[child_idx].height,
                FlexDirection::Column => self.rects[child_idx].width,
            };
            max_cross = max_cross.max(child_cross + margin::uniform_extent(margin));

            cursor_main += margin::uniform_extent(margin) + resolved_main;
            if pos + 1 < in_flow.len() {
                cursor_main += gap + justify_extra_gap;
            }
        }

        // Auto-size do pai:
        // - Eixo CRUZADO: vira `max_cross` (maior filho no eixo cruzado) + edges.
        // - Eixo PRINCIPAL: vira o cursor_main final (soma dos children + gaps
        //   + margins) + edges. Isso é o que permite `flex-direction: column`
        //   com `height: auto` reportar a altura real do conteúdo pro container
        //   pai — sem isso, o pai fica com altura indefinida e o próximo
        //   sibling em block layout desenha por cima.
        //
        // `cursor_main` começou em `main_start(...)` e acumulou cada resolved_main
        // + margins + gaps. A extensão útil do conteúdo = cursor_main - start.
        if parent_idx != BODY_INDEX {
            let content_main_used = cursor_main - main_start(content_x, content_y, direction);
            match direction {
                FlexDirection::Row => {
                    // Main = width. Cross = height.
                    if self.entries[parent_idx].height_rule == DimensionRule::Auto {
                        self.rects[parent_idx].height = max_cross + padding * 2.0 + border_y;
                    }
                    if self.entries[parent_idx].width_rule == DimensionRule::Auto {
                        self.rects[parent_idx].width =
                            content_main_used + padding * 2.0 + border_x;
                    }
                }
                FlexDirection::Column => {
                    // Main = height. Cross = width.
                    if self.entries[parent_idx].width_rule == DimensionRule::Auto {
                        self.rects[parent_idx].width = max_cross + padding * 2.0 + border_x;
                    }
                    if self.entries[parent_idx].height_rule == DimensionRule::Auto {
                        self.rects[parent_idx].height =
                            content_main_used + padding * 2.0 + border_y;
                    }
                }
            }
        }
    }
}

/// Origem (em pixels absolutos) do eixo PRINCIPAL no content box do container.
/// É a coordenada onde o primeiro filho começa a ser posicionado, ANTES de
/// qualquer offset de `justify-content`.
///
/// - `content_x` / `content_y`: já calculados pelo caller como
///   `parent_rect.{x,y} + padding + border_{left,top}` — i.e., o canto
///   interno top-left do content box.
/// - Row: main = horizontal → usa `content_x`.
/// - Column: main = vertical → usa `content_y`.
///
/// O caller soma o offset inicial de justify-content a esse valor antes de
/// usar como cursor do loop de filhos. `main_start` é também usado no final
/// do layout pra calcular `content_main_used = cursor_main - main_start(...)`,
/// que alimenta o auto-size do container no eixo principal.
fn main_start(content_x: f32, content_y: f32, direction: FlexDirection) -> f32 {
    match direction {
        FlexDirection::Row => content_x,
        FlexDirection::Column => content_y,
    }
}

/// Origem do eixo CRUZADO (perpendicular ao principal) no content box.
/// `y` em row (cross = vertical), `x` em column (cross = horizontal).
/// Simétrico a [`main_start`].
fn cross_start_pos(content_x: f32, content_y: f32, direction: FlexDirection) -> f32 {
    match direction {
        FlexDirection::Row => content_y,
        FlexDirection::Column => content_x,
    }
}

/// `width_rule` em row, `height_rule` em column.
fn main_dim_rule(child: &LayoutEntry, direction: FlexDirection) -> DimensionRule {
    match direction {
        FlexDirection::Row => child.width_rule,
        FlexDirection::Column => child.height_rule,
    }
}

/// Resolve o tamanho do filho no eixo principal (com regra explícita).
fn main_resolve(child: &LayoutEntry, content_main: f32, direction: FlexDirection) -> f32 {
    match direction {
        FlexDirection::Row => width::resolve(child, content_main),
        FlexDirection::Column => height::resolve(child, content_main),
    }
}

/// Tamanho do filho no eixo principal quando Auto sem flex-grow (shrink-to-fit).
fn main_auto_leaf(child: &LayoutEntry, direction: FlexDirection) -> f32 {
    match direction {
        FlexDirection::Row => width::resolve_auto_leaf(child),
        FlexDirection::Column => height::resolve_auto_leaf(child),
    }
}

/// Resolve tamanho do filho no eixo cruzado considerando align-items.
///
/// `Stretch` + cross dim Auto = preenche `cross_size` do pai. Senão resolve
/// normalmente (`width::resolve` ou `height::resolve`).
fn resolve_cross_size(
    child: &LayoutEntry,
    content_cross: f32,
    align: AlignItems,
    direction: FlexDirection,
) -> f32 {
    let cross_rule = match direction {
        FlexDirection::Row => child.height_rule,
        FlexDirection::Column => child.width_rule,
    };
    if align == AlignItems::Stretch && cross_rule == DimensionRule::Auto {
        return content_cross;
    }
    match direction {
        FlexDirection::Row => height::resolve(child, content_cross),
        FlexDirection::Column => width::resolve(child, content_cross),
    }
}

/// Offset do filho no eixo cruzado, dado o tamanho cruzado do pai (`content_cross`)
/// e do filho (`child_cross`).
///
/// Stretch retorna 0 (filho já foi esticado em `resolve_cross_size`).
fn align_items_offset(align: AlignItems, content_cross: f32, child_cross: f32) -> f32 {
    let slack = (content_cross - child_cross).max(0.0);
    match align {
        AlignItems::Stretch | AlignItems::FlexStart => 0.0,
        AlignItems::Center => slack * 0.5,
        AlignItems::FlexEnd => slack,
    }
}

/// Distribuição CSS de `justify-content` no eixo principal.
///
/// Retorna `(offset_inicial, gap_extra_entre_filhos)` — offset_inicial é
/// somado ao cursor antes do primeiro filho; gap_extra é adicionado ao gap
/// entre cada par de filhos (depois do gap normal do CSS `gap:`).
///
/// `space-between` com 1 filho cai pra `flex-start` (não tem espaço entre).
fn justify_content_distribution(
    justify: JustifyContent,
    remaining: f32,
    n_children: usize,
) -> (f32, f32) {
    if remaining <= 0.0 || n_children == 0 {
        return (0.0, 0.0);
    }
    match justify {
        JustifyContent::FlexStart => (0.0, 0.0),
        JustifyContent::Center => (remaining * 0.5, 0.0),
        JustifyContent::FlexEnd => (remaining, 0.0),
        JustifyContent::SpaceBetween => {
            if n_children < 2 {
                (0.0, 0.0)
            } else {
                (0.0, remaining / (n_children - 1) as f32)
            }
        }
        JustifyContent::SpaceAround => {
            // R/(2N) nas pontas, R/N entre cada par.
            let unit = remaining / n_children as f32;
            (unit * 0.5, unit)
        }
        JustifyContent::SpaceEvenly => {
            // R/(N+1) em cada um dos N+1 espaços (incluindo antes do 1º e depois do último).
            let unit = remaining / (n_children + 1) as f32;
            (unit, unit)
        }
    }
}
