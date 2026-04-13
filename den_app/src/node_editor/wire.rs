//! Renderização de wires (bezier cúbica entre ports).

use super::{theme, types::*};
use eframe::egui;

/// Retorna a posição screen-space de um port em um node.
/// `is_output = true` → borda direita do node; `false` → borda esquerda.
/// Retorna `None` se `port_name` não for encontrado na lista correspondente.
pub fn get_port_position(
    node: &NodeData,
    port_name: &str,
    is_output: bool,
    scale: f32,
    canvas_origin: egui::Pos2,
) -> Option<egui::Pos2> {
    let ports = if is_output {
        &node.outputs
    } else {
        &node.inputs
    };
    let idx = ports.iter().position(|p| p.name == port_name)?;

    let x = if is_output {
        node.x + theme::NODE_WIDTH
    } else {
        node.x
    };
    let y = node.y
        + theme::HEADER_HEIGHT
        + theme::BODY_PAD_TOP
        + idx as f32 * theme::PORT_ROW_HEIGHT
        + theme::PORT_ROW_HEIGHT / 2.0;

    Some(egui::pos2(
        canvas_origin.x + x * scale,
        canvas_origin.y + y * scale,
    ))
}

/// Desenha uma bezier cúbica entre dois ports conectados por `wire`.
/// Cor determinada por `wire.wire_type`. Retorna sem desenhar se algum node ou port não for encontrado.
pub fn draw_wire(
    painter: &egui::Painter,
    wire: &WireData,
    nodes: &[NodeData],
    scale: f32,
    canvas_origin: egui::Pos2,
) {
    let from_node = nodes.iter().find(|n| n.id == wire.from_node);
    let to_node = nodes.iter().find(|n| n.id == wire.to_node);
    let (Some(from), Some(to)) = (from_node, to_node) else {
        return;
    };

    let Some(from_pos) = get_port_position(from, &wire.from_port, true, scale, canvas_origin)
    else {
        return;
    };
    let Some(to_pos) = get_port_position(to, &wire.to_port, false, scale, canvas_origin) else {
        return;
    };

    let wire_color = match wire.wire_type {
        PortType::Exec => theme::WIRE_EXEC,
        PortType::Data => theme::WIRE_DATA,
        PortType::Input => theme::PORT_INPUT,
        PortType::Output => theme::PORT_OUTPUT,
    };

    let dx = (to_pos.x - from_pos.x).abs();
    let tension = dx.max(theme::WIRE_MIN_TENSION * scale) * theme::WIRE_TENSION_RATIO;
    let cp1 = egui::pos2(from_pos.x + tension, from_pos.y);
    let cp2 = egui::pos2(to_pos.x - tension, to_pos.y);

    let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
        [from_pos, cp1, cp2, to_pos],
        false,
        egui::Color32::TRANSPARENT,
        egui::Stroke::new(
            theme::WIRE_THICKNESS,
            wire_color.linear_multiply(theme::WIRE_OPACITY),
        ),
    );
    painter.add(bezier);
}
