use eframe::egui;
use super::{theme, types::*};

pub fn calculate_node_height(node: &NodeData) -> f32 {
    let port_rows = node.inputs.len().max(node.outputs.len());
    let field_rows = node.fields.len();
    let fields_section = if field_rows > 0 {
        theme::FIELD_SEPARATOR_GAP + field_rows as f32 * theme::FIELD_ROW_HEIGHT
    } else {
        0.0
    };

    theme::HEADER_HEIGHT
        + theme::BODY_PAD_TOP
        + port_rows as f32 * theme::PORT_ROW_HEIGHT
        + fields_section
        + theme::BODY_PAD_BOTTOM
}

pub fn draw_node(
    painter: &egui::Painter,
    node: &NodeData,
    scale: f32,
    canvas_origin: egui::Pos2,
) {
    let s = scale;
    let x = canvas_origin.x + node.x * s;
    let y = canvas_origin.y + node.y * s;
    let w = theme::NODE_WIDTH * s;
    let h = calculate_node_height(node) * s;
    let node_rect = egui::Rect::from_min_size(egui::pos2(x, y), egui::vec2(w, h));

    // 1. Shadow
    let shadow_rect = node_rect.translate(egui::vec2(theme::SHADOW_OFFSET * s, theme::SHADOW_OFFSET * s));
    painter.rect_filled(
        shadow_rect,
        theme::NODE_CORNER_RADIUS * s,
        egui::Color32::from_rgba_unmultiplied(0, 0, 0, theme::SHADOW_ALPHA),
    );

    // 2. Body
    let border_color = if node.selected { node.color } else { theme::NODE_BORDER };
    let border_width = if node.selected { theme::BORDER_WIDTH_SELECTED } else { theme::BORDER_WIDTH_NORMAL };
    painter.rect(
        node_rect,
        theme::NODE_CORNER_RADIUS * s,
        theme::NODE_BG,
        egui::Stroke::new(border_width, border_color),
        egui::StrokeKind::Outside,
    );

    // 3. Header background
    let header_rect = egui::Rect::from_min_size(node_rect.min, egui::vec2(w, theme::HEADER_HEIGHT * s));
    painter.rect_filled(header_rect, theme::NODE_CORNER_RADIUS * s, theme::NODE_HEADER_BG);
    // Patch: cover bottom rounded corners
    let patch_rect = egui::Rect::from_min_size(
        egui::pos2(x, y + (theme::HEADER_HEIGHT - theme::HEADER_PATCH_HEIGHT) * s),
        egui::vec2(w, theme::HEADER_PATCH_HEIGHT * s),
    );
    painter.rect_filled(patch_rect, 0.0, theme::NODE_HEADER_BG);

    // 4. Accent line
    let accent_rect = egui::Rect::from_min_size(node_rect.min, egui::vec2(w, theme::ACCENT_LINE_HEIGHT * s));
    painter.rect_filled(accent_rect, theme::NODE_CORNER_RADIUS * s, node.color);
    let accent_cover = egui::Rect::from_min_size(
        egui::pos2(x, y + theme::ACCENT_LINE_HEIGHT * s - theme::ACCENT_COVER_HEIGHT * s),
        egui::vec2(w, theme::ACCENT_COVER_HEIGHT * s),
    );
    painter.rect_filled(accent_cover, 0.0, node.color);

    // 5. Title
    painter.text(
        egui::pos2(x + theme::NODE_PAD_X * s, y + theme::TITLE_OFFSET_Y * s),
        egui::Align2::LEFT_TOP,
        &node.node_type,
        egui::FontId::monospace(theme::FONT_NODE_TITLE * s),
        node.color,
    );

    // 5b. Subtitle
    painter.text(
        egui::pos2(x + theme::NODE_PAD_X * s, y + theme::SUBTITLE_OFFSET_Y * s),
        egui::Align2::LEFT_TOP,
        &node.subtitle,
        egui::FontId::monospace(theme::FONT_NODE_SUBTITLE * s),
        theme::TEXT_DIM,
    );

    // 6. Header separator
    painter.line_segment(
        [
            egui::pos2(x + theme::SEPARATOR_INSET * s, y + theme::HEADER_HEIGHT * s),
            egui::pos2(x + w - theme::SEPARATOR_INSET * s, y + theme::HEADER_HEIGHT * s),
        ],
        egui::Stroke::new(theme::SEPARATOR_STROKE_WIDTH, theme::NODE_BORDER),
    );

    // 7. Input ports
    for (i, port) in node.inputs.iter().enumerate() {
        let py = y + (theme::HEADER_HEIGHT + theme::BODY_PAD_TOP
            + i as f32 * theme::PORT_ROW_HEIGHT + theme::PORT_ROW_HEIGHT / 2.0) * s;
        draw_port(painter, egui::pos2(x, py), port, false, s);
        painter.text(
            egui::pos2(x + theme::NODE_PAD_X * s, py + theme::PORT_LABEL_OFFSET_Y * s),
            egui::Align2::LEFT_TOP,
            &port.name,
            egui::FontId::monospace(theme::FONT_PORT * s),
            theme::TEXT,
        );
    }

    // 8. Output ports
    for (i, port) in node.outputs.iter().enumerate() {
        let py = y + (theme::HEADER_HEIGHT + theme::BODY_PAD_TOP
            + i as f32 * theme::PORT_ROW_HEIGHT + theme::PORT_ROW_HEIGHT / 2.0) * s;
        draw_port(painter, egui::pos2(x + w, py), port, true, s);
        painter.text(
            egui::pos2(x + w - theme::NODE_PAD_X * s, py + theme::PORT_LABEL_OFFSET_Y * s),
            egui::Align2::RIGHT_TOP,
            &port.name,
            egui::FontId::monospace(theme::FONT_PORT * s),
            theme::TEXT,
        );
    }

    // 9-10. Fields
    if !node.fields.is_empty() {
        let max_ports = node.inputs.len().max(node.outputs.len());
        let fields_base_y = y + (theme::HEADER_HEIGHT + theme::BODY_PAD_TOP
            + max_ports as f32 * theme::PORT_ROW_HEIGHT) * s;
        draw_fields(painter, node, x, w, fields_base_y, s);
    }
}

fn draw_fields(
    painter: &egui::Painter,
    node: &NodeData,
    node_x: f32,
    node_w: f32,
    base_y: f32,
    s: f32,
) {
    // 9. Dashed separator
    let sep_y = base_y + theme::FIELD_SEPARATOR_OFFSET_Y * s;
    draw_dashed_line(
        painter,
        egui::pos2(node_x + theme::NODE_PAD_X * s, sep_y),
        egui::pos2(node_x + node_w - theme::NODE_PAD_X * s, sep_y),
        theme::FIELD_SEPARATOR_DASH * s,
        theme::FIELD_SEPARATOR_GAP_DASH * s,
        egui::Stroke::new(theme::SEPARATOR_STROKE_WIDTH, theme::NODE_BORDER),
    );

    // 10. Fields
    for (i, field) in node.fields.iter().enumerate() {
        let fy = base_y
            + theme::FIELD_SEPARATOR_GAP * s
            + i as f32 * theme::FIELD_ROW_HEIGHT * s
            + theme::FIELD_ROW_HEIGHT / 2.0 * s
            + theme::FIELD_OFFSET_Y * s;

        // Label
        painter.text(
            egui::pos2(node_x + theme::FIELD_LABEL_X * s, fy),
            egui::Align2::LEFT_TOP,
            &field.label,
            egui::FontId::monospace(theme::FONT_FIELD_LABEL * s),
            theme::TEXT_DIM,
        );

        // Value box
        let box_x = node_x + theme::FIELD_VALUE_BOX_X * s;
        let box_w = node_w - (theme::FIELD_VALUE_BOX_X + theme::FIELD_VALUE_BOX_MARGIN) * s;
        let box_rect = egui::Rect::from_min_size(
            egui::pos2(box_x, fy - theme::FIELD_VALUE_BOX_OFFSET_Y * s),
            egui::vec2(box_w, theme::FIELD_VALUE_BOX_HEIGHT * s),
        );
        painter.rect(
            box_rect,
            theme::FIELD_VALUE_BOX_RADIUS * s,
            egui::Color32::from_rgba_unmultiplied(255, 255, 255, theme::FIELD_VALUE_BG_ALPHA),
            egui::Stroke::new(theme::SEPARATOR_STROKE_WIDTH, theme::NODE_BORDER),
            egui::StrokeKind::Outside,
        );

        // Value text
        painter.text(
            egui::pos2(node_x + theme::FIELD_VALUE_TEXT_X * s, fy),
            egui::Align2::LEFT_TOP,
            &field.value,
            egui::FontId::monospace(theme::FONT_FIELD_VALUE * s),
            theme::TEXT_BRIGHT,
        );
    }
}

fn draw_dashed_line(
    painter: &egui::Painter,
    from: egui::Pos2,
    to: egui::Pos2,
    dash_len: f32,
    gap_len: f32,
    stroke: egui::Stroke,
) {
    let total_len = from.distance(to);
    if total_len < 0.01 { return; }
    let dir = (to - from) / total_len;
    let mut pos = 0.0;
    while pos < total_len {
        let dash_end = (pos + dash_len).min(total_len);
        painter.line_segment([from + dir * pos, from + dir * dash_end], stroke);
        pos = dash_end + gap_len;
    }
}

fn draw_port(
    painter: &egui::Painter,
    center: egui::Pos2,
    port: &PortData,
    is_output: bool,
    scale: f32,
) {
    let r = theme::PORT_RADIUS * scale;
    let color = port_color(port.port_type);

    let fill = color.linear_multiply(theme::PORT_FILL_OPACITY);

    match port.port_type {
        PortType::Exec => {
            let s = r + theme::PORT_SHAPE_EXTRA * scale;
            let points = if is_output {
                vec![
                    egui::pos2(center.x - s, center.y - s),
                    egui::pos2(center.x + s, center.y),
                    egui::pos2(center.x - s, center.y + s),
                ]
            } else {
                vec![
                    egui::pos2(center.x + s, center.y - s),
                    egui::pos2(center.x - s, center.y),
                    egui::pos2(center.x + s, center.y + s),
                ]
            };
            painter.add(egui::Shape::convex_polygon(points, fill, egui::Stroke::NONE));
        }
        PortType::Output => {
            let s = r + theme::PORT_SHAPE_EXTRA * scale;
            let points = vec![
                egui::pos2(center.x, center.y - s),
                egui::pos2(center.x + s, center.y),
                egui::pos2(center.x, center.y + s),
                egui::pos2(center.x - s, center.y),
            ];
            painter.add(egui::Shape::convex_polygon(points, fill, egui::Stroke::NONE));
        }
        PortType::Data | PortType::Input => {
            painter.circle_filled(center, r, color);
            painter.circle_stroke(
                center,
                r + theme::PORT_GLOW_OFFSET * scale,
                egui::Stroke::new(
                    theme::PORT_GLOW_STROKE,
                    color.linear_multiply(theme::PORT_GLOW_OPACITY),
                ),
            );
        }
    }
}

fn port_color(port_type: PortType) -> egui::Color32 {
    match port_type {
        PortType::Exec => theme::PORT_EXEC,
        PortType::Data => theme::PORT_DATA,
        PortType::Input => theme::PORT_INPUT,
        PortType::Output => theme::PORT_OUTPUT,
    }
}
