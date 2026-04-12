use eframe::egui;

/// Estado de um drag em andamento. None = nada sendo arrastado.
#[derive(Debug)]
pub struct DragState {
    /// Id do node sendo arrastado.
    pub node_id: String,
    /// Offset entre o ponto de clique e o origin (x,y) do node,
    /// em CSS pixels (unscaled). Garante que o node não "teleporta"
    /// pro cursor ao iniciar o drag.
    pub offset: egui::Vec2,
}

/// Estado de um wire drag em andamento (arraste de port → cursor).
#[derive(Debug)]
pub struct WireDragState {
    /// Id do node de origem.
    pub from_node_id: String,
    /// Nome do port de saída sendo arrastado.
    pub from_port_name: String,
    /// Tipo do port (determina a cor da bezier temporária).
    pub port_type: PortType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PortType {
    Exec,
    Data,
    Input,
    Output,
}

#[derive(Debug, Clone)]
pub struct PortData {
    pub name: String,
    pub port_type: PortType,
}

#[derive(Debug, Clone)]
pub struct FieldData {
    pub label: String,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct NodeData {
    pub id: String,
    pub node_type: String,
    pub subtitle: String,
    pub x: f32,
    pub y: f32,
    pub color: egui::Color32,
    pub inputs: Vec<PortData>,
    pub outputs: Vec<PortData>,
    pub fields: Vec<FieldData>,
    pub selected: bool,
}

#[derive(Debug, Clone)]
pub struct WireData {
    pub from_node: String,
    pub from_port: String,
    pub to_node: String,
    pub to_port: String,
    pub wire_type: PortType,
}
