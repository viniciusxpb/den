use eframe::egui;

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
