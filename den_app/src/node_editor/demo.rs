use super::theme;
use super::types::*;

pub fn demo_nodes() -> Vec<NodeData> {
    vec![
        NodeData {
            id: "hermes".into(),
            node_type: "Hermes".into(),
            subtitle: "Message Router".into(),
            x: 80.0, y: 60.0,
            color: theme::ACCENT_BLUE,
            inputs: vec![
                PortData { name: "trigger".into(), port_type: PortType::Exec },
                PortData { name: "payload".into(), port_type: PortType::Data },
            ],
            outputs: vec![
                PortData { name: "next".into(), port_type: PortType::Exec },
                PortData { name: "response".into(), port_type: PortType::Data },
                PortData { name: "error".into(), port_type: PortType::Output },
            ],
            fields: vec![
                FieldData { label: "protocol".into(), value: "WebSocket".into() },
                FieldData { label: "port".into(), value: "8080".into() },
            ],
            selected: false,
        },
        NodeData {
            id: "atlas".into(),
            node_type: "Atlas".into(),
            subtitle: "State Manager".into(),
            x: 460.0, y: 30.0,
            color: theme::ACCENT_YELLOW,
            inputs: vec![
                PortData { name: "exec".into(), port_type: PortType::Exec },
                PortData { name: "key".into(), port_type: PortType::Data },
                PortData { name: "value".into(), port_type: PortType::Data },
            ],
            outputs: vec![
                PortData { name: "done".into(), port_type: PortType::Exec },
                PortData { name: "state".into(), port_type: PortType::Data },
            ],
            fields: vec![
                FieldData { label: "store".into(), value: "persistent".into() },
                FieldData { label: "ttl".into(), value: "3600s".into() },
            ],
            selected: false,
        },
        NodeData {
            id: "argus".into(),
            node_type: "Argus".into(),
            subtitle: "Monitor".into(),
            x: 460.0, y: 310.0,
            color: theme::ACCENT_GREEN,
            inputs: vec![
                PortData { name: "watch".into(), port_type: PortType::Exec },
                PortData { name: "source".into(), port_type: PortType::Input },
            ],
            outputs: vec![
                PortData { name: "alert".into(), port_type: PortType::Exec },
                PortData { name: "metrics".into(), port_type: PortType::Data },
            ],
            fields: vec![
                FieldData { label: "interval".into(), value: "500ms".into() },
                FieldData { label: "threshold".into(), value: "0.95".into() },
            ],
            selected: false,
        },
        NodeData {
            id: "athena".into(),
            node_type: "Athena".into(),
            subtitle: "Decision Engine".into(),
            x: 840.0, y: 140.0,
            color: theme::ACCENT_RED,
            inputs: vec![
                PortData { name: "exec".into(), port_type: PortType::Exec },
                PortData { name: "state".into(), port_type: PortType::Data },
                PortData { name: "metrics".into(), port_type: PortType::Data },
            ],
            outputs: vec![
                PortData { name: "action".into(), port_type: PortType::Exec },
                PortData { name: "result".into(), port_type: PortType::Output },
            ],
            fields: vec![
                FieldData { label: "strategy".into(), value: "adaptive".into() },
                FieldData { label: "confidence".into(), value: "0.87".into() },
            ],
            selected: true,
        },
    ]
}

pub fn demo_wires() -> Vec<WireData> {
    vec![
        WireData { from_node: "hermes".into(), from_port: "next".into(), to_node: "atlas".into(), to_port: "exec".into(), wire_type: PortType::Exec },
        WireData { from_node: "hermes".into(), from_port: "response".into(), to_node: "atlas".into(), to_port: "value".into(), wire_type: PortType::Data },
        WireData { from_node: "hermes".into(), from_port: "next".into(), to_node: "argus".into(), to_port: "watch".into(), wire_type: PortType::Exec },
        WireData { from_node: "atlas".into(), from_port: "state".into(), to_node: "athena".into(), to_port: "state".into(), wire_type: PortType::Data },
        WireData { from_node: "argus".into(), from_port: "metrics".into(), to_node: "athena".into(), to_port: "metrics".into(), wire_type: PortType::Data },
        WireData { from_node: "argus".into(), from_port: "alert".into(), to_node: "athena".into(), to_port: "exec".into(), wire_type: PortType::Exec },
    ]
}
