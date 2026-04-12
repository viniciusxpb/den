use eframe::egui;

// -- Background & Grid ---------------------------------------------------
pub const BG: egui::Color32 = egui::Color32::from_rgb(13, 13, 26);
pub const GRID_DOT_COLOR: egui::Color32 = egui::Color32::from_rgb(37, 37, 64);
pub const GRID_SPACING: f32 = 24.0;
pub const GRID_DOT_RADIUS: f32 = 0.5;

// -- Node: dimensions ----------------------------------------------------
pub const NODE_WIDTH: f32 = 260.0;
pub const NODE_CORNER_RADIUS: f32 = 8.0;
pub const NODE_PAD_X: f32 = 14.0;

// -- Node: header ---------------------------------------------------------
pub const HEADER_HEIGHT: f32 = 44.0;
pub const HEADER_PATCH_HEIGHT: f32 = 8.0;
pub const ACCENT_LINE_HEIGHT: f32 = 3.0;
pub const ACCENT_COVER_HEIGHT: f32 = 2.0;
pub const TITLE_OFFSET_Y: f32 = 20.0;
pub const SUBTITLE_OFFSET_Y: f32 = 36.0;

// -- Node: body -----------------------------------------------------------
pub const BODY_PAD_TOP: f32 = 8.0;
pub const BODY_PAD_BOTTOM: f32 = 12.0;
pub const PORT_ROW_HEIGHT: f32 = 26.0;
pub const FIELD_ROW_HEIGHT: f32 = 24.0;
pub const FIELD_SEPARATOR_GAP: f32 = 12.0;
pub const SEPARATOR_INSET: f32 = 10.0;

// -- Node: shadow ---------------------------------------------------------
pub const SHADOW_OFFSET: f32 = 3.0;
pub const SHADOW_ALPHA: u8 = 100;

// -- Node: borders --------------------------------------------------------
pub const BORDER_WIDTH_NORMAL: f32 = 0.5;
pub const BORDER_WIDTH_SELECTED: f32 = 1.5;
pub const SEPARATOR_STROKE_WIDTH: f32 = 0.5;

// -- Ports ----------------------------------------------------------------
pub const PORT_RADIUS: f32 = 5.0;
pub const PORT_SHAPE_EXTRA: f32 = 1.0;
pub const PORT_GLOW_OFFSET: f32 = 2.0;
pub const PORT_GLOW_STROKE: f32 = 0.5;
pub const PORT_GLOW_OPACITY: f32 = 0.3;
pub const PORT_FILL_OPACITY: f32 = 0.9;
pub const PORT_LABEL_OFFSET_Y: f32 = 4.0;

// -- Fields ---------------------------------------------------------------
pub const FIELD_LABEL_X: f32 = 14.0;
pub const FIELD_VALUE_BOX_X: f32 = 100.0;
pub const FIELD_VALUE_TEXT_X: f32 = 108.0;
pub const FIELD_VALUE_BOX_MARGIN: f32 = 14.0;
pub const FIELD_VALUE_BOX_HEIGHT: f32 = 18.0;
pub const FIELD_VALUE_BOX_RADIUS: f32 = 3.0;
pub const FIELD_VALUE_BOX_OFFSET_Y: f32 = 12.0;
pub const FIELD_VALUE_BG_ALPHA: u8 = 8;
pub const FIELD_OFFSET_Y: f32 = 3.0;

// -- Fields: separator (dashed line) --------------------------------------
pub const FIELD_SEPARATOR_OFFSET_Y: f32 = 4.0;
pub const FIELD_SEPARATOR_DASH: f32 = 3.0;
pub const FIELD_SEPARATOR_GAP_DASH: f32 = 3.0;

// -- Wires ----------------------------------------------------------------
pub const WIRE_THICKNESS: f32 = 1.5;
pub const WIRE_OPACITY: f32 = 0.6;
pub const WIRE_MIN_TENSION: f32 = 80.0;
pub const WIRE_TENSION_RATIO: f32 = 0.5;

// -- Colors: node ---------------------------------------------------------
pub const NODE_BG: egui::Color32 = egui::Color32::from_rgb(20, 20, 37);
pub const NODE_BORDER: egui::Color32 = egui::Color32::from_rgb(42, 42, 74);
pub const NODE_HEADER_BG: egui::Color32 = egui::Color32::from_rgb(26, 26, 53);

// -- Colors: ports --------------------------------------------------------
pub const PORT_EXEC: egui::Color32 = egui::Color32::from_rgb(129, 199, 132);
pub const PORT_DATA: egui::Color32 = egui::Color32::from_rgb(255, 213, 79);
pub const PORT_INPUT: egui::Color32 = egui::Color32::from_rgb(79, 195, 247);
pub const PORT_OUTPUT: egui::Color32 = egui::Color32::from_rgb(233, 69, 96);

// -- Colors: wires --------------------------------------------------------
pub const WIRE_EXEC: egui::Color32 = PORT_EXEC;
pub const WIRE_DATA: egui::Color32 = PORT_DATA;
pub const WIRE_DEFAULT: egui::Color32 = PORT_INPUT;

// -- Colors: accent per node type -----------------------------------------
pub const ACCENT_BLUE: egui::Color32 = egui::Color32::from_rgb(79, 195, 247);
pub const ACCENT_YELLOW: egui::Color32 = egui::Color32::from_rgb(255, 213, 79);
pub const ACCENT_GREEN: egui::Color32 = egui::Color32::from_rgb(129, 199, 132);
pub const ACCENT_RED: egui::Color32 = egui::Color32::from_rgb(233, 69, 96);

// -- Colors: text ---------------------------------------------------------
pub const TEXT: egui::Color32 = egui::Color32::from_rgb(200, 200, 224);
pub const TEXT_DIM: egui::Color32 = egui::Color32::from_rgb(106, 106, 138);
pub const TEXT_BRIGHT: egui::Color32 = egui::Color32::from_rgb(240, 240, 255);

// -- Font sizes -----------------------------------------------------------
pub const FONT_NODE_TITLE: f32 = 13.0;
pub const FONT_NODE_SUBTITLE: f32 = 10.0;
pub const FONT_PORT: f32 = 11.0;
pub const FONT_FIELD_LABEL: f32 = 10.0;
pub const FONT_FIELD_VALUE: f32 = 10.0;
