import { useState, useRef, useCallback, useEffect, useMemo } from "react";
import { PlayCircle, Hash, Percent, Type, FolderOpen, Database, Shuffle, BrainCircuit, Terminal, FileText, ChevronDown, ChevronRight, Zap, Activity, HardDrive, Cpu, Thermometer, MemoryStick, Settings, Palette, X, Info, Layers } from "lucide-react";

// ── Color System ──
const C = {
  bg: "#08080d",
  canvas: "#0a0a10",
  gridDot: "#16161f",
  node: "#0e0e16",
  nodeBorder: "#1a1a28",
  nodeHeader: "#111119",
  sidebar: "#0b0b12",
  statusBar: "#0a0a0f",
  text: "#c4c4d0",
  textDim: "#555568",
  textBright: "#e8e8f2",
  accent: "#00d4aa",
  accentDim: "rgba(0,212,170,0.15)",
  // Port type colors
  MODEL: "#ff8844",
  CLIP: "#aa66ff",
  VAE: "#4488ff",
  CONDITIONING: "#ffcc00",
  LATENT: "#ff4488",
  IMAGE: "#00d4aa",
  FLOAT: "#10b981",
  INT: "#10b981",
  STRING: "#10b981",
  PATH: "#10b981",
  ANY: "#888899",
};

const portGlow = (color) => `0 0 6px ${color}66, 0 0 2px ${color}aa`;

// ── Icon Map ──
const ICON_MAP = {
  "Simple Play": PlayCircle,
  "Input Int": Hash,
  "Input Float": Percent,
  "Input String": Type,
  "Input Path": FolderOpen,
  "Load Checkpoint": Database,
  "CLIP Text Encode": Layers,
  "KSampler": Shuffle,
  "Empty Latent Image": HardDrive,
  "VAE Decode": BrainCircuit,
  "Save Image": FileText,
  "Console Log": Terminal,
};

// ── Node Definitions ──
const NODE_DEFS = [
  {
    id: "ckpt", type: "Load Checkpoint", cat: "loaders", color: C.MODEL,
    x: 60, y: 160, w: 240,
    fields: [{ label: "ckpt_name", value: "dreamshaper_8.safetensors", kind: "dropdown" }],
    inputs: [],
    outputs: [
      { name: "MODEL", type: "MODEL" },
      { name: "CLIP", type: "CLIP" },
      { name: "VAE", type: "VAE" },
    ],
    desc: "Loads model weights from .safetensors file",
  },
  {
    id: "clip_pos", type: "CLIP Text Encode", cat: "conditioning", color: C.CONDITIONING,
    x: 400, y: 60, w: 260,
    fields: [{ label: "prompt", value: "mao mao, golshi pose, confident stance, anime style", kind: "text" }],
    inputs: [{ name: "CLIP", type: "CLIP" }],
    outputs: [{ name: "CONDITIONING", type: "CONDITIONING" }],
    desc: "Encodes positive prompt via CLIP",
  },
  {
    id: "clip_neg", type: "CLIP Text Encode", cat: "conditioning", color: C.CONDITIONING,
    x: 400, y: 280, w: 260,
    fields: [{ label: "prompt", value: "worst quality, blurry, deformed", kind: "text" }],
    inputs: [{ name: "CLIP", type: "CLIP" }],
    outputs: [{ name: "CONDITIONING", type: "CONDITIONING" }],
    desc: "Encodes negative prompt via CLIP",
  },
  {
    id: "empty_latent", type: "Empty Latent Image", cat: "latent", color: C.LATENT,
    x: 400, y: 470, w: 220,
    fields: [
      { label: "width", value: "512", kind: "int" },
      { label: "height", value: "512", kind: "int" },
      { label: "batch", value: "1", kind: "int" },
    ],
    inputs: [],
    outputs: [{ name: "LATENT", type: "LATENT" }],
    desc: "Creates empty latent tensor",
  },
  {
    id: "ksampler", type: "KSampler", cat: "sampling", color: C.LATENT,
    x: 760, y: 180, w: 250,
    fields: [
      { label: "seed", value: "42", kind: "int" },
      { label: "steps", value: "20", kind: "int" },
      { label: "cfg", value: "7.0", kind: "float" },
      { label: "sampler", value: "euler_a", kind: "dropdown" },
    ],
    inputs: [
      { name: "model", type: "MODEL" },
      { name: "positive", type: "CONDITIONING" },
      { name: "negative", type: "CONDITIONING" },
      { name: "latent", type: "LATENT" },
    ],
    outputs: [{ name: "LATENT", type: "LATENT" }],
    desc: "Denoises latent using model + conditioning",
  },
  {
    id: "vae_decode", type: "VAE Decode", cat: "latent", color: C.IMAGE,
    x: 1100, y: 200, w: 220,
    fields: [],
    inputs: [
      { name: "samples", type: "LATENT" },
      { name: "vae", type: "VAE" },
    ],
    outputs: [{ name: "IMAGE", type: "IMAGE" }],
    desc: "Decodes latent to pixel space via VAE",
  },
  {
    id: "save", type: "Save Image", cat: "output", color: C.IMAGE,
    x: 1400, y: 200, w: 220,
    fields: [{ label: "prefix", value: "ndnm_output", kind: "text" }],
    inputs: [{ name: "images", type: "IMAGE" }],
    outputs: [],
    desc: "Saves output images to disk via Atlas",
  },
];

// ── Wire Definitions ──
const WIRE_DEFS = [
  { from: "ckpt", fromPort: 0, to: "ksampler", toPort: 0, type: "MODEL" },
  { from: "ckpt", fromPort: 1, to: "clip_pos", toPort: 0, type: "CLIP" },
  { from: "ckpt", fromPort: 1, to: "clip_neg", toPort: 0, type: "CLIP" },
  { from: "ckpt", fromPort: 2, to: "vae_decode", toPort: 1, type: "VAE" },
  { from: "clip_pos", fromPort: 0, to: "ksampler", toPort: 1, type: "CONDITIONING" },
  { from: "clip_neg", fromPort: 0, to: "ksampler", toPort: 2, type: "CONDITIONING" },
  { from: "empty_latent", fromPort: 0, to: "ksampler", toPort: 3, type: "LATENT" },
  { from: "ksampler", fromPort: 0, to: "vae_decode", toPort: 0, type: "LATENT" },
  { from: "vae_decode", fromPort: 0, to: "save", toPort: 0, type: "IMAGE" },
];

// ── Categories for sidebar ──
const CATEGORIES = [
  { name: "loaders", label: "Loaders", icon: Database },
  { name: "conditioning", label: "Conditioning", icon: Layers },
  { name: "latent", label: "Latent", icon: HardDrive },
  { name: "sampling", label: "Sampling", icon: Shuffle },
  { name: "output", label: "Output", icon: FileText },
];

// ── Port component ──
function Port({ type, side, index, total, nodeH }) {
  const spacing = Math.min(28, (nodeH - 32) / Math.max(total, 1));
  const startY = 36 + (nodeH - 36 - spacing * (total - 1)) / 2;
  const y = startY + index * spacing;
  const x = side === "input" ? -6 : "100%";
  const color = C[type] || C.ANY;

  return (
    <div
      style={{
        position: "absolute",
        left: side === "input" ? -6 : "auto",
        right: side === "output" ? -6 : "auto",
        top: y,
        width: 12, height: 12,
        borderRadius: "50%",
        background: color,
        border: `2px solid ${C.node}`,
        boxShadow: portGlow(color),
        zIndex: 5,
        cursor: "crosshair",
      }}
      title={`${type}`}
    />
  );
}

// ── Node component ──
function Node({ node, isSelected, onSelect, onDragStart, camera }) {
  const Icon = ICON_MAP[node.type] || Zap;
  const nodeH = 36 + Math.max(node.fields.length, Math.max(node.inputs.length, node.outputs.length)) * 28 + 20;

  return (
    <div
      onMouseDown={(e) => {
        if (e.target.tagName === "INPUT" || e.target.tagName === "SELECT") return;
        e.stopPropagation();
        onSelect(node.id);
        onDragStart(node.id, e);
      }}
      style={{
        position: "absolute",
        left: node.x * camera.zoom + camera.x,
        top: node.y * camera.zoom + camera.y,
        width: node.w * camera.zoom,
        minHeight: nodeH * camera.zoom,
        background: C.node,
        border: `1px solid ${isSelected ? node.color : C.nodeBorder}`,
        borderRadius: 8 * camera.zoom,
        overflow: "visible",
        cursor: "grab",
        transition: "border-color 0.15s, box-shadow 0.15s",
        boxShadow: isSelected
          ? `0 0 20px ${node.color}22, 0 0 6px ${node.color}44`
          : "0 2px 12px rgba(0,0,0,0.4)",
        transform: `scale(1)`,
        zIndex: isSelected ? 10 : 1,
        fontSize: 12 * camera.zoom,
      }}
    >
      {/* Ports */}
      {node.inputs.map((p, i) => (
        <Port key={`in-${i}`} type={p.type} side="input" index={i} total={node.inputs.length} nodeH={nodeH * camera.zoom} />
      ))}
      {node.outputs.map((p, i) => (
        <Port key={`out-${i}`} type={p.type} side="output" index={i} total={node.outputs.length} nodeH={nodeH * camera.zoom} />
      ))}

      {/* Header */}
      <div
        style={{
          display: "flex", alignItems: "center", gap: 8 * camera.zoom,
          padding: `${6 * camera.zoom}px ${10 * camera.zoom}px`,
          borderBottom: `1px solid ${C.nodeBorder}`,
          background: C.nodeHeader,
          borderRadius: `${8 * camera.zoom}px ${8 * camera.zoom}px 0 0`,
        }}
      >
        <Icon size={16 * camera.zoom} color={node.color} strokeWidth={1.8} />
        <span style={{
          color: C.textBright,
          fontWeight: 600,
          fontSize: 11 * camera.zoom,
          letterSpacing: 0.5,
          flex: 1,
          fontFamily: "'JetBrains Mono', monospace",
        }}>
          {node.type}
        </span>
        <Info size={12 * camera.zoom} color={C.textDim} style={{ cursor: "pointer", opacity: 0.6 }} />
      </div>

      {/* Body */}
      <div style={{ padding: `${6 * camera.zoom}px ${10 * camera.zoom}px ${8 * camera.zoom}px` }}>
        {node.fields.map((f, i) => (
          <div key={i} style={{ display: "flex", alignItems: "center", gap: 6 * camera.zoom, marginBottom: 4 * camera.zoom }}>
            <span style={{
              color: C.textDim,
              fontSize: 10 * camera.zoom,
              minWidth: 48 * camera.zoom,
              fontFamily: "'JetBrains Mono', monospace",
            }}>
              {f.label}
            </span>
            {f.kind === "dropdown" ? (
              <div style={{
                flex: 1,
                background: "rgba(0,0,0,0.3)",
                border: `1px solid ${C.nodeBorder}`,
                borderRadius: 3 * camera.zoom,
                padding: `${2 * camera.zoom}px ${6 * camera.zoom}px`,
                color: C.accent,
                fontSize: 9 * camera.zoom,
                fontFamily: "'JetBrains Mono', monospace",
                display: "flex", alignItems: "center", justifyContent: "space-between",
              }}>
                <span style={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{f.value}</span>
                <ChevronDown size={10 * camera.zoom} color={C.textDim} />
              </div>
            ) : (
              <input
                defaultValue={f.value}
                onClick={(e) => e.stopPropagation()}
                style={{
                  flex: 1,
                  background: "rgba(0,0,0,0.3)",
                  border: `1px solid ${C.nodeBorder}`,
                  borderRadius: 3 * camera.zoom,
                  padding: `${2 * camera.zoom}px ${6 * camera.zoom}px`,
                  color: C.text,
                  fontSize: 9 * camera.zoom,
                  fontFamily: "'JetBrains Mono', monospace",
                  outline: "none",
                  width: 0,
                }}
              />
            )}
          </div>
        ))}
        {node.fields.length === 0 && (
          <span style={{ color: C.textDim, fontSize: 9 * camera.zoom, fontStyle: "italic", fontFamily: "'JetBrains Mono', monospace" }}>
            {node.desc}
          </span>
        )}
      </div>
    </div>
  );
}

// ── Bezier wire ──
function getPortPos(node, side, index, total, camera) {
  const nodeH = 36 + Math.max(node.fields.length, Math.max(node.inputs.length, node.outputs.length)) * 28 + 20;
  const spacing = Math.min(28, (nodeH - 32) / Math.max(total, 1));
  const startY = 36 + (nodeH - 36 - spacing * (total - 1)) / 2;
  const y = startY + index * spacing;

  const px = side === "output" ? node.x + node.w : node.x;
  const py = node.y + y;

  return {
    x: px * camera.zoom + camera.x,
    y: py * camera.zoom + camera.y,
  };
}

function Wire({ wire, nodes, camera, progress }) {
  const fromNode = nodes.find((n) => n.id === wire.from);
  const toNode = nodes.find((n) => n.id === wire.to);
  if (!fromNode || !toNode) return null;

  const start = getPortPos(fromNode, "output", wire.fromPort, fromNode.outputs.length, camera);
  const end = getPortPos(toNode, "input", wire.toPort, toNode.inputs.length, camera);

  const dx = Math.abs(end.x - start.x) * 0.5;
  const d = `M ${start.x} ${start.y} C ${start.x + dx} ${start.y}, ${end.x - dx} ${end.y}, ${end.x} ${end.y}`;
  const color = C[wire.type] || C.ANY;

  const isActive = progress > 0 && progress < 1;

  return (
    <g>
      <path d={d} fill="none" stroke={color} strokeWidth={2 * camera.zoom} strokeOpacity={0.2} />
      <path d={d} fill="none" stroke={color} strokeWidth={1.5 * camera.zoom} strokeOpacity={0.6} />
      {isActive && (
        <circle r={4 * camera.zoom} fill={color} opacity={0.9}>
          <animateMotion dur="1.2s" repeatCount="indefinite" path={d} />
        </circle>
      )}
    </g>
  );
}

// ── Sidebar category ──
function SidebarCategory({ cat, expanded, onToggle }) {
  const Icon = cat.icon;
  const catNodes = NODE_DEFS.filter((n) => n.cat === cat.name);

  return (
    <div style={{ marginBottom: 2 }}>
      <div
        onClick={onToggle}
        style={{
          display: "flex", alignItems: "center", gap: 8,
          padding: "7px 12px",
          cursor: "pointer",
          color: expanded ? C.textBright : C.textDim,
          fontSize: 11,
          fontWeight: 500,
          letterSpacing: 0.8,
          fontFamily: "'JetBrains Mono', monospace",
          transition: "color 0.15s",
          borderLeft: expanded ? `2px solid ${C.accent}` : "2px solid transparent",
          background: expanded ? "rgba(0,212,170,0.04)" : "transparent",
        }}
      >
        {expanded ? <ChevronDown size={12} /> : <ChevronRight size={12} />}
        <Icon size={13} strokeWidth={1.5} />
        <span style={{ textTransform: "uppercase" }}>{cat.label}</span>
        <span style={{ marginLeft: "auto", fontSize: 9, color: C.textDim }}>{catNodes.length}</span>
      </div>
      {expanded && (
        <div style={{ paddingLeft: 28 }}>
          {catNodes.map((n) => {
            const NIcon = ICON_MAP[n.type] || Zap;
            return (
              <div
                key={n.id}
                style={{
                  display: "flex", alignItems: "center", gap: 6,
                  padding: "4px 8px",
                  fontSize: 10,
                  color: C.text,
                  cursor: "grab",
                  borderRadius: 4,
                  fontFamily: "'JetBrains Mono', monospace",
                }}
              >
                <NIcon size={11} color={n.color} strokeWidth={1.5} />
                {n.type}
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}

// ── Main App ──
export default function NDNMEditor() {
  const [nodes, setNodes] = useState(NODE_DEFS.map((n) => ({ ...n })));
  const [camera, setCamera] = useState({ x: 0, y: 0, zoom: 0.85 });
  const [selected, setSelected] = useState(null);
  const [expandedCats, setExpandedCats] = useState(["loaders", "sampling"]);
  const [queueState, setQueueState] = useState("idle"); // idle | running | done
  const [queueProgress, setQueueProgress] = useState(0);
  const [stats, setStats] = useState({ cpu: 34, ram: 62, vram: 48, temp: 67 });
  const canvasRef = useRef(null);
  const dragRef = useRef(null);
  const panRef = useRef(null);

  // Fake stats pulse
  useEffect(() => {
    const t = setInterval(() => {
      setStats((s) => ({
        cpu: Math.min(100, Math.max(5, s.cpu + (Math.random() - 0.5) * 8)),
        ram: Math.min(100, Math.max(20, s.ram + (Math.random() - 0.5) * 4)),
        vram: Math.min(100, Math.max(10, s.vram + (Math.random() - 0.5) * 6)),
        temp: Math.min(90, Math.max(50, s.temp + (Math.random() - 0.5) * 3)),
      }));
    }, 2000);
    return () => clearInterval(t);
  }, []);

  // Queue runner
  const runQueue = useCallback(() => {
    if (queueState === "running") return;
    setQueueState("running");
    setQueueProgress(0);
    let p = 0;
    const t = setInterval(() => {
      p += 0.008 + Math.random() * 0.012;
      if (p >= 1) {
        p = 1;
        clearInterval(t);
        setQueueState("done");
        setTimeout(() => setQueueState("idle"), 3000);
      }
      setQueueProgress(p);
    }, 50);
  }, [queueState]);

  // Drag node
  const handleDragStart = useCallback((id, e) => {
    const node = nodes.find((n) => n.id === id);
    if (!node) return;
    dragRef.current = {
      id,
      startX: e.clientX,
      startY: e.clientY,
      origX: node.x,
      origY: node.y,
    };
  }, [nodes]);

  // Pan canvas
  const handleCanvasMouseDown = useCallback((e) => {
    if (e.target !== canvasRef.current && e.target.tagName !== "svg") return;
    setSelected(null);
    panRef.current = { startX: e.clientX, startY: e.clientY, origX: camera.x, origY: camera.y };
  }, [camera]);

  useEffect(() => {
    const handleMouseMove = (e) => {
      if (dragRef.current) {
        const d = dragRef.current;
        const dx = (e.clientX - d.startX) / camera.zoom;
        const dy = (e.clientY - d.startY) / camera.zoom;
        setNodes((prev) => prev.map((n) => n.id === d.id ? { ...n, x: d.origX + dx, y: d.origY + dy } : n));
      }
      if (panRef.current) {
        const p = panRef.current;
        setCamera((c) => ({ ...c, x: p.origX + (e.clientX - p.startX), y: p.origY + (e.clientY - p.startY) }));
      }
    };
    const handleMouseUp = () => {
      dragRef.current = null;
      panRef.current = null;
    };
    window.addEventListener("mousemove", handleMouseMove);
    window.addEventListener("mouseup", handleMouseUp);
    return () => {
      window.removeEventListener("mousemove", handleMouseMove);
      window.removeEventListener("mouseup", handleMouseUp);
    };
  }, [camera.zoom]);

  // Zoom
  const handleWheel = useCallback((e) => {
    e.preventDefault();
    const delta = e.deltaY > 0 ? 0.92 : 1.08;
    setCamera((c) => {
      const newZoom = Math.max(0.3, Math.min(2.5, c.zoom * delta));
      const rect = canvasRef.current.getBoundingClientRect();
      const mx = e.clientX - rect.left;
      const my = e.clientY - rect.top;
      return {
        zoom: newZoom,
        x: mx - (mx - c.x) * (newZoom / c.zoom),
        y: my - (my - c.y) * (newZoom / c.zoom),
      };
    });
  }, []);

  useEffect(() => {
    const el = canvasRef.current;
    if (!el) return;
    el.addEventListener("wheel", handleWheel, { passive: false });
    return () => el.removeEventListener("wheel", handleWheel);
  }, [handleWheel]);

  const toggleCat = (name) => {
    setExpandedCats((prev) => prev.includes(name) ? prev.filter((c) => c !== name) : [...prev, name]);
  };

  // Stat bar helper
  const StatPill = ({ icon: Icon, label, value, unit, color }) => (
    <div style={{ display: "flex", alignItems: "center", gap: 4 }}>
      <Icon size={11} color={color || C.textDim} strokeWidth={1.5} />
      <span style={{ color: C.textDim, fontSize: 10, fontFamily: "'JetBrains Mono', monospace" }}>
        {label}
      </span>
      <span style={{ color: C.text, fontSize: 10, fontFamily: "'JetBrains Mono', monospace", fontWeight: 600 }}>
        {typeof value === "number" ? value.toFixed(0) : value}{unit}
      </span>
    </div>
  );

  const GodStatus = ({ name, status, color }) => (
    <div style={{ display: "flex", alignItems: "center", gap: 4 }}>
      <div style={{
        width: 6, height: 6, borderRadius: "50%",
        background: color,
        boxShadow: `0 0 6px ${color}88`,
        animation: status === "active" ? "pulse 2s infinite" : "none",
      }} />
      <span style={{ color: C.textDim, fontSize: 10, fontFamily: "'JetBrains Mono', monospace" }}>
        {name}: <span style={{ color }}>{status}</span>
      </span>
    </div>
  );

  return (
    <div style={{
      width: "100%", height: "100vh",
      display: "flex",
      background: C.bg,
      fontFamily: "'JetBrains Mono', 'SF Mono', monospace",
      color: C.text,
      overflow: "hidden",
      position: "relative",
    }}>
      <link href="https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@300;400;500;600;700&display=swap" rel="stylesheet" />
      <style>{`
        @keyframes pulse {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.4; }
        }
        @keyframes scanline {
          0% { transform: translateY(-100%); }
          100% { transform: translateY(100vh); }
        }
        input:focus { border-color: ${C.accent} !important; background: rgba(0,212,170,0.05) !important; }
        ::-webkit-scrollbar { width: 4px; }
        ::-webkit-scrollbar-track { background: transparent; }
        ::-webkit-scrollbar-thumb { background: ${C.nodeBorder}; border-radius: 2px; }
      `}</style>

      {/* CRT scanline overlay */}
      <div style={{
        position: "fixed", inset: 0, pointerEvents: "none", zIndex: 999,
        background: "repeating-linear-gradient(0deg, rgba(0,0,0,0.03) 0px, rgba(0,0,0,0.03) 1px, transparent 1px, transparent 3px)",
        opacity: 0.5,
      }} />

      {/* ── Sidebar ── */}
      <div style={{
        width: 200,
        background: C.sidebar,
        borderRight: `1px solid ${C.nodeBorder}`,
        display: "flex",
        flexDirection: "column",
        flexShrink: 0,
      }}>
        {/* Logo */}
        <div style={{
          padding: "14px 12px",
          borderBottom: `1px solid ${C.nodeBorder}`,
          display: "flex",
          alignItems: "center",
          gap: 8,
        }}>
          <div style={{
            width: 28, height: 28,
            border: `1.5px solid ${C.accent}`,
            borderRadius: 6,
            display: "flex", alignItems: "center", justifyContent: "center",
            background: C.accentDim,
          }}>
            <Zap size={15} color={C.accent} strokeWidth={2} />
          </div>
          <div>
            <div style={{ fontSize: 13, fontWeight: 700, color: C.textBright, letterSpacing: 2 }}>NDNM</div>
            <div style={{ fontSize: 8, color: C.textDim, letterSpacing: 1.5 }}>NO DEPS NO MASTERS</div>
          </div>
        </div>

        {/* Categories */}
        <div style={{ flex: 1, overflowY: "auto", paddingTop: 8 }}>
          {CATEGORIES.map((cat) => (
            <SidebarCategory
              key={cat.name}
              cat={cat}
              expanded={expandedCats.includes(cat.name)}
              onToggle={() => toggleCat(cat.name)}
            />
          ))}
        </div>

        {/* Queue button */}
        <div style={{ padding: 12, borderTop: `1px solid ${C.nodeBorder}` }}>
          <button
            onClick={runQueue}
            disabled={queueState === "running"}
            style={{
              width: "100%",
              padding: "10px 0",
              background: queueState === "running"
                ? "rgba(0,212,170,0.1)"
                : queueState === "done"
                ? "rgba(0,212,170,0.2)"
                : C.accentDim,
              border: `1px solid ${C.accent}`,
              borderRadius: 6,
              color: C.accent,
              fontSize: 12,
              fontWeight: 600,
              letterSpacing: 1.5,
              cursor: queueState === "running" ? "wait" : "pointer",
              fontFamily: "'JetBrains Mono', monospace",
              transition: "all 0.2s",
              position: "relative",
              overflow: "hidden",
            }}
          >
            {queueState === "running" && (
              <div style={{
                position: "absolute", left: 0, top: 0, bottom: 0,
                width: `${queueProgress * 100}%`,
                background: "rgba(0,212,170,0.15)",
                transition: "width 0.1s linear",
              }} />
            )}
            <span style={{ position: "relative", zIndex: 1, display: "flex", alignItems: "center", justifyContent: "center", gap: 6 }}>
              <PlayCircle size={14} />
              {queueState === "idle" ? "QUEUE PROMPT" : queueState === "running" ? `${(queueProgress * 100).toFixed(0)}%` : "DONE ✓"}
            </span>
          </button>
        </div>

        {/* System stats */}
        <div style={{ padding: "8px 12px", borderTop: `1px solid ${C.nodeBorder}` }}>
          <div style={{ fontSize: 8, color: C.textDim, letterSpacing: 1.5, marginBottom: 6, textTransform: "uppercase" }}>System</div>
          <div style={{ display: "grid", gridTemplateColumns: "1fr 1fr", gap: 4 }}>
            <StatPill icon={Cpu} label="CPU" value={stats.cpu} unit="%" color={stats.cpu > 80 ? "#ff4444" : C.accent} />
            <StatPill icon={MemoryStick} label="RAM" value={stats.ram} unit="%" color={stats.ram > 85 ? "#ff4444" : "#4488ff"} />
            <StatPill icon={Activity} label="VRAM" value={stats.vram} unit="%" color={stats.vram > 80 ? "#ff4444" : "#aa66ff"} />
            <StatPill icon={Thermometer} label="GPU" value={stats.temp} unit="°C" color={stats.temp > 80 ? "#ff4444" : "#ffcc00"} />
          </div>
        </div>
      </div>

      {/* ── Canvas ── */}
      <div style={{ flex: 1, display: "flex", flexDirection: "column" }}>
        <div
          ref={canvasRef}
          onMouseDown={handleCanvasMouseDown}
          style={{
            flex: 1,
            position: "relative",
            overflow: "hidden",
            background: C.canvas,
            backgroundImage: `radial-gradient(circle, ${C.gridDot} 1px, transparent 1px)`,
            backgroundSize: `${24 * camera.zoom}px ${24 * camera.zoom}px`,
            backgroundPosition: `${camera.x}px ${camera.y}px`,
            cursor: panRef.current ? "grabbing" : "default",
          }}
        >
          {/* SVG wires */}
          <svg
            style={{ position: "absolute", inset: 0, width: "100%", height: "100%", pointerEvents: "none" }}
          >
            {WIRE_DEFS.map((w, i) => (
              <Wire key={i} wire={w} nodes={nodes} camera={camera} progress={queueState === "running" ? queueProgress : 0} />
            ))}
          </svg>

          {/* Nodes */}
          {nodes.map((node) => (
            <Node
              key={node.id}
              node={node}
              isSelected={selected === node.id}
              onSelect={setSelected}
              onDragStart={handleDragStart}
              camera={camera}
            />
          ))}

          {/* Preview placeholder on save node when done */}
          {queueState === "done" && (() => {
            const saveNode = nodes.find((n) => n.id === "save");
            if (!saveNode) return null;
            const nodeH = 36 + Math.max(saveNode.fields.length, Math.max(saveNode.inputs.length, saveNode.outputs.length)) * 28 + 20;
            return (
              <div style={{
                position: "absolute",
                left: saveNode.x * camera.zoom + camera.x + 8 * camera.zoom,
                top: (saveNode.y + nodeH + 8) * camera.zoom + camera.y,
                width: (saveNode.w - 16) * camera.zoom,
                height: 80 * camera.zoom,
                background: "rgba(0,212,170,0.08)",
                border: `1px solid ${C.accent}44`,
                borderRadius: 4 * camera.zoom,
                display: "flex",
                alignItems: "center",
                justifyContent: "center",
                color: C.accent,
                fontSize: 20 * camera.zoom,
                fontWeight: 700,
              }}>
                ✓ 512×512
              </div>
            );
          })()}
        </div>

        {/* ── Status Bar ── */}
        <div style={{
          height: 32,
          background: C.statusBar,
          borderTop: `1px solid ${C.nodeBorder}`,
          display: "flex",
          alignItems: "center",
          justifyContent: "space-between",
          padding: "0 16px",
        }}>
          <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
            <span style={{ color: C.textDim, fontSize: 10, fontFamily: "'JetBrains Mono', monospace" }}>
              <span style={{ color: C.accent }}>●</span> {nodes.length} nodes
            </span>
            <span style={{ color: C.textDim, fontSize: 10, fontFamily: "'JetBrains Mono', monospace" }}>
              <span style={{ color: C.CLIP }}>●</span> {WIRE_DEFS.length} wires
            </span>
            <span style={{ color: C.textDim, fontSize: 10, fontFamily: "'JetBrains Mono', monospace" }}>
              zoom: {(camera.zoom * 100).toFixed(0)}%
            </span>
          </div>
          <div style={{ display: "flex", alignItems: "center", gap: 16 }}>
            <GodStatus name="Hermes" status={queueState === "running" ? "routing" : "ready"} color={queueState === "running" ? "#ffcc00" : C.accent} />
            <GodStatus name="Atlas" status={queueState === "done" ? "writing" : "ready"} color={queueState === "done" ? "#4488ff" : C.accent} />
            <GodStatus name="Argus" status="logging" color="#ffcc00" />
          </div>
        </div>
      </div>
    </div>
  );
}
