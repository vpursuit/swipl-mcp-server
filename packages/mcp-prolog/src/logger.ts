import path from "path";

type LogLevel = "debug" | "info" | "warn" | "error" | "silent";

const levelOrder: Record<Exclude<LogLevel, "silent">, number> = {
  debug: 10,
  info: 20,
  warn: 30,
  error: 40,
};

function resolveLevel(): LogLevel {
  const fromEnv = (process.env.MCP_LOG_LEVEL || "").toLowerCase();
  if (fromEnv === "silent") return "silent";
  if (fromEnv in levelOrder) return fromEnv as LogLevel;
  // Fallback: enable debug if DEBUG includes 'swipl-mcp-server'
  const dbg = process.env.DEBUG || "";
  if (dbg.split(",").some((s) => s.trim().includes("swipl-mcp-server"))) return "debug";
  return "warn";
}

let currentLevel: LogLevel = resolveLevel();

function shouldLog(target: Exclude<LogLevel, "silent">): boolean {
  if (currentLevel === "silent") return false;
  const cur = levelOrder[currentLevel as Exclude<LogLevel, "silent">] ?? 9999;
  const tgt = levelOrder[target];
  return cur <= tgt;
}

function fmt(level: string, msg: string) {
  const ts = new Date().toISOString();
  return `[swipl-mcp-server ${level}] ${ts} ${msg}`;
}

export const logger = {
  setLevel(lvl: LogLevel) {
    currentLevel = lvl;
  },
  debug(msg: string) {
    if (shouldLog("debug")) console.error(fmt("debug", msg));
  },
  info(msg: string) {
    if (shouldLog("info")) console.error(fmt("info", msg));
  },
  warn(msg: string) {
    if (shouldLog("warn")) console.error(fmt("warn", msg));
  },
  error(msg: string) {
    if (shouldLog("error")) console.error(fmt("error", msg));
  },
  // Helpers to avoid leaking sensitive details
  redactPath(p: string): string {
    try {
      const rel = path.relative(process.cwd(), p);
      return rel && !rel.startsWith("..") ? rel : path.basename(p);
    } catch {
      return "path";
    }
  },
  redactPid(pid?: number | null): string {
    return pid ? `pid:${String(pid).slice(0, 1)}**` : "pid:***";
  },
};
