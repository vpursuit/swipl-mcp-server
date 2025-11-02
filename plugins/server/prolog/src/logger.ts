/**
 * MCP-aware logger for mcp-prolog package
 * Uses the shared MCP logger infrastructure
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { createMcpLogger, type McpLogger } from "@vpursuit/mcp-server-core";
import path from "path";

// Shared server reference for MCP logging
export const serverRef: { current: McpServer | null } = { current: null };

// Create MCP-aware logger
const mcpLogger: McpLogger = createMcpLogger("mcp-prolog", serverRef);

export const logger = {
  debug(msg: string, data?: Record<string, unknown>) {
    mcpLogger.debug(msg, data);
  },
  info(msg: string, data?: Record<string, unknown>) {
    mcpLogger.info(msg, data);
  },
  warn(msg: string, data?: Record<string, unknown>) {
    mcpLogger.warn(msg, data);
  },
  error(msg: string, errorOrData?: Error | Record<string, unknown>) {
    mcpLogger.error(msg, errorOrData);
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
