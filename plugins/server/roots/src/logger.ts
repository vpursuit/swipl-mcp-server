/**
 * MCP-aware logger for mcp-roots package
 * Uses the shared MCP logger infrastructure
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { createMcpLogger, type McpLogger } from "@vpursuit/mcp-server-core";

// Shared server reference for MCP logging
export const serverRef: { current: McpServer | null } = { current: null };

// Create MCP-aware logger
const mcpLogger: McpLogger = createMcpLogger("mcp-roots", serverRef);

export const logger = {
  info: (message: string, data?: Record<string, unknown>) =>
    mcpLogger.info(message, data),
  error: (message: string, errorOrData?: Error | Record<string, unknown>) =>
    mcpLogger.error(message, errorOrData),
  warn: (message: string, data?: Record<string, unknown>) =>
    mcpLogger.warn(message, data),
  debug: (message: string, data?: Record<string, unknown>) =>
    mcpLogger.debug(message, data),
};
