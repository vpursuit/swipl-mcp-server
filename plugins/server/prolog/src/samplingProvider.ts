/**
 * Sampling provider reference for MCP sampling operations
 *
 * This module provides a shared reference to the sampling provider that can be
 * used by tools (especially explain_error) without creating circular dependencies.
 *
 * For stdio transport, sampling is handled via MCP protocol using McpServer.server.createMessage().
 * The provider is initialized during plugin startup.
 */

import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import {
  type SamplingProvider,
  NoOpSamplingProvider,
  McpProtocolSamplingProvider,
} from "@vpursuit/mcp-server-sampling";
import { logger } from "./logger.js";
import { DEFAULT_ERROR_ANALYSIS_TIMEOUT_MS } from "./constants.js";

/**
 * Shared sampling provider reference
 *
 * Initialized with NoOpSamplingProvider (sampling unavailable by default).
 * Call initializeSamplingProvider() during plugin initialization to enable
 * MCP protocol-based sampling.
 */
export const samplingProviderRef: { current: SamplingProvider } = {
  current: new NoOpSamplingProvider(),
};

/**
 * Initialize sampling provider with McpServer instance
 *
 * This enables the explain_error tool to use MCP protocol sampling.
 * The client's sampling handler will be invoked automatically via the protocol.
 *
 * Timeout priority (highest to lowest):
 * 1. SWI_MCP_ERROR_ANALYSIS_TIMEOUT_MS environment variable
 * 2. Explicit timeoutMs parameter
 * 3. DEFAULT_ERROR_ANALYSIS_TIMEOUT_MS constant (60000ms)
 *
 * @param mcpServer - The MCP server instance
 * @param timeoutMs - Optional timeout for sampling requests
 */
export function initializeSamplingProvider(
  mcpServer: McpServer,
  timeoutMs?: number
): void {
  // Read timeout from environment variable with fallback
  const envTimeout = Number.parseInt(
    process.env['SWI_MCP_ERROR_ANALYSIS_TIMEOUT_MS'] ?? "",
    10
  );

  const timeout = Number.isFinite(envTimeout) && envTimeout > 0
    ? envTimeout
    : (timeoutMs ?? DEFAULT_ERROR_ANALYSIS_TIMEOUT_MS);

  logger.info(`Initializing MCP protocol sampling provider with ${timeout}ms timeout`);
  samplingProviderRef.current = new McpProtocolSamplingProvider(mcpServer, timeout);
  logger.info("Sampling provider initialized - will use MCP protocol");
}
