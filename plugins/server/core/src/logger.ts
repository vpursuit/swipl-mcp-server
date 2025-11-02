/**
 * MCP-aware logger that sends logs via the MCP protocol
 */
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";

export type LogLevel =
  | "debug"
  | "info"
  | "notice"
  | "warning"
  | "error"
  | "critical"
  | "alert"
  | "emergency";

export interface McpLogger {
  debug(message: string, data?: Record<string, unknown>): void;
  info(message: string, data?: Record<string, unknown>): void;
  notice(message: string, data?: Record<string, unknown>): void;
  warn(message: string, data?: Record<string, unknown>): void;
  error(message: string, error?: Error | Record<string, unknown>): void;
  critical(message: string, data?: Record<string, unknown>): void;
  alert(message: string, data?: Record<string, unknown>): void;
  emergency(message: string, data?: Record<string, unknown>): void;
}

// Log level hierarchy for filtering
const LOG_LEVELS: Record<LogLevel, number> = {
  debug: 0,
  info: 1,
  notice: 2,
  warning: 3,
  error: 4,
  critical: 5,
  alert: 6,
  emergency: 7,
};

/**
 * Get minimum log level from environment variable
 * Defaults to 'info' if not set or invalid
 */
function getMinLogLevel(): LogLevel {
  const envLevel = process.env.MCP_LOG_LEVEL?.toLowerCase();
  if (envLevel && envLevel in LOG_LEVELS) {
    return envLevel as LogLevel;
  }
  return "info";
}

/**
 * Check if dual logging mode is enabled (MCP + stderr)
 * Enabled when MCP_LOG_LEVEL=debug
 */
function isDualLoggingEnabled(): boolean {
  return process.env.MCP_LOG_LEVEL?.toLowerCase() === "debug";
}

/**
 * Create an MCP-aware logger that sends logs via the MCP protocol
 * Falls back to stderr when server is not available
 * Respects MCP_LOG_LEVEL environment variable for server-side filtering
 */
export function createMcpLogger(
  loggerName: string,
  serverRef?: { current: McpServer | null }
): McpLogger {
  const minLevel = getMinLogLevel();
  const minLevelValue = LOG_LEVELS[minLevel];
  const dualLogging = isDualLoggingEnabled();

  const log = async (
    level: LogLevel,
    message: string,
    data?: Record<string, unknown> | Error
  ) => {
    // Server-side log level filtering
    if (LOG_LEVELS[level] < minLevelValue) {
      return;
    }

    const server = serverRef?.current;

    // Convert Error to data object
    const logData =
      data instanceof Error
        ? {
            error: data.message,
            stack: data.stack,
            name: data.name,
          }
        : data || {};

    // Format for stderr output
    const stderrMessage = `[${loggerName}] ${level.toUpperCase()}: ${message}`;
    const hasData = Object.keys(logData).length > 0;

    if (server?.isConnected()) {
      try {
        // Send via MCP protocol
        await server.sendLoggingMessage({
          level,
          logger: loggerName,
          data: {
            message,
            ...logData,
          },
        });

        // Also log to stderr in dual logging mode (debug level)
        if (dualLogging) {
          if (hasData) {
            console.error(stderrMessage, logData);
          } else {
            console.error(stderrMessage);
          }
        }
      } catch (err) {
        // Fallback to stderr if MCP logging fails
        console.error(stderrMessage, hasData ? logData : "", err);
      }
    } else {
      // Fallback to stderr when server not connected
      if (hasData) {
        console.error(stderrMessage, logData);
      } else {
        console.error(stderrMessage);
      }
    }
  };

  return {
    debug: (message, data) => log("debug", message, data),
    info: (message, data) => log("info", message, data),
    notice: (message, data) => log("notice", message, data),
    warn: (message, data) => log("warning", message, data),
    error: (message, errorOrData) => log("error", message, errorOrData),
    critical: (message, data) => log("critical", message, data),
    alert: (message, data) => log("alert", message, data),
    emergency: (message, data) => log("emergency", message, data),
  };
}
