/**
 * Centralized logging utility for MCP Host client operations
 *
 * Provides log level control to reduce duplicate and verbose logging.
 * Set MCP_LOG_LEVEL environment variable to control verbosity:
 * - ERROR: Only errors
 * - WARN: Warnings and errors
 * - INFO: Informational messages, warnings, and errors (default)
 * - DEBUG: Debug info, info, warnings, and errors
 * - TRACE: Full protocol dumps and all other logs
 */

export enum LogLevel {
  ERROR = 0,
  WARN = 1,
  INFO = 2,
  DEBUG = 3,
  TRACE = 4,
}

export class Logger {
  private level: LogLevel;

  constructor(private prefix: string = "") {
    const envLevel = process.env.MCP_LOG_LEVEL?.toUpperCase();
    this.level = this.parseLogLevel(envLevel);
  }

  private parseLogLevel(level?: string): LogLevel {
    switch (level) {
      case "ERROR":
        return LogLevel.ERROR;
      case "WARN":
        return LogLevel.WARN;
      case "INFO":
        return LogLevel.INFO;
      case "DEBUG":
        return LogLevel.DEBUG;
      case "TRACE":
        return LogLevel.TRACE;
      default:
        return LogLevel.INFO; // Default to INFO
    }
  }

  private log(level: LogLevel, levelName: string, ...args: any[]): void {
    if (this.level >= level) {
      const prefix = this.prefix ? `[${this.prefix}] ` : "";
      console.log(`${prefix}${levelName}:`, ...args);
    }
  }

  error(...args: any[]): void {
    this.log(LogLevel.ERROR, "ERROR", ...args);
  }

  warn(...args: any[]): void {
    this.log(LogLevel.WARN, "WARN", ...args);
  }

  info(...args: any[]): void {
    this.log(LogLevel.INFO, "INFO", ...args);
  }

  debug(...args: any[]): void {
    this.log(LogLevel.DEBUG, "DEBUG", ...args);
  }

  trace(...args: any[]): void {
    this.log(LogLevel.TRACE, "TRACE", ...args);
  }

  /**
   * Check if a log level is enabled
   */
  isLevelEnabled(level: LogLevel): boolean {
    return this.level >= level;
  }
}

/**
 * Default logger for MCP Host operations
 */
export const mcpHostLogger = new Logger("MCPHost");
