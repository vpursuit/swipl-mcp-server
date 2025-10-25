/**
 * Simple logger for mcp-roots package
 */
export const logger = {
  info: (message: string) => console.log(`[mcp-roots] ${message}`),
  error: (message: string, error?: Error) =>
    console.error(`[mcp-roots] ERROR: ${message}`, error),
  warn: (message: string) => console.warn(`[mcp-roots] WARNING: ${message}`),
  debug: (message: string) => {
    if (process.env.DEBUG === "mcp-roots" || process.env.DEBUG === "*") {
      console.log(`[mcp-roots] DEBUG: ${message}`);
    }
  },
};
