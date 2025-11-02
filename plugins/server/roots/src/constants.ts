/**
 * Time-to-live for roots cache (5 minutes)
 */
export const ROOT_CACHE_TTL_MS = 300000; // 5 minutes

/**
 * Default fallback directory (relative to home directory)
 */
export const DEFAULT_FALLBACK_DIR = ".swipl-mcp-server";

/**
 * System directories that should never be accessible
 */
export const BLOCKED_SYSTEM_DIRS = [
  "/etc",
  "/usr",
  "/bin",
  "/sbin",
  "/var",
  "/sys",
  "/proc",
  "/boot",
  "/dev",
  "/root",
  "C:\\Windows",
  "C:\\Program Files",
  "C:\\Program Files (x86)",
] as const;
