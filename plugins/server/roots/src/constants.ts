/**
 * Time-to-live for roots cache (5 minutes)
 */
export const ROOT_CACHE_TTL_MS = 300000; // 5 minutes

/**
 * Timeout for listRoots() MCP call (5 seconds)
 * If client doesn't respond within this time, treat as protocol error
 */
export const LIST_ROOTS_TIMEOUT_MS = 5000; // 5 seconds

/**
 * Default fallback directory (relative to home directory)
 * @deprecated No longer used - explicit root configuration required via MCP client or SWI_MCP_ALLOWED_ROOTS
 */
export const DEFAULT_FALLBACK_DIR = ".model-context-lab";

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
