// Centralized validation constants
export const MAX_QUERY_LENGTH = 5000;
export const MAX_FACT_LENGTH = 5000;
export const MAX_FILENAME_LENGTH = 1000;

// Protocol string constants
export const READY_MARK = "@@READY@@";
export const TERM_SOLUTION = "solution(";
export const TERM_ERROR = "error(";
export const NO_MORE_SOLUTIONS = "no_more_solutions";

// Buffer and performance constants
export const MAX_BUFFER_SIZE = 1024 * 1024; // 1MB
export const DEFAULT_QUERY_TIMEOUT_MS = 30000; // 30 seconds
export const DEFAULT_READY_TIMEOUT_MS = 5000; // 5 seconds
export const STOP_KILL_DELAY_MS = 50; // 50ms

// Roots management constants
export const ROOT_CACHE_TTL_MS = 300000; // 5 minutes
export const DEFAULT_FALLBACK_DIR = ".swipl-mcp-server"; // Relative to home directory

// Security: System directories that should never be accessible
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
  "/System",      // macOS
  "/Library",     // macOS system library
  "/Applications", // macOS applications
  "C:\\Windows",  // Windows
  "C:\\Program Files", // Windows
];
