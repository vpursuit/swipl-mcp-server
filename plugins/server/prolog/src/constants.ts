/**
 * Constants for the MCP Prolog plugin
 */

import path from "path";
import os from "os";

// Input validation limits
export const MAX_QUERY_LENGTH = 5000;
export const MAX_FILENAME_LENGTH = 1000;
export const MAX_FACT_LENGTH = 5000;

// Timeout configurations (in milliseconds)
export const DEFAULT_QUERY_TIMEOUT_MS = 30000; // 30 seconds
export const DEFAULT_READY_TIMEOUT_MS = 5000;  // 5 seconds
export const CLEANUP_TIMEOUT_MS = 5000;        // 5 seconds for cleanup operations

// Buffer management
export const MAX_BUFFER_SIZE = 1024 * 1024; // 1MB
export const MAX_QUERY_PROMISES = 100;

// Security: Default allowed directory for file operations
export const ALLOWED_DIR = path.join(os.homedir(), '.model-context-lab');

// Prolog server markers and protocol strings
export const READY_MARKER = "@@READY@@";
export const READY_MARK = "@@READY@@"; // Alias for compatibility
export const SOLUTION_PREFIX = "solution(";
export const TERM_SOLUTION = "solution("; // Alias for compatibility
export const NO_SOLUTION = "no";
export const ERROR_PREFIX = "error(";
export const TERM_ERROR = "error("; // Alias for compatibility
export const NO_MORE_SOLUTIONS = "no_more_solutions";

// Process management
export const STOP_KILL_DELAY_MS = 50; // 50ms delay before SIGTERM
export const STOP_MAX_WAIT_MS = 3000; // Maximum 3 seconds to wait for process exit before SIGKILL
