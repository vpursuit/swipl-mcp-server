// Centralized validation constants
export const MAX_QUERY_LENGTH = 5000;
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
