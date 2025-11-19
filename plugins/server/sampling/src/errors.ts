/**
 * Structured error types for MCP sampling operations
 *
 * These error types provide detailed context about sampling failures,
 * enabling better error handling and debugging.
 */

/**
 * Error codes for sampling operations
 */
export enum SamplingErrorCode {
  NOT_AVAILABLE = "SAMPLING_NOT_AVAILABLE",
  REQUEST_FAILED = "SAMPLING_REQUEST_FAILED",
  TIMEOUT = "SAMPLING_TIMEOUT",
  PARSE_ERROR = "SAMPLING_PARSE_ERROR",
  INVALID_RESPONSE = "SAMPLING_INVALID_RESPONSE",
  CALLBACK_ERROR = "SAMPLING_CALLBACK_ERROR",
}

/**
 * Base interface for sampling errors
 */
export interface SamplingError {
  code: SamplingErrorCode;
  message: string;
  details?: unknown;
}

/**
 * Error when sampling is not available
 */
export class SamplingNotAvailableError extends Error implements SamplingError {
  code = SamplingErrorCode.NOT_AVAILABLE as const;
  details?: unknown;

  constructor(message: string = "Sampling is not available", details?: unknown) {
    super(message);
    this.name = "SamplingNotAvailableError";
    this.details = details;
  }
}

/**
 * Error when sampling request fails
 */
export class SamplingRequestFailedError extends Error implements SamplingError {
  code = SamplingErrorCode.REQUEST_FAILED as const;
  details?: unknown;

  constructor(message: string, details?: unknown) {
    super(message);
    this.name = "SamplingRequestFailedError";
    this.details = details;
  }
}

/**
 * Error when sampling request times out
 */
export class SamplingTimeoutError extends Error implements SamplingError {
  code = SamplingErrorCode.TIMEOUT as const;
  details?: unknown;

  constructor(timeoutMs: number, details?: unknown) {
    super(`Sampling request timed out after ${timeoutMs}ms`);
    this.name = "SamplingTimeoutError";
    this.details = details;
  }
}

/**
 * Error when parsing sampling response fails
 */
export class SamplingParseError extends Error implements SamplingError {
  code = SamplingErrorCode.PARSE_ERROR as const;
  details?: unknown;

  constructor(message: string, details?: unknown) {
    super(message);
    this.name = "SamplingParseError";
    this.details = details;
  }
}

/**
 * Error when sampling response is invalid
 */
export class SamplingInvalidResponseError extends Error implements SamplingError {
  code = SamplingErrorCode.INVALID_RESPONSE as const;
  details?: unknown;

  constructor(message: string, details?: unknown) {
    super(message);
    this.name = "SamplingInvalidResponseError";
    this.details = details;
  }
}

/**
 * Error when callback execution fails
 */
export class SamplingCallbackError extends Error implements SamplingError {
  code = SamplingErrorCode.CALLBACK_ERROR as const;
  details?: unknown;

  constructor(message: string, details?: unknown) {
    super(message);
    this.name = "SamplingCallbackError";
    this.details = details;
  }
}

/**
 * Type guard to check if an error is a SamplingError
 */
export function isSamplingError(error: unknown): error is SamplingError {
  return (
    error !== null &&
    typeof error === "object" &&
    "code" in error &&
    Object.values(SamplingErrorCode).includes((error as any).code)
  );
}

/**
 * Convert any error to a SamplingError with structured format
 */
export function toSamplingError(error: unknown): SamplingError {
  if (isSamplingError(error)) {
    return error;
  }

  if (error instanceof Error) {
    return {
      code: SamplingErrorCode.REQUEST_FAILED,
      message: error.message,
      details: { stack: error.stack },
    };
  }

  return {
    code: SamplingErrorCode.REQUEST_FAILED,
    message: String(error),
    details: error,
  };
}
