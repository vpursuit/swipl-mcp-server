import type { ToolResponse } from "@vpursuit/mcp-core";

/**
 * Create an error response for tool handlers (MCP SDK format)
 */
export function createErrorResponse(
  error: string,
  startTime: number,
  additionalData?: Record<string, unknown>
): ToolResponse {
  const processingTimeMs = Date.now() - startTime;
  return {
    content: [{ type: "text", text: `Error: ${error}\nProcessing time: ${processingTimeMs}ms` }],
    structuredContent: {
      error,
      processing_time_ms: processingTimeMs,
      ...additionalData,
    },
    isError: true,
  };
}

/**
 * Create a success response for tool handlers (MCP SDK format)
 */
export function createSuccessResponse(
  message: string,
  startTime: number,
  data?: Record<string, unknown>
): ToolResponse {
  const processingTimeMs = Date.now() - startTime;
  return {
    content: [{ type: "text", text: `${message}\nProcessing time: ${processingTimeMs}ms` }],
    structuredContent: {
      processing_time_ms: processingTimeMs,
      ...data,
    },
  };
}
