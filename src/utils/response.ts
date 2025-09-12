type ToolResponse = {
  content: any[];
  structuredContent: Record<string, unknown>;
  isError?: boolean;
};

interface ToolResult {
  success: boolean;
  data?: any;
  error?: string;
}

export function createToolResponse(
  result: ToolResult,
  startTime: number
): ToolResponse {
  const processingTimeMs = Date.now() - startTime;
  
  if (result.success && result.data !== undefined) {
    return {
      content: [{ type: "text", text: `${result.data}\nProcessing time: ${processingTimeMs}ms` }],
      structuredContent: { 
        ...result.data,
        processing_time_ms: processingTimeMs 
      },
      isError: false,
    };
  }
  
  if (!result.success && result.error) {
    return {
      content: [{ type: "text", text: `Error: ${result.error}\nProcessing time: ${processingTimeMs}ms` }],
      structuredContent: { 
        error: result.error, 
        processing_time_ms: processingTimeMs 
      },
      isError: true,
    };
  }
  
  // Default success case
  return {
    content: [{ type: "text", text: `Processing time: ${processingTimeMs}ms` }],
    structuredContent: { processing_time_ms: processingTimeMs },
    isError: false,
  };
}

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
      ...additionalData
    },
    isError: true,
  };
}

export function createSuccessResponse(
  message: string,
  startTime: number,
  structuredData?: Record<string, unknown>
): ToolResponse {
  const processingTimeMs = Date.now() - startTime;
  return {
    content: [{ type: "text", text: `${message}\nProcessing time: ${processingTimeMs}ms` }],
    structuredContent: { 
      processing_time_ms: processingTimeMs,
      ...structuredData
    },
    isError: false,
  };
}