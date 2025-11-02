import type { ToolResponse } from "@vpursuit/mcp-server-core";

/**
 * Result of a clause operation (assert/retract)
 */
export interface ClauseOperationResult {
  clause: string;
  status: "success" | "error";
  message: string;
}

/**
 * Truncate long clauses for display
 */
function truncateClause(clause: string, maxLength = 80): string {
  if (clause.length <= maxLength) return clause;

  const halfLen = Math.floor((maxLength - 5) / 2);
  return clause.slice(0, halfLen) + " ... " + clause.slice(-halfLen);
}

/**
 * Format clause operation results for human and machine consumption
 */
export function formatClauseResults(
  operation: "assert" | "retract",
  results: ClauseOperationResult[],
  processingTimeMs: number
): { text: string; structured: Record<string, unknown> } {
  const total = results.length;
  const succeeded = results.filter(r => r.status === "success").length;
  const failed = total - succeeded;

  // Build structured data with backward compatibility fields
  const structured: Record<string, unknown> = {
    summary: {
      total,
      succeeded,
      failed,
      processing_time_ms: processingTimeMs,
    },
    results: results.map(r => ({
      clause: r.clause,
      status: r.status,
      message: r.message,
    })),
    // Backward compatibility fields
    success: succeeded,
    total: total,
    processing_time_ms: processingTimeMs,
  };

  // For single-clause operations, add the result directly for backward compatibility
  if (results.length === 1) {
    structured.result = results[0].message;
    structured.fact = results[0].clause;
  }

  // Build human-readable text
  const lines: string[] = [];

  // Header with summary
  if (failed === 0) {
    lines.push(`✅ ${operation.toUpperCase()} RESULTS: ${succeeded}/${total} successful`);
  } else if (succeeded === 0) {
    lines.push(`❌ ${operation.toUpperCase()} RESULTS: 0/${total} successful`);
  } else {
    lines.push(`⚠️  ${operation.toUpperCase()} RESULTS: ${succeeded}/${total} successful`);
  }
  lines.push("");

  // Group by status
  const successResults = results.filter(r => r.status === "success");
  const errorResults = results.filter(r => r.status === "error");

  // Show success section
  if (successResults.length > 0) {
    lines.push(`SUCCESS (${successResults.length}):`);
    for (const result of successResults) {
      const displayClause = truncateClause(result.clause);
      lines.push(`  • ${displayClause}`);
    }
    if (errorResults.length > 0) lines.push("");
  }

  // Show error section
  if (errorResults.length > 0) {
    lines.push(`FAILED (${errorResults.length}):`);
    for (const result of errorResults) {
      const displayClause = truncateClause(result.clause);
      lines.push(`  ✗ ${displayClause}`);
      if (result.message !== "error") {
        lines.push(`    Reason: ${result.message}`);
      }
    }
  }

  // Footer with timing
  lines.push("");
  lines.push(`⏱️  ${processingTimeMs}ms`);

  return {
    text: lines.join("\n"),
    structured,
  };
}

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
