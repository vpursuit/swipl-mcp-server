/**
 * Prolog-specific error explanation using MCP sampling
 */

import { ErrorExplainerBase } from "@vpursuit/mcp-server-sampling";
import type { PrologError } from "./PrologInterface.js";
import { PrologErrorKind } from "./PrologInterface.js";
import { getErrorKnowledge } from "./errorKnowledge.js";

/**
 * Prolog error explainer using domain-specific knowledge
 */
export class PrologErrorExplainer extends ErrorExplainerBase {
  /**
   * System prompt for Prolog expertise
   */
  protected getSystemPrompt(): string {
    return `You are an expert SWI-Prolog developer and teacher with deep knowledge of:
- Logic programming and Prolog semantics
- SWI-Prolog built-in predicates and libraries
- Constraint solving with CLP(FD)
- Common Prolog pitfalls and best practices
- Knowledge base management and query optimization
- MCP tool usage patterns for Prolog servers

Your goal is to provide clear, accurate, and actionable error explanations that help
users understand what went wrong and how to fix it. Focus on the specific error type
and provide concrete examples when possible.`;
  }

  /**
   * Format a Prolog error into a structured description
   */
  protected formatError(error: unknown): string {
    if (this.isPrologError(error)) {
      let formatted = `Error Kind: ${error.kind}\n`;
      formatted += `Message: ${error.message}\n`;

      if (error.details) {
        formatted += `\nDetails:\n`;
        if (error.details.predicate) {
          formatted += `  - Predicate: ${error.details.predicate}\n`;
        }
        if (error.details.file) {
          formatted += `  - File: ${error.details.file}\n`;
        }
        if (error.details.operation) {
          formatted += `  - Operation: ${error.details.operation}\n`;
        }
        if (error.details.goal) {
          formatted += `  - Goal: ${error.details.goal}\n`;
        }
        if (error.details.timeoutMs) {
          formatted += `  - Timeout: ${error.details.timeoutMs}ms\n`;
        }
        if (error.details.raw) {
          formatted += `  - Raw: ${error.details.raw}\n`;
        }
      }

      return formatted;
    }

    // Fallback for non-structured errors
    if (error instanceof Error) {
      return `Error: ${error.message}`;
    }

    return `Error: ${String(error)}`;
  }

  /**
   * Get domain-specific knowledge for this error
   */
  protected getDomainKnowledge(error: unknown): string {
    if (this.isPrologError(error)) {
      const knowledge = getErrorKnowledge(error.kind);
      let formatted = `## Error Type: ${error.kind}\n\n`;
      formatted += `### Description\n${knowledge.description}\n\n`;
      formatted += `### Common Patterns\n${knowledge.examples}`;
      return formatted;
    }

    return "";
  }

  /**
   * Type guard for PrologError
   */
  private isPrologError(error: unknown): error is PrologError {
    return (
      typeof error === "object" &&
      error !== null &&
      "kind" in error &&
      "message" in error &&
      typeof (error as PrologError).kind === "string" &&
      typeof (error as PrologError).message === "string"
    );
  }

  /**
   * Build enhanced prompt with tool usage context
   */
  protected buildPrompt(request: { error: unknown; context?: string; domainKnowledge?: string }): string {
    const basePrompt = super.buildPrompt(request);

    // Add tool usage guidance
    const toolGuidance = `
## Tool Usage Context

The swipl-mcp-server provides these tools:
- knowledge_base_assert / knowledge_base_assert_many: Add facts/rules
- knowledge_base_load: Load Prolog files
- knowledge_base_load_library: Load safe libraries (clpfd, lists, etc.)
- knowledge_base_clear: Clear all facts/rules
- knowledge_base_dump: View current KB state
- symbols_list: List defined predicates
- query_start: Start query (standard mode with call_nth/2)
- query_startEngine: Start query (engine mode)
- query_next: Get next solution
- query_close: Close active query/engine

Common tool usage patterns:
1. Load library → assert facts → query
2. Clear KB → reload facts → retry query
3. Use symbols_list to verify predicates exist
4. Use knowledge_base_dump to see current state

Include tool usage guidance in your response if the error relates to:
- Missing predicates (suggest checking with symbols_list)
- Library issues (suggest knowledge_base_load_library)
- Query state issues (explain query lifecycle)
- KB state issues (suggest knowledge_base_dump or clear)
`;

    return basePrompt + toolGuidance;
  }
}

/**
 * Singleton instance for reuse
 */
export const prologErrorExplainer = new PrologErrorExplainer();
