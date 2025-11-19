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

    // Add tool usage guidance (updated for current unified tool architecture)
    const toolGuidance = `
## Tool Usage Context

The swipl-mcp-server provides these unified tools:

### Core Tools
- **clauses**: Manage facts/rules
  - operation="assert": Add facts/rules to KB
  - operation="retract": Remove clauses from KB

- **files**: Import Prolog files and libraries
  - operation="import": Load a library (e.g., filename="clpfd" for library(clpfd))
  - operation="unimport": Unload a library
  - operation="list": Show loaded files

- **query**: Execute Prolog queries
  - operation="start": Begin a query (use use_engine=true for CLP(FD), false for standard)
  - operation="next": Get next solution from active query
  - operation="close": Close active query/engine

- **workspace**: Manage knowledge base state
  - operation="snapshot": View current KB dump
  - operation="reset": Clear all facts/rules
  - operation="list_symbols": List all defined predicates

- **capabilities**: View server capabilities and metadata

- **explain_error**: Get detailed error explanations (this tool)

### Common Tool Usage Patterns
1. **Load library → assert facts → query**:
   - files (import "clpfd") → clauses (assert rules) → query (start with use_engine=true)

2. **Clear KB → reload facts → retry query**:
   - workspace (reset) → clauses (assert) → query (start)

3. **Verify predicates exist**:
   - workspace (list_symbols) to see all defined predicates

4. **Inspect KB state**:
   - workspace (snapshot) to see current facts/rules

### Tool-Specific Guidance
Include tool usage guidance in your response if the error relates to:
- **Missing predicates**: Suggest workspace tool with operation="list_symbols"
- **Library issues**: Suggest files tool with operation="import" (filename should be just library name, no 'library()' wrapper)
- **Syntax errors in clauses**: Explain correct format for clauses tool (plain strings, not arrays)
- **Query state issues**: Explain query lifecycle (start → next → close) and use_engine parameter
- **KB state issues**: Suggest workspace tool with operation="snapshot" or operation="reset"
`;

    return basePrompt + toolGuidance;
  }
}

/**
 * Singleton instance for reuse
 */
export const prologErrorExplainer = new PrologErrorExplainer();
