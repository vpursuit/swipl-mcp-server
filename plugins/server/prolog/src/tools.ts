import { readFile } from "fs/promises";
import path from "path";
import os from "os";
import type { ToolDefinitions, CallToolResult, ToolDefinition } from "@vpursuit/mcp-server-core";
import { PrologInterface } from "./PrologInterface.js";
import type { CapabilitiesSummary } from "./types.js";
import { RootsManager } from "@vpursuit/mcp-server-roots";
import {
  MAX_FILENAME_LENGTH,
  MAX_QUERY_LENGTH,
  MAX_FACT_LENGTH,
} from "./constants.js";
import {
  symbolsListOutputSchema,
  queryNextOutputSchema,
  capabilitiesSchema,
  capabilitiesOutputSchema,
  querySchema,
  clausesSchema,
  filesSchema,
  workspaceSchema,
  explainErrorSchema,
  explainErrorOutputSchema,
} from "./schemas.js";
import { resolvePackageVersion, findNearestFile } from "@vpursuit/mcp-server-core";
import { validateStringInput } from "./utils/validation.js";
import { createErrorResponse, createSuccessResponse, formatClauseResults, type ClauseOperationResult } from "./utils/response.js";
import { prologErrorExplainer } from "./errorExplainer.js";
import { serverRef } from "./logger.js";
import { isSamplingSupported } from "@vpursuit/mcp-server-sampling";

// Security: Use RootsManager for dynamic root discovery and validation
async function validateFilePath(filename: string): Promise<{ allowed: boolean; error?: string }> {
  const rootsManager = RootsManager.getInstance();
  const validation = await rootsManager.validatePath(filename);

  return {
    allowed: validation.allowed,
    error: validation.error
  };
}

// Global Prolog interface instance
const prologInterface = new PrologInterface();

// Build a machine-readable summary of capabilities
export function getCapabilitiesSummary(): CapabilitiesSummary {
  const version = resolvePackageVersion();
  const caps: CapabilitiesSummary = {
    server: { name: "swipl-mcp-server", version },
    branding: {
      logo: {
        uri: "reference://logo",
        format: "image/svg+xml",
        description: "Official swipl-mcp-server logo",
      },
    },
    modes: ["standard", "engine"],
    predicates: {
      standard_prolog: "All standard SWI-Prolog predicates available",
      clpfd_available: true,
      clpfd_note: "library(clpfd) available via safe library loading - see SECURITY.md for whitelist",
    },
    tools: {
      core: ["capabilities"],
      knowledge_base: [
        "clauses",
        "files",
        "workspace",
      ],
      query: ["query"],
      analysis: ["explain_error"],
    },
    prompts: {
      domain_examples: [
        "genealogy",
        "scheduling",
        "puzzle",
        "grammar"
      ]
    },
    security: {
      module: "knowledge_base",
      file_restrictions: {
        allowed_directory: "Explicit configuration required (secure by default): MCP client roots or SWI_MCP_ALLOWED_ROOTS=/path/one,/path/two",
        blocked_directories: [
          "/etc",
          "/usr",
          "/bin",
          "/var",
          "/sys",
          "/proc",
          "/boot",
          "/dev",
          "/root",
        ],
        validation: "pre-execution path checking with strict roots validation",
      },
      consult: "facts/rules only; directives rejected",
      model: "enhanced_hybrid",
      dangerous_predicate_blocking: {
        detection: "pre-execution",
        blocked_predicates: [
          "call/1",
          "assert/1",
          "system/1",
          "shell/1",
          "retract/1",
          "abolish/1",
          "halt/1",
        ],
        error_format: "Security Error: Operation blocked - contains dangerous predicate 'X'",
      },
      sandbox_validation: "library(sandbox) validates most built-ins",
      user_predicates: "allowed in knowledge_base/user modules for recursion",
      safe_categories: [
        "arithmetic",
        "lists",
        "term operations",
        "logic",
        "string/atom helpers",
        "comparisons",
        "type checking",
      ],
      blocked_categories: [
        "I/O",
        "OS/process",
        "network",
        "external modules",
        "directives",
        "system files",
      ],
    },
    available_libraries: {
      note: "Load libraries using knowledge_base_load_library tool or via :- use_module(library(...)) in .pl files",
      safe_libraries: [
        "clpfd",
        "lists",
        "apply",
        "aggregate",
        "assoc",
        "pairs",
        "ordsets",
        "clpb",
        "solution_sequences",
        "yall",
        "nb_set",
        "rbtrees",
        "ugraphs",
        "heaps",
        "terms",
        "random",
      ],
      description: "Sandbox-approved libraries for constraint solving, list manipulation, data structures, and more",
    },
    state_model: {
      knowledge_base: "Persistent across all queries until explicitly cleared with knowledge_base_clear",
      libraries: "Persist for entire session, survive knowledge_base_clear, only removed by server restart",
      queries: "Temporary iteration state, auto-closed when starting new query or engine",
      best_practices: [
        "Use knowledge_base_clear before retrying failed queries to ensure clean state",
        "Libraries don't need reloading after clear (idempotent if reloaded)",
        "Use symbols_list or knowledge_base_dump to inspect current state",
        "Clear KB when switching problem domains to avoid predicate conflicts",
      ],
    },
  };
  return caps;
}

/**
 * Tool definitions for Prolog plugin
 */
export const tools: ToolDefinitions = {
  query: {
    title: "Query Prolog",
    description: "Unified tool for Prolog query operations: start queries (standard or engine mode), iterate solutions, close sessions. Query executes against the current persistent knowledge base state, which includes all previously asserted facts, loaded rules, and imported libraries. Use operation='start' with use_engine=false for call_nth/2 pagination (standard mode) or use_engine=true for SWI-Prolog engine mode (required for constraint solving with CLP(FD)). Use operation='next' to iterate solutions, operation='close' to end session.",
    inputSchema: querySchema,
    outputSchema: queryNextOutputSchema,
    handler: async (
      { operation, query, use_engine }: {
        operation: "start" | "next" | "close";
        query?: string;
        use_engine?: boolean;
      },
      _extra
    ): Promise<CallToolResult> => {
      const startTime = Date.now();

      // Handle 'start' operation
      if (operation === "start") {
        // Validate query parameter
        if (!query) {
          return createErrorResponse(
            "query parameter required for 'start' operation",
            startTime,
            { error_code: "missing_query" }
          );
        }

        const v = validateStringInput("query", query, MAX_QUERY_LENGTH);
        if (!v.ok) {
          return createErrorResponse(v.error, startTime, { error_code: v.code });
        }

        try {
          await prologInterface.start();

          if (use_engine) {
            // Engine mode
            const result = await prologInterface.startEngine(query);
            const processingTimeMs = Date.now() - startTime;
            return {
              content: [
                {
                  type: "text",
                  text: `Engine started: ${query}\nStatus: ${result.status}\nEngine ready: ${result.engine_ready}\nProcessing time: ${processingTimeMs}ms`,
                },
              ],
              structuredContent: {
                operation: "start",
                mode: "engine",
                query,
                status: result.status,
                engine_ready: true,
                processing_time_ms: processingTimeMs
              },
            };
          } else {
            // Standard mode
            const result = await prologInterface.startQuery(query);
            return createSuccessResponse(
              `Query started: ${query}\nStatus: ${result.status}`,
              startTime,
              { operation: "start", mode: "standard", query, status: result.status }
            );
          }
        } catch (error) {
          const processingTimeMs = Date.now() - startTime;
          let errMsg = error instanceof Error ? error.message : String(error);
          if (errMsg.startsWith("error(session_conflict(")) {
            if (errMsg.includes("query,engine")) {
              errMsg = "query session is already active";
            } else if (errMsg.includes("engine,query")) {
              errMsg = "engine session is already active";
            }
          }
          return {
            content: [{ type: "text", text: `Error: ${errMsg}\nProcessing time: ${processingTimeMs}ms` }],
            structuredContent: {
              operation: "start",
              error: errMsg,
              processing_time_ms: processingTimeMs
            },
            isError: true,
          };
        }
      }

      // Handle 'next' operation
      if (operation === "next") {
        try {
          await prologInterface.start();

          const sessionState = prologInterface.getSessionState();
          let result;

          if (sessionState === "query" || sessionState === "query_completed") {
            result = await prologInterface.nextSolution();
          } else if (sessionState === "engine" || sessionState === "engine_completed") {
            result = await prologInterface.nextEngine();
          } else {
            return createErrorResponse(
              "No active query session",
              startTime,
              { operation: "next", solution: null, status: "done" }
            );
          }

          const processingTimeMs = Date.now() - startTime;

          if (result.error) {
            return createErrorResponse(
              result.error,
              startTime,
              { operation: "next", solution: null, status: "done" }
            );
          }

          if (result.status === "success") {
            return {
              content: [
                {
                  type: "text",
                  text: `Solution: ${result.solution}\nStatus: ${result.status}\nProcessing time: ${processingTimeMs}ms`,
                },
              ],
              structuredContent: {
                operation: "next",
                solution: result.solution,
                status: result.status,
                processing_time_ms: processingTimeMs,
              },
            };
          } else {
            return {
              content: [
                {
                  type: "text",
                  text: `No more solutions available\nStatus: done\nProcessing time: ${processingTimeMs}ms`
                }
              ],
              structuredContent: {
                operation: "next",
                solution: null,
                status: "done",
                processing_time_ms: processingTimeMs
              },
            };
          }
        } catch (error) {
          const processingTimeMs = Date.now() - startTime;
          return {
            content: [
              {
                type: "text",
                text: `Error: ${error instanceof Error ? error.message : String(error)}\nProcessing time: ${processingTimeMs}ms`,
              },
            ],
            structuredContent: {
              operation: "next",
              solution: null,
              status: "done" as const,
              processing_time_ms: processingTimeMs,
              error: error instanceof Error ? error.message : String(error)
            },
            isError: true,
          };
        }
      }

      // Handle 'close' operation
      if (operation === "close") {
        const sessionState = prologInterface.getSessionState();
        if (sessionState !== "query" && sessionState !== "query_completed" &&
            sessionState !== "engine" && sessionState !== "engine_completed") {
          const processingTimeMs = Date.now() - startTime;
          return {
            content: [
              {
                type: "text",
                text: `No active session to close\nProcessing time: ${processingTimeMs}ms`
              }
            ],
            structuredContent: {
              operation: "close",
              status: "no_active_session",
              processing_time_ms: processingTimeMs
            },
          };
        }

        try {
          const result = (sessionState === "query" || sessionState === "query_completed") ?
            await prologInterface.closeQuery() : await prologInterface.closeEngine();
          const processingTimeMs = Date.now() - startTime;
          const statusText = result.status === "no_active_query" || result.status === "no_active_engine" ? "closed" : result.status;
          return {
            content: [
              {
                type: "text",
                text: `Session ${statusText}\nProcessing time: ${processingTimeMs}ms`
              }
            ],
            structuredContent: {
              operation: "close",
              status: statusText,
              processing_time_ms: processingTimeMs
            },
          };
        } catch (error) {
          const processingTimeMs = Date.now() - startTime;
          return {
            content: [
              {
                type: "text",
                text: `Error: ${error instanceof Error ? error.message : String(error)}\nProcessing time: ${processingTimeMs}ms`
              },
            ],
            structuredContent: {
              operation: "close",
              error: error instanceof Error ? error.message : String(error),
              processing_time_ms: processingTimeMs
            },
            isError: true,
          };
        }
      }

      // Should never reach here if operation is validated by schema
      return createErrorResponse(
        `Unknown operation: ${operation}`,
        startTime,
        { error_code: "invalid_operation" }
      );
    },
  },

  capabilities: {
    title: "Capabilities",
    description: "Get machine-readable summary of available tools, query modes, environment config, and security model",
    inputSchema: capabilitiesSchema,
    outputSchema: capabilitiesOutputSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      const caps = getCapabilitiesSummary();
      const json = JSON.stringify(caps, null, 2);
      return {
        content: [{ type: "text", text: json }],
        structuredContent: caps,
      };
    },
  },

  clauses: {
    title: "Manage Clauses",
    description: "Unified tool to assert, retract, or clear facts/rules in knowledge base. Preserves source formatting and variable names. Use 'assert' to add facts/rules (single or batch), 'retract' to remove matching facts/rules, 'clear' to remove all facts/rules.",
    inputSchema: clausesSchema,
    handler: async (
      { operation, clauses }: { operation: "assert" | "retract" | "clear"; clauses?: string | string[] },
      _extra
    ): Promise<CallToolResult> => {
      const startTime = Date.now();

      // Handle clear operation (no clauses needed)
      if (operation === "clear") {
        try {
          await prologInterface.start();
          await prologInterface.clearWorkspaceWithSource();
          const processingTimeMs = Date.now() - startTime;
          return {
            content: [
              {
                type: "text",
                text: `Knowledge base cleared\nProcessing time: ${processingTimeMs}ms`,
              },
            ],
            structuredContent: {
              operation: "clear",
              processing_time_ms: processingTimeMs,
            },
          };
        } catch (error) {
          return createErrorResponse(
            error instanceof Error ? error.message : String(error),
            startTime
          );
        }
      }

      // For assert/retract, clauses parameter is required
      if (!clauses) {
        return createErrorResponse(
          `clauses parameter required for ${operation} operation`,
          startTime,
          { error_code: "missing_clauses" }
        );
      }

      // Normalize to array
      const clauseArray = Array.isArray(clauses) ? clauses : [clauses];

      if (clauseArray.length === 0) {
        return createErrorResponse("At least one clause required", startTime);
      }

      // Validate each clause
      for (const clause of clauseArray) {
        if (typeof clause !== "string" || clause.length > MAX_FACT_LENGTH) {
          return createErrorResponse(
            `Each clause must be a string with max ${MAX_FACT_LENGTH} characters`,
            startTime,
            { error_code: "clause_too_long" }
          );
        }
      }

      try {
        await prologInterface.start();
        const clauseResults: ClauseOperationResult[] = [];
        let errorCount = 0;

        if (operation === "assert") {
          // Use source-preserving assert
          for (const clause of clauseArray) {
            const result = await prologInterface.assertClauseWithSource(clause, "inline");
            clauseResults.push({
              clause,
              status: result.success ? "success" : "error",
              message: result.error || "ok",
            });
            if (!result.success) errorCount++;
          }
        } else {
          // operation === 'retract'
          // Use source-aware retract
          for (const clause of clauseArray) {
            const success = await prologInterface.retractClauseWithSource(clause);
            clauseResults.push({
              clause,
              status: success ? "success" : "error",
              message: success ? "ok" : "No matching clause found",
            });
            if (!success) errorCount++;
          }
        }

        const processingTimeMs = Date.now() - startTime;
        const formatted = formatClauseResults(operation, clauseResults, processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: errorCount > 0,
        };
      } catch (error) {
        return createErrorResponse(
          error instanceof Error ? error.message : String(error),
          startTime
        );
      }
    },
  },

  files: {
    title: "Manage Files",
    description: "Import/unimport Prolog files with provenance tracking. 'import' loads .pl file and tracks which clauses came from it, 'unimport' removes all clauses from a file, 'list' shows all imported files. Files must be within configured roots.",
    inputSchema: filesSchema,
    handler: async (
      { operation, filename }: {
        operation: "import" | "unimport" | "list";
        filename?: string;
      },
      _extra
    ): Promise<CallToolResult> => {
      const startTime = Date.now();

      // Validate operation-specific parameters
      if (operation === "import" || operation === "unimport") {
        if (!filename) {
          return createErrorResponse(
            `filename parameter required for '${operation}' operation`,
            startTime,
            { error_code: "missing_filename" }
          );
        }

        // Validate filename input
        const v = validateStringInput("filename", filename, MAX_FILENAME_LENGTH);
        if (!v.ok) {
          return createErrorResponse(v.error, startTime, { error_code: v.code });
        }

        // Validate file path
        const pathValidation = await validateFilePath(filename);
        if (!pathValidation.allowed) {
          return createErrorResponse(
            pathValidation.error || "File access denied",
            startTime,
            { error_code: "file_path_violation", blocked_path: filename }
          );
        }
      }

      try {
        await prologInterface.start();

        if (operation === "import") {
          if (!filename) {
            return createErrorResponse(
              "filename parameter required for 'import' operation",
              startTime,
              { error_code: "missing_filename" }
            );
          }

          // Check file exists and is readable
          try {
            await readFile(filename, "utf8");
          } catch (err) {
            const error = err as NodeJS.ErrnoException;
            let errorMessage = `Error: File '${filename}' not found or not readable`;
            let errorCode = "file_error";

            if (error.code === "ENOENT") {
              errorMessage = `Error: File '${filename}' does not exist`;
              errorCode = "file_not_found";
            } else if (error.code === "EACCES") {
              errorMessage = `Error: Permission denied accessing file '${filename}'`;
              errorCode = "permission_denied";
            } else if (error.code === "EISDIR") {
              errorMessage = `Error: '${filename}' is a directory, not a file`;
              errorCode = "is_directory";
            }

            return createErrorResponse(errorMessage, startTime, { error_code: errorCode, filename });
          }

          const result = await prologInterface.importFileWithSource(filename);
          const processingTimeMs = Date.now() - startTime;

          if (result.success) {
            return {
              content: [{
                type: "text",
                text: `Successfully imported file: ${filename}\nClauses added: ${result.clausesAdded}\n${result.errors.length > 0 ? `Warnings: ${result.errors.join(", ")}` : ""}\nProcessing time: ${processingTimeMs}ms`
              }],
              structuredContent: {
                operation: "import",
                file: filename,
                clauses_added: result.clausesAdded,
                errors: result.errors,
                processing_time_ms: processingTimeMs
              },
              isError: false
            };
          } else {
            return createErrorResponse(
              `Error importing file '${filename}': ${result.errors.join(", ")}`,
              startTime,
              {
                error_code: "import_failed",
                filename,
                errors: result.errors
              }
            );
          }
        }
        else if (operation === "unimport") {
          if (!filename) {
            return createErrorResponse(
              "filename parameter required for 'unimport' operation",
              startTime,
              { error_code: "missing_filename" }
            );
          }

          const clausesRemoved = await prologInterface.unimportFile(filename);
          const processingTimeMs = Date.now() - startTime;

          return {
            content: [{
              type: "text",
              text: `Successfully unimported file: ${filename}\nClauses removed: ${clausesRemoved}\nProcessing time: ${processingTimeMs}ms`
            }],
            structuredContent: {
              operation: "unimport",
              file: filename,
              clauses_removed: clausesRemoved,
              processing_time_ms: processingTimeMs
            },
            isError: false
          };
        }
        else if (operation === "list") {
          const imports = await prologInterface.getImportedFiles();
          const processingTimeMs = Date.now() - startTime;

          const importsList = imports.length > 0
            ? imports.map(f => `${f.filename}: ${f.clauseCount} clause(s), imported at ${new Date(f.timestamp).toISOString()}`).join("\n")
            : "(no imported files)";

          return {
            content: [{
              type: "text",
              text: `Imported Files:\n${importsList}\n\nProcessing time: ${processingTimeMs}ms`
            }],
            structuredContent: {
              operation: "list",
              imports,
              processing_time_ms: processingTimeMs
            },
            isError: false
          };
        }
        else {
          return createErrorResponse(
            `Invalid operation: ${operation}`,
            startTime,
            { error_code: "invalid_operation" }
          );
        }
      } catch (error) {
        return createErrorResponse(
          error instanceof Error ? error.message : String(error),
          startTime
        );
      }
    },
  },

  workspace: {
    title: "Manage Workspace",
    description: "Workspace introspection and management. 'snapshot' gets original source text of all asserted clauses, 'reset' removes all facts/rules from workspace, 'list_symbols' lists all user-defined predicates.",
    inputSchema: workspaceSchema,
    handler: async (
      { operation }: {
        operation: "snapshot" | "reset" | "list_symbols";
      },
      _extra
    ): Promise<CallToolResult> => {
      const startTime = Date.now();

      try {
        await prologInterface.start();

        if (operation === "snapshot") {
          const snapshot = await prologInterface.getSnapshot();
          const processingTimeMs = Date.now() - startTime;

          return {
            content: [{
              type: "text",
              text: snapshot || "(empty workspace)"
            }],
            structuredContent: {
              operation: "snapshot",
              processing_time_ms: processingTimeMs,
              has_content: !!snapshot
            },
            isError: false
          };
        }
        else if (operation === "reset") {
          await prologInterface.clearWorkspaceWithSource();
          const processingTimeMs = Date.now() - startTime;

          return {
            content: [{
              type: "text",
              text: `Successfully reset workspace - all facts and rules removed\nProcessing time: ${processingTimeMs}ms`
            }],
            structuredContent: {
              operation: "reset",
              processing_time_ms: processingTimeMs
            },
            isError: false
          };
        }
        else if (operation === "list_symbols") {
          const result = await prologInterface.query("list_predicates");
          const processingTimeMs = Date.now() - startTime;

          const parsePreds = (s: string): string[] => {
            try {
              const trimmed = s.trim();
              if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
                const inner = trimmed.slice(1, -1).trim();
                if (!inner) return [];
                return inner.split(",").map((p) => p.trim());
              }
            } catch { }
            return [];
          };

          const predicates = parsePreds(result);

          return {
            content: [{
              type: "text",
              text: `Available predicates:\n${result}\nProcessing time: ${processingTimeMs}ms`
            }],
            structuredContent: {
              operation: "list_symbols",
              predicates,
              raw: result,
              processing_time_ms: processingTimeMs
            },
            isError: false
          };
        }
        else {
          return createErrorResponse(
            `Invalid operation: ${operation}`,
            startTime,
            { error_code: "invalid_operation" }
          );
        }
      } catch (error) {
        return createErrorResponse(
          error instanceof Error ? error.message : String(error),
          startTime
        );
      }
    },
  },

  explain_error: {
    title: "Explain Prolog Error",
    description: "Analyze and explain a Prolog error using domain expertise and MCP sampling. Provides detailed explanation, root cause analysis, and actionable suggestions. Returns structured guidance including tool usage patterns when applicable.",
    inputSchema: explainErrorSchema,
    outputSchema: explainErrorOutputSchema,
    handler: async ({ error, query, include_kb }: { error: { kind: string; message: string; details?: any }; query?: string; include_kb?: boolean }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      // Check if server reference is available for sampling
      if (!serverRef.current) {
        return createErrorResponse(
          "Server reference not available for sampling",
          startTime,
          { sampling_used: false }
        );
      }

      // Check if sampling is supported
      const samplingSupported = isSamplingSupported(serverRef.current);
      if (!samplingSupported) {
        return createErrorResponse(
          "MCP sampling not supported by client. This tool requires sampling capability.",
          startTime,
          { sampling_used: false }
        );
      }

      try {
        // Build context from KB if requested (default true)
        let kbContext = "";
        if (include_kb !== false) {
          try {
            await prologInterface.start();

            // Get predicates list
            const predicatesResult = await prologInterface.query("list_predicates");
            kbContext += `\nCurrent Predicates:\n${predicatesResult}\n`;

            // Get KB dump (limit to 2000 chars to avoid overwhelming the prompt)
            const dumpResult = await prologInterface.query("dump_knowledge_base");
            const truncatedDump = dumpResult.length > 2000
              ? dumpResult.substring(0, 2000) + "\n... (truncated)"
              : dumpResult;
            kbContext += `\nKnowledge Base:\n${truncatedDump}\n`;
          } catch (kbError) {
            // KB context is optional, continue without it
            kbContext = "\n(Knowledge base context unavailable)\n";
          }
        }

        // Build query context
        const queryContext = query
          ? `\nQuery that caused the error:\n${query}\n`
          : "";

        // Combine all context
        const fullContext = queryContext + kbContext;

        // Use the Prolog error explainer
        const explanation = await prologErrorExplainer.explainError(
          serverRef.current,
          {
            error,
            context: fullContext || undefined,
          }
        );

        const processingTimeMs = Date.now() - startTime;

        if (!explanation) {
          return createErrorResponse(
            "Failed to generate error explanation via sampling",
            startTime,
            { sampling_used: true }
          );
        }

        // Format response text
        let responseText = `# Error Explanation\n\n`;
        responseText += `## What Went Wrong\n${explanation.explanation}\n\n`;
        responseText += `## Root Cause\n${explanation.cause}\n\n`;
        responseText += `## Suggestions\n`;
        explanation.suggestions.forEach((suggestion: string, index: number) => {
          responseText += `${index + 1}. ${suggestion}\n`;
        });

        if (explanation.examples && explanation.examples.length > 0) {
          responseText += `\n## Code Examples\n`;
          explanation.examples.forEach((example: string, index: number) => {
            responseText += `\n### Example ${index + 1}\n\`\`\`prolog\n${example}\n\`\`\`\n`;
          });
        }

        if (explanation.toolGuidance) {
          responseText += `\n## Tool Usage Guidance\n${explanation.toolGuidance}\n`;
        }

        responseText += `\n---\nProcessing time: ${processingTimeMs}ms`;

        return {
          content: [{ type: "text", text: responseText }],
          structuredContent: {
            explanation: explanation.explanation,
            cause: explanation.cause,
            suggestions: explanation.suggestions,
            examples: explanation.examples,
            tool_guidance: explanation.toolGuidance,
            processing_time_ms: processingTimeMs,
            sampling_used: true,
          },
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        return createErrorResponse(
          error instanceof Error ? error.message : String(error),
          startTime,
          { sampling_used: true }
        );
      }
    },
  },
};

/**
 * Convert snake_case to camelCase
 */
function snakeToCamel(str: string): string {
  return str.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
}

/**
 * Extract handlers from tools for direct access (used by integration tests)
 * Converts snake_case tool names to camelCase for backward compatibility
 *
 * Type-safe export: No type assertions, enforces correct handler signatures
 */
type ToolHandler = ToolDefinition['handler'];

export const toolHandlers: Record<string, ToolHandler> = Object.fromEntries(
  Object.entries(tools).map(([key, def]) => [snakeToCamel(key), def.handler])
);

export { prologInterface };

// Re-export schemas for backward compatibility with tests
export { zodSchemas as inputSchemas, jsonSchemas } from "./schemas.js";
