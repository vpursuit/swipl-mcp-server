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
import { logger, serverRef } from "./logger.js";
import { samplingProviderRef } from "./samplingProvider.js";
import { getErrorKnowledge } from "./errorKnowledge.js";
import { PrologErrorKind } from "./PrologInterface.js";
import type { ErrorExplanation } from "@vpursuit/mcp-server-sampling";

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
        "puzzle"
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
 * Fast-path explanation for common, well-known errors
 * Returns explanation without LLM call for instant response
 *
 * @param error - The error object
 * @returns ErrorExplanation if this is a common error, null otherwise
 */
function tryFastPathExplanation(error: { kind: string; message: string; details?: any }): ErrorExplanation | null {
  // 1. Undefined predicate - PRIORITY: suggest clauses assert first
  if (error.kind === 'existence_error' || error.message.includes('Unknown procedure') ||
      error.message.includes('undefined') || error.message.includes('does not exist')) {
    const predicateMatch = error.message.match(/(\w+)\/(\d+)/);
    const predicateName = predicateMatch ? `${predicateMatch[1]}/${predicateMatch[2]}` : 'the predicate';

    return {
      explanation: `The predicate ${predicateName} is not defined in the knowledge base.`,
      cause: "You're trying to call a predicate that hasn't been defined yet with facts or rules.",
      suggestions: [
        `Define ${predicateName} using clauses tool: { operation: 'assert', clauses: 'your_fact_or_rule.' }`,
        "Check if you need to load a library (use clauses to assert use_module first)",
        "Verify the predicate name and arity are correct",
      ],
      examples: [
        `% Define with facts:\nclauses tool: { operation: 'assert', clauses: 'parent(tom, bob).' }\n\n% Or with rules:\nclauses tool: { operation: 'assert', clauses: 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z).' }`
      ],
      toolGuidance: "Use clauses tool (operation='assert') to define predicates. No file access needed.",
    };
  }

  // 2. Unbound variable in arithmetic
  if (error.kind === 'instantiation_error' ||
      (error.message.includes('instantiation') && (error.message.includes('arithmetic') || error.message.includes('is') || error.message.includes('#=')))) {
    return {
      explanation: "A variable was used in arithmetic before it was bound to a value.",
      cause: "Prolog requires variables to be instantiated (have a value) before use in arithmetic expressions or constraints.",
      suggestions: [
        "Bind the variable before using it (e.g., X = 5 before X > 3)",
        "Reorder your goals: put binding goals before arithmetic tests",
        "For CLP(FD), use #= for both assignment and constraints",
      ],
      examples: [
        "% BAD:\nsolve(X) :- X > 5, X = 10.\n\n% GOOD:\nsolve(X) :- X = 10, X > 5.\n\n% CLP(FD) style:\nsolve(X) :- X #= 10, X #> 5."
      ],
      toolGuidance: "Use clauses tool to define correct goal ordering.",
    };
  }

  // 3. CLP(FD) operator errors (should be rare - clpfd is pre-loaded)
  if (error.message.includes('clpfd') || error.message.includes('#=') || error.message.includes('#>') || error.message.includes('ins')) {
    return {
      explanation: "Error using CLP(FD) constraint operators. Note: library(clpfd) is pre-loaded by default.",
      cause: "Syntax error or incorrect usage of constraint operators (#=, #>, ins, etc.). The clpfd library is already available.",
      suggestions: [
        "Check operator syntax: Use #= not =, #> not >, etc.",
        "Verify variable names are capitalized (X, Y, Vars)",
        "Ensure constraints are within a rule body (after :-)",
        "If error persists, check for typos in operator names",
      ],
      examples: [
        "% CORRECT syntax (clpfd pre-loaded):\nsolve(X) :- X ins 1..9, X #> 5.\n\n% COMMON ERRORS:\n% Wrong: X ins 1..9.  (constraint at top level)\n% Wrong: X = 5.  (use #= for constraints)\n% Wrong: x ins 1..9.  (lowercase variable)"
      ],
      toolGuidance: "Use clauses tool with proper constraint syntax. CLP(FD) is pre-loaded - no need to load library(clpfd).",
    };
  }

  // 4. Timeout (infinite loop)
  if (error.kind === 'timeout' || error.message.includes('timeout') || error.message.includes('time limit')) {
    return {
      explanation: "Query exceeded time limit. Likely an infinite loop or non-terminating recursion.",
      cause: "The query didn't complete within the timeout period (default 10s).",
      suggestions: [
        "Check for infinite recursion (missing base case or wrong order)",
        "Add base case before recursive case in your rules",
        "Verify termination conditions are reachable",
        "Use trace/debugging to identify the loop",
      ],
      examples: [
        "% BAD (infinite loop):\ncount(N) :- count(N), N > 0.\n\n% GOOD:\ncount(0).\ncount(N) :- N > 0, N1 is N-1, count(N1)."
      ],
      toolGuidance: "Review rules defined with clauses tool. Ensure base cases come first.",
    };
  }

  // 5. Permission error (modifying built-ins)
  if (error.kind === 'permission_error' || error.message.includes('permission')) {
    return {
      explanation: "Attempted to modify a built-in or protected predicate.",
      cause: "You cannot assert/retract built-in predicates like member/2, append/3, etc.",
      suggestions: [
        "Don't try to modify built-in predicates",
        "Define your own predicate with a different name",
        "Use built-ins as-is or load them from libraries",
      ],
      examples: [
        "% BAD:\nclauses tool: { operation: 'assert', clauses: 'member(X, [X|_]).' }\n\n% GOOD:\nclauses tool: { operation: 'assert', clauses: 'my_member(X, [X|_]).' }"
      ],
      toolGuidance: "Choose different predicate names to avoid conflicts with built-ins.",
    };
  }

  // 6. File not found errors (library loading) - SECONDARY option after clauses
  if (error.message.includes('File') && error.message.includes('does not exist')) {
    const fileMatch = error.message.match(/File '([^']+)' does not exist/);
    const filename = fileMatch ? fileMatch[1] : 'library';

    // Special case for clpfd library (should never happen - it's pre-loaded)
    if (filename.includes('clpfd') || filename === 'library(clpfd)') {
      return {
        explanation: "File not found error mentioning clpfd. Note: library(clpfd) is pre-loaded by default.",
        cause: "This error should not occur for clpfd. Possible incorrect file path or syntax error.",
        suggestions: [
          "CLP(FD) is pre-loaded - you don't need to load it",
          "If you're trying to use constraints, just use them directly: X ins 1..9, X #> 5",
          "If you got this from a file operation, check your file paths",
          "If error persists, check for typos in your constraint code",
        ],
        examples: [
          "% CLP(FD) is pre-loaded - use directly:\nsolve(X) :- X ins 1..9, X #> 5, labeling([ff], [X])."
        ],
        toolGuidance: "CLP(FD) is pre-loaded. Use constraints directly in your clauses.",
      };
    }

    return {
      explanation: `The file or library '${filename}' could not be found.`,
      cause: "The specified file does not exist or the library is not installed.",
      suggestions: [
        "For libraries: use clauses tool with ':- use_module(library(name)).'",
        "For files: use files tool with operation='import', filename='libraryname' (no library() wrapper)",
        "Check the filename/path for typos",
      ],
      examples: [
        "% Load library (PREFERRED):\nclauses tool: { operation: 'assert', clauses: ':- use_module(library(lists)).' }\n\n% Load file:\nfiles tool: { operation: 'import', filename: 'lists' }"
      ],
      toolGuidance: "Use clauses tool for libraries, files tool for custom .pl files.",
    };
  }

  // 7. Syntax error: operator expected
  if (error.kind === 'syntax_error' && (
    error.message.includes('operator_expected') ||
    error.details?.raw?.includes('operator_expected')
  )) {
    return {
      explanation: "Prolog parser expected an operator but found something else.",
      cause: "Incorrect clause syntax. Often caused by wrapping clauses in extra brackets or missing operators.",
      suggestions: [
        "Use plain string: clauses tool with clauses='solve(X) :- length(X, 11).'",
        "For multiple clauses: clauses=['clause1.', 'clause2.'] (array of strings, not nested)",
        "Check for missing or extra parentheses",
        "Ensure operators like :-, #=, ins are used correctly",
      ],
      examples: [
        "% CORRECT:\nclauses tool: { operation: 'assert', clauses: 'solve(X) :- length(X, 11).' }\n\n% WRONG:\nclauses tool: { operation: 'assert', clauses: '[solve(X) :- length(X, 11).]' }"
      ],
      toolGuidance: "The clauses tool expects strings, not nested arrays.",
    };
  }

  // 8. Missing operator definition
  if (error.message.includes('operator') && error.message.includes('undefined')) {
    return {
      explanation: "Using an operator that isn't defined or loaded.",
      cause: "The operator is from a library that needs to be loaded first. Note: CLP(FD) operators are pre-loaded.",
      suggestions: [
        "For CLP(FD) operators (#=, ins, etc.): These are pre-loaded - check spelling/syntax",
        "For other library operators: Load using clauses: ':- use_module(library(name)).'",
        "Verify operator spelling and correct usage context",
      ],
      examples: [
        "% CLP(FD) operators (pre-loaded - check syntax):\nX ins 1..9, X #> 5\n\n% Other library operators:\nclauses({ operation: 'assert', clauses: ':- use_module(library(name)).' })"
      ],
      toolGuidance: "CLP(FD) is pre-loaded. For other libraries, use clauses tool to load them.",
    };
  }

  // 9. Type error in built-ins
  if (error.kind === 'type_error' || error.message.includes('type error')) {
    return {
      explanation: "A predicate received an argument of the wrong type.",
      cause: "Built-in predicates expect specific types (e.g., atom, integer, list).",
      suggestions: [
        "Check argument types match predicate expectations",
        "Use is/2 for arithmetic: X is Y + 1",
        "Ensure lists are proper lists [H|T] ending in []",
      ],
      examples: [
        "% BAD:\nX = 5 + 3  % X bound to structure +(5,3)\n\n% GOOD:\nX is 5 + 3  % X bound to 8"
      ],
      toolGuidance: "Review predicate documentation for expected types.",
    };
  }

  // 10. Session errors (query lifecycle)
  if (error.kind === 'session_conflict' || error.message.includes('session')) {
    return {
      explanation: "Query session management error.",
      cause: "You need to close the current session before starting a new one, or tried to use next/close without start.",
      suggestions: [
        "Follow lifecycle: query start → query next (repeat) → query close",
        "Close current session before starting a new query",
        "Don't call query next/close without an active session",
      ],
      examples: [
        "% Correct flow:\n1. query tool: { operation: 'start', query: 'parent(X,Y)' }\n2. query tool: { operation: 'next' }  (repeat for more solutions)\n3. query tool: { operation: 'close' }"
      ],
      toolGuidance: "Manage query lifecycle: start → next → close.",
    };
  }

  // 11. Missing closing period
  if (error.message.includes('end_of_file') || error.message.includes('unexpected end')) {
    return {
      explanation: "Clause or query is incomplete.",
      cause: "Missing closing period (.) at end of clause or incomplete syntax.",
      suggestions: [
        "Ensure all clauses end with a period (.)",
        "Check for unclosed parentheses or brackets",
        "Verify string quotes are properly closed",
      ],
      examples: [
        "% BAD:\nclauses: 'parent(tom, bob)'\n\n% GOOD:\nclauses: 'parent(tom, bob).'"
      ],
      toolGuidance: "All clauses must end with a period (.)",
    };
  }

  // 12. List syntax error
  if (error.message.includes('list') || (error.kind === 'syntax_error' && error.message.includes('|'))) {
    return {
      explanation: "Invalid list syntax.",
      cause: "Lists must use proper head|tail syntax or comma-separated elements.",
      suggestions: [
        "Use [H|T] for head/tail decomposition",
        "Use [a,b,c] for explicit lists",
        "Empty list is []",
      ],
      examples: [
        "% Correct list patterns:\n[H|T]  % head and tail\n[X,Y,Z]  % explicit elements\n[]  % empty list\n[H|[]]  % single element list"
      ],
      toolGuidance: "Review Prolog list syntax.",
    };
  }

  // 13. Arity mismatch
  if (error.message.includes('arity') || (error.message.includes('arguments') && error.message.includes('expected'))) {
    return {
      explanation: "Predicate called with wrong number of arguments.",
      cause: "The predicate was defined with a different arity than how it's being called.",
      suggestions: [
        "Check the predicate definition for number of arguments",
        "Verify you're calling the right predicate variant",
        "Predicates with different arities are different predicates",
      ],
      examples: [
        "% parent/2 and parent/3 are DIFFERENT:\nparent(X,Y).  % parent/2\nparent(X,Y,Z).  % parent/3"
      ],
      toolGuidance: "Match argument count to predicate definition.",
    };
  }

  // 14. Circular rule (left recursion)
  if (error.message.includes('circular') || error.message.includes('left recursion')) {
    return {
      explanation: "Rule has problematic left recursion or circular dependency.",
      cause: "The recursive call is the first goal, leading to infinite recursion.",
      suggestions: [
        "Put base case first in rule definitions",
        "Move recursive goal after other goals",
        "Add termination condition",
      ],
      examples: [
        "% BAD (left recursion):\npath(X,Y) :- path(X,Z), edge(Z,Y).\n\n% GOOD:\npath(X,Y) :- edge(X,Y).\npath(X,Y) :- edge(X,Z), path(Z,Y)."
      ],
      toolGuidance: "Structure rules to avoid left recursion.",
    };
  }

  // 15. Other syntax errors
  if (error.kind === 'syntax_error' || error.message.toLowerCase().includes('syntax')) {
    return {
      explanation: "Invalid Prolog syntax detected.",
      cause: "The clause or query contains syntax that doesn't conform to Prolog's grammar.",
      suggestions: [
        "Check for matching parentheses and brackets",
        "Ensure atoms are properly quoted if they contain spaces",
        "Verify operator usage (:-,  =, #=, etc.)",
        "Make sure clauses end with a period (.)",
      ],
      examples: [
        "% Valid clause syntax:\nfact(value).\nrule(X) :- condition(X), another(X).\nquery :- goal1, goal2."
      ],
      toolGuidance: "Use clauses tool (operation='assert') to add facts/rules.",
    };
  }

  // No fast-path match
  return null;
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
                solution: null,
                status: "success" as const,
                processing_time_ms: processingTimeMs
              },
            };
          } else {
            // Standard mode
            const result = await prologInterface.startQuery(query);
            const processingTimeMs = Date.now() - startTime;
            return {
              content: [
                {
                  type: "text",
                  text: `Query started: ${query}\nStatus: ${result.status}\nProcessing time: ${processingTimeMs}ms`,
                },
              ],
              structuredContent: {
                solution: null,
                status: "success" as const,
                processing_time_ms: processingTimeMs
              },
            };
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
              solution: null,
              status: "done" as const,
              processing_time_ms: processingTimeMs,
              error: errMsg
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
              { solution: null, status: "done" }
            );
          }

          const processingTimeMs = Date.now() - startTime;

          if (result.error) {
            return createErrorResponse(
              result.error,
              startTime,
              { solution: null, status: "done" }
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
    description: `Assert or retract facts/rules in knowledge base. Preserves source formatting and variable names. Use 'assert' to add facts/rules (single or batch), 'retract' to remove matching facts/rules. To reset entire workspace, use workspace tool's 'reset' operation.

INPUT FORMATS:
- Single clause: clauses: "parent(john, mary)."
- Multiple independent clauses: clauses: ["fact1.", "fact2.", "rule(X) :- body(X)."]
- Multi-line rule (single clause): clauses: "rule(X) :- goal1(X), goal2(X), goal3(X)."

CRITICAL: Array Elements are Complete Clauses, NOT Lines of Code
- Each array element must be a COMPLETE, self-contained clause
- Multi-line rules should be ONE string with comma-separated goals
- Do NOT split a single rule across multiple array elements

CORRECT USAGE:
✓ clauses: "ancestor(X,Y) :- parent(X,Y)."
✓ clauses: "ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z)."
✓ clauses: ["fact1.", "fact2.", "fact3."]
✓ clauses: ["rule1(X) :- body1(X).", "rule2(Y) :- body2(Y)."]

INCORRECT USAGE (Common Mistakes):
✗ clauses: ["ancestor(X,Y) :-", "  parent(X,Y)."]  // Wrong: incomplete clauses
✗ clauses: ["solve(X) :-", "  length(X, 10),", "  X ins 1..10."]  // Wrong: rule split across elements
✗ clauses: "% This is a comment\\nfact(x)."  // Wrong: comments not supported

COMMENTS NOT SUPPORTED:
- Prolog comments (%) cause syntax errors in direct assertions
- For code with comments, use files tool instead:
  1. Create a .pl file with your commented code
  2. Use files tool: { operation: 'import', filename: 'yourfile.pl' }

TOOL SELECTION GUIDE:
- Simple facts/rules without comments → use clauses tool
- Complex multi-predicate code → use clauses tool with multiple complete clauses
- Code with comments or extensive documentation → use files tool
- Library loading → use clauses: ':- use_module(library(name)).' OR files: 'library_name'`,
    inputSchema: clausesSchema,
    handler: async (
      { operation, clauses }: { operation: "assert" | "retract"; clauses?: string | string[] },
      _extra
    ): Promise<CallToolResult> => {
      const startTime = Date.now();

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

          // Detect if this is a library name (no path separators, no .pl extension)
          const isLibraryName = !/[\/\\]/.test(filename) && !filename.endsWith('.pl');

          if (isLibraryName) {
            // Load as library using loadLibrary()
            try {
              await prologInterface.loadLibrary(filename);
              const processingTimeMs = Date.now() - startTime;

              return {
                content: [{
                  type: "text",
                  text: `Successfully loaded library: ${filename}\nProcessing time: ${processingTimeMs}ms`
                }],
                structuredContent: {
                  operation: "import",
                  library: filename,
                  processing_time_ms: processingTimeMs
                },
                isError: false
              };
            } catch (error) {
              return createErrorResponse(
                `Error loading library '${filename}': ${error instanceof Error ? error.message : String(error)}`,
                startTime,
                { error_code: "library_load_failed", library: filename }
              );
            }
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

          const result = await prologInterface.unimportFile(filename);
          const processingTimeMs = Date.now() - startTime;

          return {
            content: [{
              type: "text",
              text: `Successfully unimported file: ${filename}\nClauses removed: ${result.clausesRemoved}\nProcessing time: ${processingTimeMs}ms`
            }],
            structuredContent: {
              operation: "unimport",
              file: filename,
              success: result.success,
              clauses_removed: result.clausesRemoved,
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
    description: "Workspace introspection and management. 'snapshot' gets original source text with preserved formatting and variable names, 'reset' performs FULL workspace reset (removes all Prolog facts/rules, clears all source storage, clears file import history and provenance tracking), 'list_symbols' lists all user-defined predicates.",
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
    description: "Analyze and explain a Prolog error using domain expertise and MCP sampling. Provides detailed explanation, root cause analysis, and actionable suggestions. Falls back to rule-based explanations when sampling is unavailable.",
    inputSchema: explainErrorSchema,
    outputSchema: explainErrorOutputSchema,
    handler: async ({ error, query, include_kb }: { error: { kind: string; message: string; details?: any }; query?: string; include_kb?: boolean }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      try {
        // Smart KB inclusion: skip KB context for errors that don't need it
        // This significantly reduces prompt size and LLM response time
        let shouldIncludeKb = include_kb !== false; // Default from schema or explicit arg

        if (shouldIncludeKb) {
          // Syntax errors don't need KB context - the error is in the syntax itself
          if (error.kind === 'syntax_error' ||
            error.message.includes('syntax') ||
            error.message.includes('operator_expected')) {
            shouldIncludeKb = false;
            logger.debug("Skipping KB context for syntax error");
          }

          // File/library errors don't need KB context - the problem is with file loading
          if (error.kind === 'file_error' ||
            error.kind === 'generic' && (
              error.message.includes('File') && error.message.includes('does not exist') ||
              error.message.includes('library')
            )) {
            shouldIncludeKb = false;
            logger.debug("Skipping KB context for file/library error");
          }
        }

        // Build context from KB if needed
        let kbContext = "";
        if (shouldIncludeKb) {
          try {
            await prologInterface.start();

            // Get predicates list
            const predicatesResult = await prologInterface.query("list_predicates");
            kbContext += `\nCurrent Predicates:\n${predicatesResult}\n`;

            // Get KB dump (limit to 2000 chars to avoid overwhelming the prompt)
            const dumpResult = await prologInterface.query("dump_knowledge_base");
            // Truncate at clause boundaries for valid Prolog
            const truncatedDump = dumpResult.length > 2000
              ? dumpResult.substring(0, 2000).replace(/\.\s*[^\n]*$/, ".") + "\n... (truncated)"
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

        // Try fast-path for instant response on common errors
        const fastPathExplanation = tryFastPathExplanation(error);

        let explanationResult;
        if (fastPathExplanation) {
          // Use fast-path result (no LLM call needed)
          logger.debug("Using fast-path explanation for common error");
          explanationResult = {
            success: true,
            explanation: fastPathExplanation,
            samplingUsed: false,
          };
        } else {
          // Try AI-powered explanation using sampling provider
          explanationResult = await prologErrorExplainer.explainError(
            samplingProviderRef.current,
            {
              error,
              context: fullContext || undefined,
            }
          );
        }

        const processingTimeMs = Date.now() - startTime;

        // If sampling succeeded, use the AI explanation
        if (explanationResult.success) {
          const explanation = explanationResult.explanation;

          // Format response text
          let responseText = `# Error Explanation (AI-Powered)\n\n`;
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
        }

        // Sampling failed or unavailable - fall back to rule-based explanation
        logger.debug("Sampling failed for error explanation, using rule-based fallback", { error: explanationResult.error });

        // Get domain knowledge for this error type
        const errorKind = error.kind as PrologErrorKind || PrologErrorKind.UNKNOWN;
        const knowledge = getErrorKnowledge(errorKind);

        // Parse examples into suggestions
        const exampleLines = knowledge.examples.split("\n").filter(line => line.trim());
        const suggestions: string[] = [];
        const examples: string[] = [];

        for (const line of exampleLines) {
          const trimmed = line.trim();
          if (trimmed.startsWith("BAD:") || trimmed.startsWith("GOOD:")) {
            examples.push(trimmed);
          } else if (trimmed && !trimmed.startsWith("Use ") && !trimmed.startsWith("PATTERN:")) {
            suggestions.push(trimmed);
          }
        }

        // Add general suggestions based on error type
        if (suggestions.length === 0) {
          suggestions.push("Review the error message and ensure proper Prolog syntax");
          suggestions.push("Check the knowledge base state with the workspace tool");
          suggestions.push("Consult SWI-Prolog documentation for this error type");
        }

        // Format response text
        let responseText = `# Error Explanation (Rule-Based)\n\n`;
        responseText += `## What Went Wrong\n${knowledge.description.trim()}\n\n`;
        responseText += `## Root Cause\nError type: ${errorKind}\n${error.message}\n\n`;
        responseText += `## Suggestions\n`;
        suggestions.slice(0, 5).forEach((suggestion: string, index: number) => {
          responseText += `${index + 1}. ${suggestion}\n`;
        });

        if (examples.length > 0) {
          responseText += `\n## Examples\n`;
          examples.forEach((example: string) => {
            responseText += `${example}\n`;
          });
        }

        responseText += `\n---\nProcessing time: ${processingTimeMs}ms\n`;
        responseText += `Note: AI-powered explanation unavailable${explanationResult.error ? ` (${explanationResult.error.code})` : ''}. Using rule-based fallback.`;

        return {
          content: [{ type: "text", text: responseText }],
          structuredContent: {
            explanation: knowledge.description.trim(),
            cause: `Error type: ${errorKind}. ${error.message}`,
            suggestions: suggestions.slice(0, 5),
            examples: examples.length > 0 ? examples : undefined,
            tool_guidance: errorKind === PrologErrorKind.NO_ACTIVE_SESSION || errorKind === PrologErrorKind.SESSION_CONFLICT
              ? "Follow the query lifecycle: query_start → query_next → query_close"
              : undefined,
            processing_time_ms: processingTimeMs,
            sampling_used: false,
          },
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        const errorMessage = error instanceof Error ? error.message : String(error);

        return {
          content: [{
            type: "text",
            text: `# Error Explanation\n\n## What Went Wrong\nAn unexpected error occurred while trying to explain the Prolog error.\n\n## Root Cause\n${errorMessage}\n\n## Suggestions\n1. Check the original error message for debugging information\n2. Review server logs for additional context\n3. Verify that all required dependencies are available\n4. Report this issue if it persists\n\n---\nProcessing time: ${processingTimeMs}ms`
          }],
          structuredContent: {
            explanation: "An unexpected error occurred in the explain_error tool while attempting to analyze the Prolog error.",
            cause: errorMessage,
            suggestions: [
              "Review the original error message and details provided",
              "Check the server logs for additional error context",
              "Verify that the Prolog interface is properly initialized",
              "Try the operation again in case this was a transient failure",
              "Report this issue if it persists with steps to reproduce"
            ],
            processing_time_ms: processingTimeMs,
            sampling_used: false
          },
          isError: true
        };
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

/**
 * Array of available tool names for programmatic access
 */
export const toolNames = Object.keys(tools) as Array<keyof typeof tools>;
