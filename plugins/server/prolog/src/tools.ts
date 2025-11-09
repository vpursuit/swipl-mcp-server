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
  helpSchema,
  licenseSchema,
  capabilitiesSchema,
  capabilitiesOutputSchema,
  symbolsListOutputSchema,
  queryNextOutputSchema,
  knowledgeBaseLoadSchema,
  queryStartSchema,
  queryNextSchema,
  queryCloseSchema,
  symbolsListSchema,
  knowledgeBaseAssertSchema,
  knowledgeBaseAssertManySchema,
  knowledgeBaseRetractSchema,
  knowledgeBaseRetractManySchema,
  knowledgeBaseClearSchema,
  knowledgeBaseLoadLibrarySchema,
  queryStartEngineSchema,
  knowledgeBaseDumpSchema,
} from "./schemas.js";
import { resolvePackageVersion, findNearestFile } from "@vpursuit/mcp-server-core";
import { validateStringInput } from "./utils/validation.js";
import { createErrorResponse, createSuccessResponse, formatClauseResults, type ClauseOperationResult } from "./utils/response.js";

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

type KnowledgeBaseClauseOutcome = {
  clause: string;
  result: string;
  ok: boolean;
  threw: boolean;
};

async function knowledgeBaseAssertClauses(
  facts: string[],
  { ignoreStartErrors = false }: { ignoreStartErrors?: boolean } = {},
): Promise<{ outcomes: KnowledgeBaseClauseOutcome[]; successCount: number; errorCount: number }> {
  const normalize = (clause: string) => {
    const trimmed = clause.trim().replace(/\.$/, "");
    // For rules (containing :-), wrap in parentheses to prevent comma operator confusion
    if (/:-/.test(trimmed) && !/^\(.*\)$/.test(trimmed)) {
      return `(${trimmed})`;
    }
    return trimmed;
  };

  if (ignoreStartErrors) {
    try {
      await prologInterface.start();
    } catch {
      // Ignore start errors so we can still collect per-clause failures
    }
  } else {
    await prologInterface.start();
  }

  const outcomes: KnowledgeBaseClauseOutcome[] = [];
  let successCount = 0;
  let errorCount = 0;

  for (const fact of facts) {
    const clause = normalize(fact);
    try {
      const result = await prologInterface.query(`assert(${clause})`);
      const ok = result === "ok";
      outcomes.push({ clause, result, ok, threw: false });
      if (ok) {
        successCount++;
      } else {
        errorCount++;
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      outcomes.push({ clause, result: message, ok: false, threw: true });
      errorCount++;
    }
  }

  return { outcomes, successCount, errorCount };
}

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
      core: ["help", "license", "capabilities"],
      knowledge_base: [
        "knowledge_base_load",
        "knowledge_base_load_library",
        "knowledge_base_assert",
        "knowledge_base_assert_many",
        "knowledge_base_retract",
        "knowledge_base_retract_many",
        "knowledge_base_clear",
        "knowledge_base_dump",
      ],
      query: ["query_start", "query_startEngine", "query_next", "query_close"],
      symbols: ["symbols_list"],
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
  };
  return caps;
}

/**
 * Tool definitions for Prolog plugin
 */
export const tools: ToolDefinitions = {
  help: {
    title: "Help",
    description: "Get usage guidelines. Optional topic: overview, standard_mode, engine_mode, safety, security, examples, prompts, troubleshooting, roots.",
    inputSchema: helpSchema,
    handler: async ({ topic }: { topic?: string } = {}, _extra): Promise<CallToolResult> => {
      const sectionsData: Record<string, string[]> = {
        overview: [
          "SWI‑Prolog MCP Server: tools for loading files, querying, managing the knowledge base, and exploring symbols.",
          "Two modes: standard (call_nth/2) and engine (SWI engines). Unified query interface.",
        ],
        standard_mode: [
          "Standard mode (call_nth/2):",
          "- Tools: query_start → query_next → query_close",
          "- Deterministic pagination of solutions, memory‑efficient",
          "- Example: query_start {query: 'member(X, [1,2,3])'} then call query_next until status='done'",
          "- Iterator pattern: query_next returns status='success' with solution, or status='done' when exhausted",
          "- Auto-close: Starting a new query automatically closes any active query or engine",
          "- Best practice: Explicitly close queries when done (though not strictly required)",
        ],
        engine_mode: [
          "Engine mode (SWI engines):",
          "- Tools: query_startEngine → query_next → query_close",
          "- True backtracking iterator with no recomputation",
          "- Unified query_next and query_close work for both modes",
          "- Iterator pattern: Same as standard mode, check status field to terminate iteration",
          "- Auto-close: Starting a new engine automatically closes any active query or engine",
          "- Best practice: Explicitly close engines when done (though not strictly required)",
        ],
        safety: [
          "Enhanced Security Model:",
          "- File Path Restrictions: Explicit root configuration required (MCP client or SWI_MCP_ALLOWED_ROOTS)",
          "- Secure by default: No file access without explicit configuration",
          "- Dangerous Predicate Blocking: Pre-execution detection of shell(), system(), call(), etc.",
          "- User code asserted into module 'knowledge_base' with unknown=fail",
          "- Files loaded via guarded consult (facts/rules only; directives rejected)",
          "- library(sandbox) validates most built-in predicates as safe/unsafe",
          "- Explicit blacklist blocks dangerous operations even if sandbox allows them",
          "- User-defined predicates in 'knowledge_base' module allowed for recursive definitions",
          "- Security is always enabled and cannot be disabled",
        ],
        security: [
          "Security Architecture:",
          "- Secure by Default: No file access without explicit root configuration",
          "- Root Configuration: MCP client roots or SWI_MCP_ALLOWED_ROOTS environment variable",
          "- File Path Restrictions: System directories (/etc, /usr, /bin, /var, etc.) always blocked",
          "- Dynamic root discovery from MCP client with strict path validation",
          "- Environment: SWI_MCP_ALLOWED_ROOTS=/path/one,/path/two (comma-separated absolute paths)",
          "- Pre-execution validation catches dangerous predicates before execution",
          "- Most built-ins validated by library(sandbox)",
          "- Safe built-ins: arithmetic, lists, term operations, logic, string/atom helpers",
          "- Dangerous operations explicitly blocked: shell(), system(), call(), assert(), halt()",
          "- User predicates in knowledge_base/user modules permitted for recursion",
          "- All queries executed in sandboxed environment",
        ],
        examples: [
          "Examples:",
          "- Even filter: findall(X, (between(1,10,X), 0 is X mod 2), L) → L=[2,4,6,8,10]",
          "- Collections: findall(X, member(X,[a,b,c]), L) → L=[a,b,c]",
          "- Engine: query_startEngine {query: '(between(1,6,X), 0 is X mod 2)'} and call query_next until status='done'",
          "- String helpers: sub_atom('hello_world',6,5,0,S) → S=world",
          "- Recursive rules: ancestor(X,Y) :- parent(X,Y). ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).",
          "See docs/examples.md for many more.",
        ],
        prompts: [
          "Domain-Specific Prompts:",
          "- genealogy: Build and query family trees using relational logic",
          "  Shows: assert_many, recursive rules, query modes, relationship inference",
          "- scheduling: Schedule tasks with dependencies using CLP(FD) constraints",
          "  Shows: load_library, constraint solving, optimization with labeling",
          "- puzzle: Solve logic puzzles using constraint programming",
          "  Shows: CLP(FD) workflow, constraint encoding, all_different, label/1",
          "- grammar: Parse natural language using Definite Clause Grammars (DCGs)",
          "  Shows: DCG syntax, phrase/2, parse tree generation",
          "",
          "Prompt Usage Pattern:",
          "Each prompt demonstrates MCP tool usage through solving domain-specific problems.",
          "All prompts include:",
          "- Complete workflow with structured steps",
          "- Concrete examples showing tool patterns",
          "- Key learning points for each pattern",
        ],
        available_predicates: [
          "Available Predicates:",
          "- All standard SWI-Prolog built-in predicates are available",
          "- Safe libraries can be loaded via :- use_module(library(...)) directives",
          "- library(clpfd) is available",
          "- Other safe libraries: lists, apply, aggregate, assoc, pairs, ordsets, clpb, and more",
          "- See SECURITY.md for complete list of allowed libraries",
          "- Load libraries in Prolog files before loading into knowledge base",
        ],
        troubleshooting: [
          "Troubleshooting:",
          "- error(unsafe_goal(...)): goal rejected by hybrid security (uses dangerous built-ins)",
          "- Session conflicts: now auto-resolved (previous sessions automatically closed)",
          "- Timeouts: configure via environment variables in Claude Desktop config:",
          "  • SWI_MCP_READY_TIMEOUT_MS: server startup (default 5000ms, try 10000ms if slow)",
          "  • SWI_MCP_QUERY_TIMEOUT_MS: query execution (default 30000ms, increase for complex queries)",
          "  • Set in claude_desktop_config.json using 'env' property",
          "- Recursive clauses: now work properly with hybrid security model",
          "- Query hangs: increase SWI_MCP_QUERY_TIMEOUT_MS or check for infinite loops",
          "- Startup failures: increase SWI_MCP_READY_TIMEOUT_MS or check SWI-Prolog installation",
        ],
        roots: [
          "Filesystem Roots:",
          "- File access requires explicit root configuration",
          "- Configure via MCP client roots or environment variable",
          "- Environment variable: SWI_MCP_ALLOWED_ROOTS (comma-separated absolute paths)",
          "- Example: SWI_MCP_ALLOWED_ROOTS=/Users/you/prolog,/Users/you/knowledge",
          "- Without configuration: file operations disabled (secure by default)",
          "- System directories always blocked for security",
          "- Use roots_list tool to see configured roots",
        ],
      };

      const includeOrder = [
        "overview",
        "standard_mode",
        "engine_mode",
        "safety",
        "security",
        "examples",
        "prompts",
        "available_predicates",
        "troubleshooting",
        "roots",
      ] as const;

      type HelpTopic = typeof includeOrder[number];
      const isValidTopic = (t: string | undefined): t is HelpTopic => {
        return (includeOrder as readonly string[]).includes(t ?? '');
      };

      const selected = topic && isValidTopic(topic) ? [topic] : includeOrder;

      const parts: string[] = [];
      for (const key of selected) {
        const lines = sectionsData[key];
        parts.push(...lines);
        if (key !== selected[selected.length - 1]) parts.push("");
      }

      const structured: Record<string, unknown> = { type: "help", sections: selected };
      const contentBySection: Record<string, string[]> = {};
      for (const key of selected) contentBySection[key] = sectionsData[key];
      structured["contentBySection"] = contentBySection;

      return { content: [{ type: "text", text: parts.join("\n") }], structuredContent: structured };
    },
  },

  license: {
    title: "License",
    description: "Get software license text",
    inputSchema: licenseSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      try {
        const licensePath = findNearestFile("LICENSE");
        if (!licensePath) {
          return {
            content: [{ type: "text", text: "License file not found." }],
            structuredContent: { error: "license_file_not_found" },
            isError: true,
          };
        }
        const licenseText = await readFile(licensePath, "utf8");
        return {
          content: [{ type: "text", text: licenseText }],
          structuredContent: { type: "license", text: licenseText, filename: "LICENSE" },
        };
      } catch (error) {
        return {
          content: [{ type: "text", text: `Error reading license file: ${error instanceof Error ? error.message : String(error)}` }],
          structuredContent: { error: error instanceof Error ? error.message : String(error) },
          isError: true,
        };
      }
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

  knowledge_base_load: {
    title: "Load Knowledge Base",
    description: "Load Prolog file (.pl) containing facts and rules. File path must be within configured roots.",
    inputSchema: knowledgeBaseLoadSchema,
    handler: async ({ filename }: { filename: string }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      const v = validateStringInput("filename", filename, MAX_FILENAME_LENGTH);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }

      const pathValidation = await validateFilePath(filename);
      if (!pathValidation.allowed) {
        return createErrorResponse(
          pathValidation.error || "File access denied",
          startTime,
          { error_code: "file_path_violation", blocked_path: filename }
        );
      }

      try {
        await prologInterface.start();

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

        const result = await prologInterface.consultFile(filename);
        return createSuccessResponse(
          `Successfully consulted file: ${filename}\nResult: ${result}`,
          startTime,
          { file: filename, result }
        );
      } catch (error) {
        return createErrorResponse(
          error instanceof Error ? error.message : String(error),
          startTime
        );
      }
    },
  },

  query_start: {
    title: "Start Query (Standard Mode)",
    description: "Start Prolog query using call_nth/2 pagination. Follow with query_next to iterate solutions, query_close when done.",
    inputSchema: queryStartSchema,
    handler: async ({ query }: { query: string }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      const v = validateStringInput("query", query, MAX_QUERY_LENGTH);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }

      try {
        await prologInterface.start();
        const result = await prologInterface.startQuery(query);
        return createSuccessResponse(
          `Query started: ${query}\nStatus: ${result.status}`,
          startTime,
          { query, status: result.status }
        );
      } catch (error) {
        return createErrorResponse(
          error instanceof Error ? error.message : String(error),
          startTime
        );
      }
    },
  },

  query_next: {
    title: "Get Next Solution",
    description: "Get next solution from active query or engine. Returns status='success' with bindings or status='done' when exhausted.",
    inputSchema: queryNextSchema,
    outputSchema: queryNextOutputSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();
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
          return createErrorResponse(result.error, startTime, { solution: null, status: "done" });
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
            content: [{ type: "text", text: `No more solutions available\nStatus: done\nProcessing time: ${processingTimeMs}ms` }],
            structuredContent: { solution: null, status: "done", processing_time_ms: processingTimeMs },
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
    },
  },

  query_close: {
    title: "Close Query",
    description: "Close active query or engine session to free resources. Auto-closes on new query but explicit close is best practice.",
    inputSchema: queryCloseSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();
      const sessionState = prologInterface.getSessionState();
      if (sessionState !== "query" && sessionState !== "query_completed" &&
          sessionState !== "engine" && sessionState !== "engine_completed") {
        const processingTimeMs = Date.now() - startTime;
        return {
          content: [{ type: "text", text: `No active session to close\nProcessing time: ${processingTimeMs}ms` }],
          structuredContent: { status: "no_active_session", processing_time_ms: processingTimeMs },
        };
      }

      try {
        const result = (sessionState === "query" || sessionState === "query_completed") ?
          await prologInterface.closeQuery() : await prologInterface.closeEngine();
        const processingTimeMs = Date.now() - startTime;
        const statusText = result.status === "no_active_query" || result.status === "no_active_engine" ? "closed" : result.status;
        return {
          content: [{ type: "text", text: `Session ${statusText}\nProcessing time: ${processingTimeMs}ms` }],
          structuredContent: { status: statusText, processing_time_ms: processingTimeMs },
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        return {
          content: [
            { type: "text", text: `Error: ${error instanceof Error ? error.message : String(error)}\nProcessing time: ${processingTimeMs}ms` },
          ],
          structuredContent: { error: error instanceof Error ? error.message : String(error), processing_time_ms: processingTimeMs },
          isError: true,
        };
      }
    },
  },

  symbols_list: {
    title: "List Predicates",
    description: "List all user-defined predicates currently in the knowledge base. Shows predicate names with arity.",
    inputSchema: symbolsListSchema,
    outputSchema: symbolsListOutputSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();
      try {
        await prologInterface.start();
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
          content: [{ type: "text", text: `Available predicates:\n${result}\nProcessing time: ${processingTimeMs}ms` }],
          structuredContent: { predicates, raw: result, processing_time_ms: processingTimeMs },
        };
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
            predicates: [],
            raw: "",
            processing_time_ms: processingTimeMs
          },
          isError: true,
        };
      }
    },
  },

  knowledge_base_assert: {
    title: "Assert Clause",
    description: "Add single fact or rule to knowledge base. Use for one-off assertions; see knowledge_base_assert_many for bulk operations.",
    inputSchema: knowledgeBaseAssertSchema,
    handler: async ({ fact }: { fact: string }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      if (!fact || typeof fact !== 'string') {
        return {
          content: [{ type: "text", text: "Error: fact parameter is required and must be a string" }],
          structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      if (fact.length > MAX_FACT_LENGTH) {
        return {
          content: [{ type: "text", text: `Error: fact must be a string with max ${MAX_FACT_LENGTH} characters` }],
          structuredContent: { error: "fact_too_long", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      try {
        const { outcomes } = await knowledgeBaseAssertClauses([fact]);
        const [{ clause, result, ok, threw }] = outcomes;
        const processingTimeMs = Date.now() - startTime;

        // Convert to ClauseOperationResult format
        const clauseResult: ClauseOperationResult = {
          clause,
          status: (ok && !threw) ? "success" : "error",
          message: result,
        };

        const formatted = formatClauseResults("assert", [clauseResult], processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: !ok || threw,
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        const errMsg = error instanceof Error ? error.message : String(error);

        const clauseResult: ClauseOperationResult = {
          clause: fact,
          status: "error",
          message: errMsg,
        };

        const formatted = formatClauseResults("assert", [clauseResult], processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: true,
        };
      }
    },
  },

  knowledge_base_assert_many: {
    title: "Assert Multiple Clauses",
    description: "Add multiple facts or rules in one operation. More efficient than repeated single assertions. Returns per-clause results.",
    inputSchema: knowledgeBaseAssertManySchema,
    handler: async ({ facts }: { facts: string[] }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      if (!Array.isArray(facts)) {
        return {
          content: [{ type: "text", text: "Error: facts parameter is required and must be an array of strings" }],
          structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      if (facts.length === 0) {
        return {
          content: [{ type: "text", text: "Error: at least one fact is required" }],
          structuredContent: { error: "no_facts_provided", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      for (const f of facts) {
        if (typeof f !== 'string' || f.length > MAX_FACT_LENGTH) {
          return {
            content: [{ type: "text", text: `Error: each fact must be a string with max ${MAX_FACT_LENGTH} characters` }],
            structuredContent: { error: "fact_too_long", processing_time_ms: Date.now() - startTime },
            isError: true,
          };
        }
      }

      try {
        const { outcomes, errorCount } = await knowledgeBaseAssertClauses(facts, { ignoreStartErrors: true });
        const processingTimeMs = Date.now() - startTime;

        // Convert outcomes to ClauseOperationResult format
        const clauseResults: ClauseOperationResult[] = outcomes.map(({ clause, result, ok, threw }) => ({
          clause,
          status: (ok && !threw) ? "success" : "error",
          message: result,
        }));

        const formatted = formatClauseResults("assert", clauseResults, processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: errorCount > 0,
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        return {
          content: [
            {
              type: "text",
              text: `Error: ${error instanceof Error ? error.message : String(error)}\nProcessing time: ${processingTimeMs}ms`,
            },
          ],
          structuredContent: { error: error instanceof Error ? error.message : String(error), processing_time_ms: processingTimeMs },
          isError: true,
        };
      }
    },
  },

  knowledge_base_retract: {
    title: "Retract Clause",
    description: "Remove single matching fact or rule from knowledge base. Removes first match only; see knowledge_base_retract_many for bulk.",
    inputSchema: knowledgeBaseRetractSchema,
    handler: async ({ fact }: { fact: string }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      if (!fact || typeof fact !== 'string') {
        return {
          content: [{ type: "text", text: "Error: fact parameter is required and must be a string" }],
          structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      if (fact.length > MAX_FACT_LENGTH) {
        return {
          content: [{ type: "text", text: `Error: fact must be a string with max ${MAX_FACT_LENGTH} characters` }],
          structuredContent: { error: "fact_too_long", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      try {
        await prologInterface.start();
        const single = fact.trim().replace(/\.$/, "");
        const result = await prologInterface.query(`retract(${single})`);
        const processingTimeMs = Date.now() - startTime;

        const clauseResult: ClauseOperationResult = {
          clause: single,
          status: result === "ok" ? "success" : "error",
          message: result,
        };

        const formatted = formatClauseResults("retract", [clauseResult], processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: result !== "ok",
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        const errMsg = error instanceof Error ? error.message : String(error);

        const clauseResult: ClauseOperationResult = {
          clause: fact,
          status: "error",
          message: errMsg,
        };

        const formatted = formatClauseResults("retract", [clauseResult], processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: true,
        };
      }
    },
  },

  knowledge_base_retract_many: {
    title: "Retract Multiple Clauses",
    description: "Remove multiple matching facts or rules in one operation. More efficient than repeated single retractions. Returns per-clause results.",
    inputSchema: knowledgeBaseRetractManySchema,
    handler: async ({ facts }: { facts: string[] }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      if (!Array.isArray(facts)) {
        return {
          content: [{ type: "text", text: "Error: facts parameter is required and must be an array of strings" }],
          structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      if (facts.length === 0) {
        return {
          content: [{ type: "text", text: "Error: at least one fact is required" }],
          structuredContent: { error: "no_facts_provided", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      for (const f of facts) {
        if (typeof f !== 'string' || f.length > MAX_FACT_LENGTH) {
          return {
            content: [{ type: "text", text: `Error: each fact must be a string with max ${MAX_FACT_LENGTH} characters` }],
            structuredContent: { error: "fact_too_long", processing_time_ms: Date.now() - startTime },
            isError: true,
          };
        }
      }

      try { await prologInterface.start(); } catch { }

      try {
        const clauseResults: ClauseOperationResult[] = [];
        let errorCount = 0;

        for (const singleRaw of facts) {
          const singleFact = singleRaw.trim().replace(/\.$/, "");
          try {
            const result = await prologInterface.query(`retract(${singleFact})`);
            const isOk = result === "ok";
            clauseResults.push({
              clause: singleFact,
              status: isOk ? "success" : "error",
              message: result,
            });
            if (!isOk) errorCount++;
          } catch (error) {
            const errMsg = error instanceof Error ? error.message : String(error);
            clauseResults.push({
              clause: singleFact,
              status: "error",
              message: errMsg,
            });
            errorCount++;
          }
        }

        const processingTimeMs = Date.now() - startTime;
        const formatted = formatClauseResults("retract", clauseResults, processingTimeMs);

        return {
          content: [{ type: "text", text: formatted.text }],
          structuredContent: formatted.structured,
          isError: errorCount > 0,
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        return {
          content: [
            {
              type: "text",
              text: `Error: ${error instanceof Error ? error.message : String(error)}\nProcessing time: ${processingTimeMs}ms`,
            },
          ],
          structuredContent: { error: error instanceof Error ? error.message : String(error), processing_time_ms: processingTimeMs },
          isError: true,
        };
      }
    },
  },

  knowledge_base_clear: {
    title: "Clear Knowledge Base",
    description: "Remove ALL user-defined facts and rules. Destructive operation - use when starting fresh or resetting workspace.",
    inputSchema: knowledgeBaseClearSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();
      try {
        await prologInterface.start();
        const result = await prologInterface.query("clear_knowledge_base");
        const processingTimeMs = Date.now() - startTime;

        const match = result.match(/removed\((\d+)\)/);
        const count = match ? parseInt(match[1]) : 0;

        return {
          content: [{
            type: "text",
            text: `Removed all facts and rules from knowledge base\nPredicates cleared: ${count}\nProcessing time: ${processingTimeMs}ms`
          }],
          structuredContent: {
            predicates_removed: count,
            processing_time_ms: processingTimeMs
          },
          isError: false
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        const errMsg = error instanceof Error ? error.message : String(error);
        return {
          content: [{
            type: "text",
            text: `Error clearing knowledge base: ${errMsg}\nProcessing time: ${processingTimeMs}ms`
          }],
          structuredContent: {
            error: errMsg,
            processing_time_ms: processingTimeMs
          },
          isError: true
        };
      }
    },
  },

  knowledge_base_load_library: {
    title: "Load Safe Library",
    description: "Load sandbox-approved library (clpfd, lists, apply, etc.) into knowledge_base module. No file access required.",
    inputSchema: knowledgeBaseLoadLibrarySchema,
    handler: async ({ library }: { library: string }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      const v = validateStringInput("library", library, 100);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }

      try {
        await prologInterface.start();
        await prologInterface.loadLibrary(library);
        const processingTimeMs = Date.now() - startTime;

        return {
          content: [{
            type: "text",
            text: `Successfully loaded library(${library}) into knowledge_base module\nProcessing time: ${processingTimeMs}ms`
          }],
          structuredContent: {
            library,
            processing_time_ms: processingTimeMs
          },
          isError: false
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        const errMsg = error instanceof Error ? error.message : String(error);
        return {
          content: [{
            type: "text",
            text: `Error loading library(${library}): ${errMsg}\nProcessing time: ${processingTimeMs}ms`
          }],
          structuredContent: {
            library,
            error: errMsg,
            processing_time_ms: processingTimeMs
          },
          isError: true
        };
      }
    },
  },

  query_startEngine: {
    title: "Start Query (Engine Mode)",
    description: "Start Prolog query using SWI-Prolog engine for true backtracking. Follow with query_next to iterate, query_close when done.",
    inputSchema: queryStartEngineSchema,
    handler: async ({ query }: { query: string }, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();

      const v = validateStringInput("query", query, MAX_QUERY_LENGTH);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }

      try {
        await prologInterface.start();
        const result = await prologInterface.startEngine(query);
        const processingTimeMs = Date.now() - startTime;
        return {
          content: [
            {
              type: "text",
              text: `Engine started: ${query}\nStatus: ${result.status}\nEngine ready: ${result.engine_ready}\nProcessing time: ${processingTimeMs}ms`,
            },
          ],
          structuredContent: { query, status: result.status, engine_ready: true, processing_time_ms: processingTimeMs },
        };
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
          structuredContent: { error: errMsg, engine_ready: false, processing_time_ms: processingTimeMs },
          isError: true,
        };
      }
    },
  },

  knowledge_base_dump: {
    title: "Dump Knowledge Base",
    description: "Export all user-defined facts and rules as Prolog source text. Useful for inspecting or backing up current state.",
    inputSchema: knowledgeBaseDumpSchema,
    handler: async (_args, _extra): Promise<CallToolResult> => {
      const startTime = Date.now();
      try {
        await prologInterface.start();
        const result = await prologInterface.query("dump_knowledge_base");
        const processingTimeMs = Date.now() - startTime;

        return {
          content: [{ type: "text", text: `Knowledge base dump:\n\n${result}\n\nProcessing time: ${processingTimeMs}ms` }],
          structuredContent: { dump: result, processing_time_ms: processingTimeMs },
        };
      } catch (error) {
        const processingTimeMs = Date.now() - startTime;
        return {
          content: [
            {
              type: "text",
              text: `Error: ${error instanceof Error ? error.message : String(error)}\nProcessing time: ${processingTimeMs}ms`,
            },
          ],
          structuredContent: { error: error instanceof Error ? error.message : String(error), processing_time_ms: processingTimeMs },
          isError: true,
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
