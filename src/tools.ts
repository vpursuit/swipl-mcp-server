import { readFile } from "fs/promises";
import { PrologInterface } from "./PrologInterface.js";
import { MAX_FILENAME_LENGTH, MAX_QUERY_LENGTH, MAX_FACT_LENGTH } from "./constants.js";
import { validateStringInput } from "./utils/validation.js";
import { createErrorResponse, createSuccessResponse } from "./utils/response.js";
import { resolvePackageVersion, findNearestFile } from "./meta.js";
import path from "path";
import os from "os";

type ToolResponse = {
  content: any[];
  structuredContent: Record<string, unknown>;
  isError?: boolean;
};

// Security: Only allow files within the designated directory
const ALLOWED_DIR = path.join(os.homedir(), '.swipl-mcp-server');

function validateFilePath(filename: string): { allowed: boolean; error?: string } {
  try {
    const absolutePath = path.resolve(filename);
    const relativePath = path.relative(ALLOWED_DIR, absolutePath);
    const isWithinAllowed = !relativePath.startsWith('..') && !path.isAbsolute(relativePath);
    
    if (isWithinAllowed) {
      return { allowed: true };
    }
    
    return {
      allowed: false,
      error: `Security Error: Files can only be loaded from ${ALLOWED_DIR}`
    };
  } catch (error) {
    return {
      allowed: false,
      error: `Security Error: Invalid file path`
    };
  }
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
    if (/:-/.test(trimmed) && !/^\s*\(/.test(trimmed)) {
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

// Schemas are centralized in './schemas.ts' and re-exported here
export { zodSchemas as inputSchemas, jsonSchemas } from "./schemas.js";

// JSON schemas are re-exported above

export const toolHandlers = {
  async help({ topic }: { topic?: string } = {}): Promise<ToolResponse> {
    // Build per-section lines so we can expose structured content too.
    const sectionsData: Record<string, string[]> = {
      overview: [
        "SWI‑Prolog MCP Server: tools for loading files, querying, managing the knowledge base, and exploring symbols.",
        "Two modes: standard (call_nth/2) and engine (SWI engines). Unified query interface.",
      ],
      standard_mode: [
        "Standard mode (call_nth/2):",
        "- Tools: query_start → query_next → query_close",
        "- Deterministic pagination of solutions, memory‑efficient",
        "- Example: query_start {query: 'member(X, [1,2,3])'} then call query_next until 'No more solutions'",
        "- After exhaustion, query_next returns 'No more solutions available' until explicitly closed",
      ],
      engine_mode: [
        "Engine mode (SWI engines):",
        "- Tools: query_startEngine → query_next → query_close",
        "- True backtracking iterator with no recomputation",
        "- Unified query_next and query_close work for both modes",
        "- After exhaustion, query_next returns 'No more solutions available' until explicitly closed",
      ],
      safety: [
        "Enhanced Security Model:",
        "- File Path Restrictions: Only ~/.swipl-mcp-server/ directory allowed for file operations",
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
        "- File Path Restrictions: System directories (/etc, /usr, /bin, /var, etc.) blocked",
        "- Only ~/.swipl-mcp-server/ directory allowed for file loading",
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
        "- Engine: query_startEngine {query: '(between(1,6,X), 0 is X mod 2)'} and call query_next",
        "- String helpers: sub_atom('hello_world',6,5,0,S) → S=world",
        "- Recursive rules: ancestor(X,Y) :- parent(X,Y). ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).",
        "See docs/examples.md for many more.",
      ],
      prompts: [
        "Expert Prompts (Start Here!):",
        "- prolog_init_expert: Initialize as Prolog expert (recommended first step)",
        "  Sets you up with full context from resources and expert knowledge",
        "- prolog_quick_reference: Get complete server overview and capabilities",
        "- prolog_analyze_knowledge_base: Analyze current knowledge base using resources",
        "- prolog_init_expert (with task): Expert reasoning for specific tasks",
        "- prolog_knowledge_base_builder: Build knowledge bases following best practices",
        "- prolog_query_optimizer: Optimize queries for performance",
        "",
        "Prompt Usage Pattern:",
        "1. Start with prolog_init_expert to become Prolog expert",
        "2. The prompt will guide you to read all resources first for context",
        "3. Then use tools efficiently based on discovered capabilities",
        "4. Use specialized prompts for specific tasks as needed",
      ],
      troubleshooting: [
        "Troubleshooting:",
        "- error(unsafe_goal(...)): goal rejected by hybrid security (uses dangerous built-ins)",
        "- Session conflicts: close current mode before starting the other",
        "- Timeouts: configure via environment variables in Claude Desktop config:",
        "  • SWI_MCP_READY_TIMEOUT_MS: server startup (default 5000ms, try 10000ms if slow)",
        "  • SWI_MCP_QUERY_TIMEOUT_MS: query execution (default 30000ms, increase for complex queries)",
        "  • Set in claude_desktop_config.json using 'env' property",
        "- Recursive clauses: now work properly with hybrid security model",
        "- Query hangs: increase SWI_MCP_QUERY_TIMEOUT_MS or check for infinite loops",
        "- Startup failures: increase SWI_MCP_READY_TIMEOUT_MS or check SWI-Prolog installation",
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
      "troubleshooting",
    ] as const;

    const selected = topic && includeOrder.includes(topic as any) ? [topic] : includeOrder;

    // Compose human-readable text
    const parts: string[] = [];
    for (const key of selected) {
      const lines = sectionsData[key];
      parts.push(...lines);
      if (key !== selected[selected.length - 1]) parts.push("");
    }

    // Build structured payload with per-section lines
    const structured: Record<string, unknown> = { type: "help", sections: selected };
    const contentBySection: Record<string, string[]> = {};
    for (const key of selected) contentBySection[key] = sectionsData[key];
    structured["contentBySection"] = contentBySection;

    return { content: [{ type: "text", text: parts.join("\n") }], structuredContent: structured };
  },


  async license(): Promise<ToolResponse> {
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
        // Omit absolute path to avoid disclosing local filesystem information
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


  async capabilities(): Promise<ToolResponse> {
    const caps = getCapabilitiesSummary();
    const json = JSON.stringify(caps, null, 2);
    return {
      content: [
        { type: "text", text: json },
      ],
      structuredContent: caps,
    };
  },

  async knowledgeBaseLoad({ filename }: { filename: string }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
    {
      const v = validateStringInput("filename", filename as any, MAX_FILENAME_LENGTH);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }
    }

    // Security validation - check file path
    const pathValidation = validateFilePath(filename);
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
        const processingTimeMs = Date.now() - startTime;
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

  async queryStart({ query }: { query: string }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
    {
      const v = validateStringInput("query", query as any, MAX_QUERY_LENGTH);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }
    }

    try {
      await prologInterface.start();
      const result = await prologInterface.startQuery(query);
      return createSuccessResponse(
        `Query started: ${query}\nStatus: ${result.status}\nSolutions available: ${result.solutions_available}`,
        startTime,
        { query, status: result.status, solutions_available: result.solutions_available }
      );
    } catch (error) {
      return createErrorResponse(
        error instanceof Error ? error.message : String(error),
        startTime
      );
    }
  },

  async queryNext(): Promise<ToolResponse> {
    const startTime = Date.now();
    try {
      await prologInterface.start();

      // Detect session type and call appropriate method
      const sessionState = (prologInterface as any).sessionState;
      let result;

      if (sessionState === "query" || sessionState === "query_completed") {
        result = await prologInterface.nextSolution();
      } else if (sessionState === "engine" || sessionState === "engine_completed") {
        result = await prologInterface.nextEngine();
      } else {
        return createErrorResponse(
          "No active query session", 
          startTime, 
          { more_solutions: false }
        );
      }

      const processingTimeMs = Date.now() - startTime;

      if (result.error) {
        return createErrorResponse(result.error, startTime, { more_solutions: false });
      }

      if (result.solution) {
        return {
          content: [
            {
              type: "text",
              text: `Solution: ${result.solution}\nMore solutions: ${result.more_solutions}\nProcessing time: ${processingTimeMs}ms`,
            },
          ],
          structuredContent: {
            solution: result.solution,
            more_solutions: !!result.more_solutions,
            processing_time_ms: processingTimeMs,
          },
        };
      } else {
        return {
          content: [{ type: "text", text: `No more solutions available\nProcessing time: ${processingTimeMs}ms` }],
          structuredContent: { solution: null, more_solutions: false, processing_time_ms: processingTimeMs },
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
        structuredContent: { error: error instanceof Error ? error.message : String(error), processing_time_ms: processingTimeMs },
        isError: true,
      };
    }
  },

  async queryClose(): Promise<ToolResponse> {
    const startTime = Date.now();
    // Detect session type without forcing a start; if none, exit gracefully
    const sessionState = (prologInterface as any).sessionState;
    if (sessionState !== "query" && sessionState !== "query_completed" && 
        sessionState !== "engine" && sessionState !== "engine_completed") {
      const processingTimeMs = Date.now() - startTime;
      return {
        content: [{ type: "text", text: `No active session to close\nProcessing time: ${processingTimeMs}ms` }],
        structuredContent: { status: "no_active_session", processing_time_ms: processingTimeMs },
      };
    }

    try {
      // If there is an active session, the interface is already started
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

  async symbolsList(): Promise<ToolResponse> {
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
        structuredContent: { error: error instanceof Error ? error.message : String(error), processing_time_ms: processingTimeMs },
        isError: true,
      };
    }
  },

  async knowledgeBaseAssert({ fact }: { fact: string }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
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
      if (threw) {
        const details = `✗ ${clause}: ${result}`;
        const summary = `Asserted 0/1 clauses successfully`;
        const text = `Error: ${result}\n\nDetails:\n${details}\n${summary}\nProcessing time: ${processingTimeMs}ms`;
        return {
          content: [{ type: "text", text }],
          structuredContent: {
            error: result,
            success: 0,
            total: 1,
            details: [details],
            processing_time_ms: processingTimeMs,
          },
          isError: true,
        };
      }

      return {
        content: [{ type: "text", text: `Asserted fact: ${clause}\nResult: ${result}\nProcessing time: ${processingTimeMs}ms` }],
        structuredContent: { fact: clause, result, processing_time_ms: processingTimeMs },
        isError: !ok,
      };
    } catch (error) {
      const processingTimeMs = Date.now() - startTime;
      const errMsg = error instanceof Error ? error.message : String(error);


      const details = `✗ ${fact}: ${errMsg}`;
      const summary = `Asserted 0/1 clauses successfully`;
      const text = `Error: ${errMsg}\n\nDetails:\n${details}\n${summary}\nProcessing time: ${processingTimeMs}ms`;
      return {
        content: [{ type: "text", text }],
        structuredContent: { error: errMsg, success: 0, total: 1, details: [details], processing_time_ms: processingTimeMs },
        isError: true,
      };
    }
  },

  async knowledgeBaseAssertMany({ facts }: { facts: string[] }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
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

    // Validate each fact
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
      const { outcomes, successCount, errorCount } = await knowledgeBaseAssertClauses(facts, { ignoreStartErrors: true });
      const results = outcomes.map(({ clause, result, ok }) => `${ok ? "✓" : "✗"} ${clause}: ${result}`);
      const processingTimeMs = Date.now() - startTime;
      const summary = `Asserted ${successCount}/${facts.length} clauses successfully`;
      const content = `${summary}\n\nDetails:\n${results.join("\n")}\nProcessing time: ${processingTimeMs}ms`;

      return {
        content: [{ type: "text", text: content }],
        structuredContent: { success: successCount, total: facts.length, details: results, processing_time_ms: processingTimeMs },
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

  async knowledgeBaseRetract({ fact }: { fact: string }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
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
      return {
        content: [{ type: "text", text: `Retracted fact: ${single}\nResult: ${result}\nProcessing time: ${processingTimeMs}ms` }],
        structuredContent: { fact: single, result, processing_time_ms: processingTimeMs },
        isError: result !== "ok",
      };
    } catch (error) {
      const processingTimeMs = Date.now() - startTime;
      const errMsg = error instanceof Error ? error.message : String(error);
      const details = `✗ ${fact}: ${errMsg}`;
      const summary = `Retracted 0/1 clauses successfully`;
      const text = `Error: ${errMsg}\n\nDetails:\n${details}\n${summary}\nProcessing time: ${processingTimeMs}ms`;
      return {
        content: [{ type: "text", text }],
        structuredContent: { error: errMsg, success: 0, total: 1, details: [details], processing_time_ms: processingTimeMs },
        isError: true,
      };
    }
  },

  async knowledgeBaseRetractMany({ facts }: { facts: string[] }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
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

    // Validate each fact
    for (const f of facts) {
      if (typeof f !== 'string' || f.length > MAX_FACT_LENGTH) {
        return {
          content: [{ type: "text", text: `Error: each fact must be a string with max ${MAX_FACT_LENGTH} characters` }],
          structuredContent: { error: "fact_too_long", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }
    }

    // Try to start; if it fails, proceed with summary aggregation
    try { await prologInterface.start(); } catch { /* proceed anyway for summary */ }

    try {
      const results: string[] = [];
      let successCount = 0;
      let errorCount = 0;

      for (const singleRaw of facts) {
        const singleFact = singleRaw.trim().replace(/\.$/, "");
        try {
          const result = await prologInterface.query(`retract(${singleFact})`);
          const isOk = result === "ok";
          if (isOk) {
            results.push(`✓ ${singleFact}: ${result}`);
            successCount++;
          } else {
            results.push(`✗ ${singleFact}: ${result}`);
            errorCount++;
          }
        } catch (error) {
          const errMsg = error instanceof Error ? error.message : String(error);
          results.push(`✗ ${singleFact}: ${errMsg}`);
          errorCount++;
        }
      }

      const processingTimeMs = Date.now() - startTime;
      const summary = `Retracted ${successCount}/${facts.length} clauses successfully`;
      const content = `${summary}\n\nDetails:\n${results.join("\n")}\nProcessing time: ${processingTimeMs}ms`;

      return {
        content: [{ type: "text", text: content }],
        structuredContent: { success: successCount, total: facts.length, details: results, processing_time_ms: processingTimeMs },
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

  async knowledgeBaseClear(): Promise<ToolResponse> {
    const startTime = Date.now();
    try {
      await prologInterface.start();
      const result = await prologInterface.query("clear_knowledge_base");
      const processingTimeMs = Date.now() - startTime;
      
      // Parse result to extract count
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

  async queryStartEngine({ query }: { query: string }): Promise<ToolResponse> {
    const startTime = Date.now();

    // Input validation
    {
      const v = validateStringInput("query", query as any, MAX_QUERY_LENGTH);
      if (!v.ok) {
        return createErrorResponse(v.error, startTime, { error_code: v.code });
      }
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

  async knowledgeBaseDump(): Promise<ToolResponse> {
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

};

export { prologInterface };

// Build a machine-readable summary of capabilities for the resource handler
export function getCapabilitiesSummary(): Record<string, unknown> {
  const version = resolvePackageVersion();
  const caps = {
    server: { name: "swipl-mcp-server", version },
    modes: ["standard", "engine"],
    tools: {
      core: ["help", "license", "capabilities"],
      knowledge_base: [
        "knowledge_base_load",
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
      expert_guidance: [
        "prolog_init_expert",
        "prolog_query_optimizer"
      ],
      knowledge_base: [
        "prolog_analyze_knowledge_base",
        "prolog_knowledge_base_builder"
      ],
      orientation: [
        "prolog_quick_reference"
      ]
    },
    security: {
      module: "knowledge_base",
      file_restrictions: {
        allowed_directory: "~/.swipl-mcp-server/",
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
        validation: "pre-execution path checking",
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
        error_format:
          "Security Error: Operation blocked - contains dangerous predicate 'X'",
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
  } as const;
  return caps as unknown as Record<string, unknown>;
}
