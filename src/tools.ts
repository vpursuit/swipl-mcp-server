import { readFile, access, constants } from "fs/promises";
import { fileURLToPath } from "url";
import { dirname, join, resolve } from "path";
import { PrologInterface } from "./PrologInterface.js";

type ToolResponse = {
  content: any[];
  structuredContent?: Record<string, unknown>;
  isError?: boolean;
};

// Global Prolog interface instance
const prologInterface = new PrologInterface();

// Schemas are centralized in './schemas.ts' and re-exported here
export { zodSchemas as inputSchemas, jsonSchemas } from "./schemas.js";

// JSON schemas are re-exported above

export const toolHandlers = {
  async help({ topic }: { topic?: string } = {}): Promise<ToolResponse> {
    // Build per-section lines so we can expose structured content too.
    const sectionsData: Record<string, string[]> = {
      overview: [
        "SWI‑Prolog MCP Server: tools for loading files, querying, managing database, and exploring symbols.",
        "Two modes: standard (call_nth/2) and engine (SWI engines). Unified query interface.",
      ],
      standard_mode: [
        "Standard mode (call_nth/2):",
        "- Tools: query_start → query_next → query_close",
        "- Deterministic pagination of solutions, memory‑efficient",
        "- Example: query_start {query: 'member(X, [1,2,3])'} then call query_next until 'No more solutions'",
      ],
      engine_mode: [
        "Engine mode (SWI engines):",
        "- Tools: query_startEngine → query_next → query_close",
        "- True backtracking iterator with no recomputation",
        "- Unified query_next and query_close work for both modes",
      ],
      safety: [
        "Hybrid Security Model:",
        "- User code asserted into module 'kb' with unknown=fail",
        "- Files loaded via guarded consult (facts/rules only; directives rejected)",
        "- library(sandbox) validates most built-in predicates as safe/unsafe",
        "- Explicit blacklist blocks dangerous operations: call/1, assert/1, system/1, shell/1",
        "- User-defined predicates in 'kb' module allowed for recursive definitions",
        "- Security is always enabled to ensure safe operation",
      ],
      security: [
        "Security Architecture:",
        "- Most built-ins validated by library(sandbox)",
        "- Safe built-ins: arithmetic, lists, term operations, logic, string/atom helpers",
        "- Dangerous operations explicitly blocked even if sandbox allows them",
        "- User predicates in kb/user modules permitted for recursion",
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
      // Find LICENSE file using same search pattern as Prolog server
      const moduleDir = (() => {
        try {
          // Try import.meta.url safely (works in both ESM and CommonJS/Jest)
          const metaUrl = Function("try { return import.meta && import.meta.url } catch (_) { return '' }")();
          if (metaUrl) {
            const currentModulePath = fileURLToPath(metaUrl);
            return dirname(currentModulePath);
          }
          // Fallback for CommonJS/Jest environment
          if (typeof __filename !== 'undefined') {
            return dirname(__filename);
          }
        } catch {}
        return "";
      })();

      const entryDir = (() => {
        try {
          return dirname(process.argv?.[1] || "");
        } catch {
          return "";
        }
      })();

      const candidates: string[] = [];
      
      // LICENSE relative to current working directory
      candidates.push(
        resolve(process.cwd(), "LICENSE"),
        resolve(process.cwd(), "..", "LICENSE"),
      );
      
      // Relative to compiled module (build folder)
      if (moduleDir) {
        candidates.push(
          resolve(moduleDir, "..", "LICENSE"),
          resolve(moduleDir, "LICENSE"), // Same folder as compiled JS
          resolve(moduleDir, "..", "..", "LICENSE"),
        );
      }
      
      // Relative to entry script directory
      if (entryDir) {
        candidates.push(
          resolve(entryDir, "..", "LICENSE"),
          resolve(entryDir, "LICENSE"), // Same folder as entry script
          resolve(entryDir, "..", "..", "LICENSE"),
        );
      }

      // De-duplicate paths
      const seen = new Set<string>();
      const possiblePaths = candidates.filter((p) => {
        if (!p) return false;
        if (seen.has(p)) return false;
        seen.add(p);
        return true;
      });

      let licensePath: string | null = null;
      
      // Search for LICENSE file
      for (const path of possiblePaths) {
        try {
          await access(path, constants.F_OK);
          licensePath = path;
          break;
        } catch {
          // File not found, continue searching
        }
      }

      if (!licensePath) {
        return {
          content: [
            {
              type: "text",
              text: `License file not found. Searched paths:\n${possiblePaths.join('\n')}`,
            },
          ],
          structuredContent: { error: "license_file_not_found", searched_paths: possiblePaths },
          isError: true,
        };
      }

      const licenseText = await readFile(licensePath, "utf8");
      return {
        content: [{ type: "text", text: licenseText }],
        structuredContent: { type: "license", text: licenseText, path: licensePath },
      };
    } catch (error) {
      return {
        content: [
          {
            type: "text",
            text: `Error reading license file: ${error instanceof Error ? error.message : String(error)}`,
          },
        ],
        isError: true,
      };
    }
  },


  async capabilities(): Promise<ToolResponse> {
    const caps = {
      server: { name: "swipl-mcp-server", version: "1.0.0" },
      modes: ["standard", "engine"],
      tools: {
        core: ["help", "license", "capabilities"],
        database: ["db_load", "db_assert", "db_retract", "db_dump"],
        query: ["query_start", "query_startEngine", "query_next", "query_close"],
        symbols: ["symbols_list"],
      },
      security: {
        module: "kb",
        consult: "facts/rules only; directives rejected",
        model: "hybrid",
        sandbox_validation: "library(sandbox) validates most built-ins",
        explicit_blacklist: ["call/1", "assert/1", "system/1", "shell/1", "retract/1", "abolish/1"],
        user_predicates: "allowed in kb/user modules for recursion",
        safe_categories: [
          "arithmetic", "lists", "term operations", "logic", 
          "string/atom helpers", "comparisons", "type checking"
        ],
        blocked_categories: ["I/O", "OS/process", "network", "external modules", "directives"],
      },
    } as const;

    // Human-readable plain text description
    const plainText = [
      "SWI-Prolog MCP Server v1.0.0",
      "",
      "Query Modes:",
      "- Standard: call_nth/2 pagination (query_start → query_next → query_close)",
      "- Engine: SWI engines with backtracking (query_startEngine → query_next → query_close)",
      "",
      "Available Tools:",
      "- Core: help, license, capabilities",
      "- Database: db_load, db_assert, db_retract, db_dump",
      "- Query: query_start, query_startEngine, query_next, query_close (unified)",
      "- Symbols: symbols_list",
      "",
      "Safety Features:",
      "- Queries execute in 'kb' module with unknown=fail",
      "- File consultation restricted to facts/rules (no directives)",
      "- Hybrid security: library(sandbox) + blacklist for critical operations",
      "- User-defined predicates in 'kb' module allowed for recursive definitions",
      "- Dangerous operations blocked: call/1, assert/1, system/1, shell/1, etc.",
      "",
      "Security Architecture:",
      "- library(sandbox) validates most built-in predicates",
      "- Explicit blacklist prevents dangerous operations sandbox allows",
      "- User predicates in kb/user modules permitted for recursion",
      "- Safe built-ins: arithmetic, lists, term operations, logic",
    ].join("\n");

    return {
      content: [{ type: "text", text: plainText }],
      structuredContent: caps as any,
    };
  },

  async dbLoad({ filename }: { filename: string }): Promise<ToolResponse> {
    const startTime = Date.now();
    
    // Input validation
    if (!filename || typeof filename !== 'string') {
      return {
        content: [{ type: "text", text: "Error: filename parameter is required and must be a string" }],
        structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
    }
    
    if (filename.length > 1000) {
      return {
        content: [{ type: "text", text: "Error: filename too long (max 1000 characters)" }],
        structuredContent: { error: "filename_too_long", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
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
        
        return {
          content: [{ type: "text", text: errorMessage }],
          structuredContent: { error: errorCode, filename, processing_time_ms: processingTimeMs },
          isError: true,
        };
      }

      const result = await prologInterface.consultFile(filename);
      const processingTimeMs = Date.now() - startTime;
      return {
        content: [
          { type: "text", text: `Successfully consulted file: ${filename}\nResult: ${result}\nProcessing time: ${processingTimeMs}ms` },
        ],
        structuredContent: { file: filename, result, processing_time_ms: processingTimeMs },
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

  async queryStart({ query }: { query: string }): Promise<ToolResponse> {
    const startTime = Date.now();
    
    // Input validation
    if (!query || typeof query !== 'string') {
      return {
        content: [{ type: "text", text: "Error: query parameter is required and must be a string" }],
        structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
    }
    
    if (query.length > 10000) {
      return {
        content: [{ type: "text", text: "Error: query too long (max 10000 characters)" }],
        structuredContent: { error: "query_too_long", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
    }
    
    try {
      await prologInterface.start();
      const result = await prologInterface.startQuery(query);
      const processingTimeMs = Date.now() - startTime;
      return {
        content: [
          {
            type: "text",
            text: `Query started: ${query}\nStatus: ${result.status}\nSolutions available: ${result.solutions_available}\nProcessing time: ${processingTimeMs}ms`,
          },
        ],
        structuredContent: {
          query,
          status: result.status,
          solutions_available: result.solutions_available,
          processing_time_ms: processingTimeMs,
        },
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

  async queryNext(): Promise<ToolResponse> {
    const startTime = Date.now();
    try {
      await prologInterface.start();
      
      // Detect session type and call appropriate method
      const sessionState = (prologInterface as any).sessionState;
      let result;
      
      if (sessionState === "query") {
        result = await prologInterface.nextSolution();
      } else if (sessionState === "engine") {
        result = await prologInterface.nextEngine();
      } else {
        return {
          content: [{ type: "text", text: `Error: No active query session\nProcessing time: ${Date.now() - startTime}ms` }],
          structuredContent: { error: "No active query session", more_solutions: false, processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }

      const processingTimeMs = Date.now() - startTime;

      if (result.error) {
        return {
          content: [{ type: "text", text: `Error: ${result.error}\nProcessing time: ${processingTimeMs}ms` }],
          structuredContent: { error: result.error, more_solutions: false, processing_time_ms: processingTimeMs },
          isError: true,
        };
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
    try {
      await prologInterface.start();
      
      // Detect session type and call appropriate method
      const sessionState = (prologInterface as any).sessionState;
      let result;
      
      if (sessionState === "query") {
        result = await prologInterface.closeQuery();
      } else if (sessionState === "engine") {
        result = await prologInterface.closeEngine();
      } else {
        return {
          content: [{ type: "text", text: `No active session to close\nProcessing time: ${Date.now() - startTime}ms` }],
          structuredContent: { status: "no_active_session", processing_time_ms: Date.now() - startTime },
        };
      }

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
        } catch {}
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

  async dbAssert({ fact }: { fact: string | string[] }): Promise<ToolResponse> {
    const startTime = Date.now();
    
    // Input validation
    if (!fact || (typeof fact !== 'string' && !Array.isArray(fact))) {
      return {
        content: [{ type: "text", text: "Error: fact parameter is required and must be a string or array of strings" }],
        structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
    }
    
    const facts = Array.isArray(fact) ? fact : [fact];
    if (facts.length === 0) {
      return {
        content: [{ type: "text", text: "Error: at least one fact is required" }],
        structuredContent: { error: "no_facts_provided", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
    }
    
    // Validate each fact
    for (const f of facts) {
      if (typeof f !== 'string' || f.length > 5000) {
        return {
          content: [{ type: "text", text: `Error: each fact must be a string with max 5000 characters` }],
          structuredContent: { error: "fact_too_long", processing_time_ms: Date.now() - startTime },
          isError: true,
        };
      }
    }
    
    try {
      await prologInterface.start();

      // Normalize: if a clause looks like a rule without parentheses, wrap it: head :- body -> (head :- body)
      const normalize = (cl: string) => {
        const trimmed = cl.trim().replace(/\.$/, ""); // remove trailing dot if present
        if (/:-/.test(trimmed) && !/^\s*\(/.test(trimmed)) {
          return `(${trimmed})`;
        }
        return trimmed;
      };
      const results: string[] = [];
      let successCount = 0;
      let errorCount = 0;

      for (const raw of facts) {
        const singleFact = normalize(raw);
        try {
          const result = await prologInterface.query(`assert(${singleFact})`);
          const isOk = result === "ok";
          if (isOk) {
            results.push(`✓ ${singleFact}: ${result}`);
            successCount++;
          } else {
            results.push(`✗ ${singleFact}: ${result}`);
            errorCount++;
          }
        } catch (error) {
          results.push(
            `✗ ${singleFact}: ${error instanceof Error ? error.message : String(error)}`,
          );
          errorCount++;
        }
      }

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

  async dbRetract({ fact }: { fact: string | string[] }): Promise<ToolResponse> {
    const startTime = Date.now();
    try {
      await prologInterface.start();

      const facts = Array.isArray(fact) ? fact : [fact];
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
          results.push(
            `✗ ${singleFact}: ${error instanceof Error ? error.message : String(error)}`,
          );
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

  async queryStartEngine({ query }: { query: string }): Promise<ToolResponse> {
    const startTime = Date.now();
    
    // Input validation
    if (!query || typeof query !== 'string') {
      return {
        content: [{ type: "text", text: "Error: query parameter is required and must be a string" }],
        structuredContent: { error: "invalid_input", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
    }
    
    if (query.length > 10000) {
      return {
        content: [{ type: "text", text: "Error: query too long (max 10000 characters)" }],
        structuredContent: { error: "query_too_long", processing_time_ms: Date.now() - startTime },
        isError: true,
      };
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

  async dbDump(): Promise<ToolResponse> {
    const startTime = Date.now();
    try {
      await prologInterface.start();
      const result = await prologInterface.query("dump_kb");
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
