import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { inputSchemas, toolHandlers, prologInterface, getCapabilitiesSummary } from "./tools.js";
import { resolvePackageVersion } from "./meta.js";
import { prologPrompts } from "./prompts.js";

/**
 * SWI-Prolog MCP Server
 *
 * Provides tools to interact with SWI-Prolog:
 * - knowledge_base_load: Load Prolog files
 * - query_start: Start a Prolog query session (call_nth/2 mode)
 * - query_startEngine: Start a Prolog query session (engine mode)
 * - query_next: Get next solution from current query (unified for both modes)
 * - query_close: Close current query session (unified for both modes)
 * - symbols_list: List available predicates
 * - knowledge_base_assert: Add a single clause (fact/rule)
 * - knowledge_base_assert_many: Add multiple clauses (facts/rules)
 * - knowledge_base_retract: Remove a single clause (fact/rule)
 * - knowledge_base_retract_many: Remove multiple clauses (facts/rules)
 * - knowledge_base_clear: Remove all user-defined facts and rules
 * - knowledge_base_dump: Export current knowledge base
 */

// Create server instance
const server = new McpServer({
  name: "swipl-mcp-server",
  version: resolvePackageVersion(),
});

// No template; we expose only a single static resource for the knowledge base

// Register help tool (guidelines for agents)
server.registerTool(
  "help",
  {
    description: "Get usage guidelines and tips for this server (optional topic)",
    inputSchema: inputSchemas.help,
  },
  toolHandlers.help as any,
);

// Register license tool
server.registerTool(
  "license",
  {
    description: "Get the license text for this software",
    inputSchema: {} as any,
  },
  toolHandlers.license as any,
);

// Register knowledge_base_load tool
server.registerTool(
  "knowledge_base_load",
  {
    description: "Load a Prolog file into the knowledge base",
    inputSchema: inputSchemas.knowledgeBaseLoad,
  },
  toolHandlers.knowledgeBaseLoad as any,
);

// Register query_start tool
server.registerTool(
  "query_start",
  {
    description: "Start a new Prolog query session (call_nth/2 mode)",
    inputSchema: inputSchemas.queryStart,
  },
  toolHandlers.queryStart as any,
);

// Register query_next tool
server.registerTool(
  "query_next",
  {
    description: "Get the next solution from the current query (unified for both modes)",
    inputSchema: inputSchemas.queryNext,
  },
  toolHandlers.queryNext as any,
);

// Register query_close tool
server.registerTool(
  "query_close",
  {
    description: "Close the current query session (unified for both modes)",
    inputSchema: inputSchemas.queryClose,
  },
  toolHandlers.queryClose as any,
);

// Register symbols_list tool
server.registerTool(
  "symbols_list",
  {
    description: "List predicates available in the knowledge base",
    inputSchema: inputSchemas.symbolsList,
  },
  toolHandlers.symbolsList as any,
);

// (module_load tool removed for now to keep API minimal)

// Register knowledge_base_assert tool
server.registerTool(
  "knowledge_base_assert",
  {
    description: "Add a single clause (fact or rule) to the knowledge base",
    inputSchema: inputSchemas.knowledgeBaseAssert,
  },
  toolHandlers.knowledgeBaseAssert as any,
);

// Register knowledge_base_assert_many tool
server.registerTool(
  "knowledge_base_assert_many",
  {
    description: "Add multiple clauses (facts or rules) to the knowledge base",
    inputSchema: inputSchemas.knowledgeBaseAssertMany,
  },
  toolHandlers.knowledgeBaseAssertMany as any,
);

// Register knowledge_base_retract tool
server.registerTool(
  "knowledge_base_retract",
  {
    description: "Remove a single clause (fact or rule) from the knowledge base",
    inputSchema: inputSchemas.knowledgeBaseRetract,
  },
  toolHandlers.knowledgeBaseRetract as any,
);

// Register knowledge_base_retract_many tool
server.registerTool(
  "knowledge_base_retract_many",
  {
    description: "Remove multiple clauses (facts or rules) from the knowledge base",
    inputSchema: inputSchemas.knowledgeBaseRetractMany,
  },
  toolHandlers.knowledgeBaseRetractMany as any,
);

// Register knowledge_base_clear tool
server.registerTool(
  "knowledge_base_clear",
  {
    description: "Remove ALL user-defined facts and rules from the knowledge base",
    inputSchema: inputSchemas.knowledgeBaseClear,
  },
  toolHandlers.knowledgeBaseClear as any,
);

// Register query_startEngine tool
server.registerTool(
  "query_startEngine",
  {
    description: "Start a new Prolog query session using SWI-Prolog engines for true backtracking",
    inputSchema: inputSchemas.queryStartEngine,
  },
  toolHandlers.queryStartEngine as any,
);

// Register knowledge_base_dump tool
server.registerTool(
  "knowledge_base_dump",
  {
    description: "Export current knowledge base as Prolog facts",
    inputSchema: inputSchemas.knowledgeBaseDump,
  },
  toolHandlers.knowledgeBaseDump as any,
);

// Register minimal MCP Resources

// 2) (Example resource removed to keep the API minimal.)

// (Predicate-specific template removed; keep only module-level predicates template.)

// Two static resources: predicates listing and full dump
server.registerResource(
  "knowledge-base-predicates",
  "prolog://knowledge_base/predicates",
  {
    title: "Knowledge Base Predicates",
    description: "List predicates defined in the knowledge_base module",
    mimeType: "text/plain",
  },
  async (uri) => {
    await prologInterface.start();
    const preds = await prologInterface.query("list_module_predicates(knowledge_base)");
    const toLines = (s: string) => {
      const trimmed = String(s).trim();
      if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
        const inner = trimmed.slice(1, -1).trim();
        if (!inner) return "(no exported predicates)";
        return inner.split(",").map((p) => p.trim()).join("\n");
      }
      return trimmed || "(no exported predicates)";
    };
    return { contents: [{ uri: uri.href, text: toLines(preds) }] };
  },
);

server.registerResource(
  "knowledge-base-dump",
  "prolog://knowledge_base/dump",
  {
    title: "Knowledge Base Dump",
    description: "Export current knowledge base as Prolog clauses",
    mimeType: "text/prolog",
  },
  async (uri) => {
    await prologInterface.start();
    const dump = await prologInterface.query("dump_knowledge_base");
    return { contents: [{ uri: uri.href, text: dump }] };
  },
);

// Static resources for server metadata
server.registerResource(
  "help",
  "reference://help",
  {
    title: "Help",
    description: "Usage guidelines and tips for this server",
    mimeType: "text/plain",
  },
  async (uri) => {
    const res = await toolHandlers.help({});
    const text = Array.isArray(res.content) && (res.content as any)[0]?.text ? (res.content as any)[0].text : "";
    return { contents: [{ uri: uri.href, text }] };
  },
);

server.registerResource(
  "license",
  "reference://license",
  {
    title: "License",
    description: "License text for this software",
    mimeType: "text/plain",
  },
  async (uri) => {
    const res = await toolHandlers.license();
    const text = Array.isArray(res.content) && (res.content as any)[0]?.text ? (res.content as any)[0].text : "License unavailable";
    return { contents: [{ uri: uri.href, text }] };
  },
);

server.registerResource(
  "capabilities",
  "reference://capabilities",
  {
    title: "Capabilities",
    description: "Machine-readable summary of tools, modes, env, and safety",
    mimeType: "application/json",
  },
  async (uri) => {
    const caps = getCapabilitiesSummary();
    const json = JSON.stringify(caps, null, 2);
    return { contents: [{ uri: uri.href, text: json }] };
  },
);

// (RDF support intentionally omitted for now to keep the API minimal.)

// (No standalone modules resource; rely on template + completions.)

// Cheat‑sheet removed to keep API minimal; use modules + template instead.


// Register capabilities tool (machine-readable summary)
server.registerTool(
  "capabilities",
  {
    description: "Get a machine-readable summary of tools, modes, env, and safety",
    inputSchema: {} as any,
  },
  toolHandlers.capabilities as any,
);

// Register prompts for expert Prolog assistance
// These guide agents to use resources first for context, then tools efficiently

// Map prompt names to schema names
const promptSchemaMap: Record<string, string> = {
  "prolog_init_expert": "prologInitExpert",
  "prolog_quick_reference": "prologQuickReference",
  "prolog_analyze_knowledge_base": "prologAnalyzeKnowledgeBase",
  "prolog_knowledge_base_builder": "prologKnowledgeBaseBuilder",
  "prolog_query_optimizer": "prologQueryOptimizer"
};

for (const promptConfig of Object.values(prologPrompts)) {
  const schemaName = promptSchemaMap[promptConfig.name];
  if (schemaName) {
    server.registerPrompt(
      promptConfig.name,
      {
        title: promptConfig.title,
        description: promptConfig.description,
        argsSchema: (inputSchemas as any)[schemaName]
      },
      async (args: Record<string, unknown>) => {
        const stringArgs = Object.fromEntries(
          Object.entries(args).map(([key, value]) => [key, String(value)])
        );
        const promptMessages = promptConfig.messages(stringArgs);
        return {
          messages: promptMessages.map((msg: any) => ({
            role: msg.role,
            content: msg.content
          }))
        };
      }
    );
  }
}

/**
 * Start the MCP server
 */
async function main(): Promise<void> {
  const transport = new StdioServerTransport();
  await server.connect(transport);

  // Warm-up Prolog process non-blocking to avoid first-call races
  try { void prologInterface.start(); } catch { /* ignore */ }

  // Cleanup on exit and when client disconnects (stdio closed)
  let shuttingDown = false;
  const shutdown = (_reason: string) => {
    if (shuttingDown) return;
    shuttingDown = true;
    try { prologInterface.stop(); } catch { /* ignore */ }
    // Prefer graceful exit
    process.exit(0);
  };

  process.on("SIGTERM", () => shutdown("SIGTERM"));
  process.on("SIGINT", () => shutdown("SIGINT"));
  // When the MCP client closes stdio, terminate this server as well.
  // Use only 'close' (not 'end') and add a tiny grace period to allow flush.
  process.stdin.on("close", () => {
    const timer = setTimeout(() => shutdown("stdin_close"), 25);
    (timer as any).unref?.();
  });
}

// Start the server
main().catch((error) => {
  console.error("Server error:", error);
  process.exit(1);
});
