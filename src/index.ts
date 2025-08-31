import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { inputSchemas, toolHandlers, prologInterface } from "./tools.js";
import { resolvePackageVersion } from "./meta.js";

/**
 * SWI-Prolog MCP Server
 *
 * Provides tools to interact with SWI-Prolog:
 * - db_load: Load Prolog files
 * - query_start: Start a Prolog query session (call_nth/2 mode)
 * - query_startEngine: Start a Prolog query session (engine mode)
 * - query_next: Get next solution from current query (unified for both modes)
 * - query_close: Close current query session (unified for both modes)
 * - symbols_list: List available predicates
 * - db_assert: Add a single clause (fact/rule)
 * - db_assert_many: Add multiple clauses (facts/rules)
 * - db_retract: Remove a single clause (fact/rule)  
 * - db_retract_many: Remove multiple clauses (facts/rules)
 * - db_retract_all: Remove all user-defined facts and rules
 * - db_dump: Export current knowledge base
 */

// Create server instance
const server = new McpServer({
  name: "swipl-mcp-server",
  version: resolvePackageVersion(),
});

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

// Register db_load tool
server.registerTool(
  "db_load",
  {
    description: "Load a Prolog file into the knowledge base",
    inputSchema: inputSchemas.dbLoad,
  },
  toolHandlers.dbLoad as any,
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

// Register db_assert tool
server.registerTool(
  "db_assert",
  {
    description: "Add a single clause (fact or rule) to the knowledge base",
    inputSchema: inputSchemas.dbAssert,
  },
  toolHandlers.dbAssert as any,
);

// Register db_assert_many tool
server.registerTool(
  "db_assert_many",
  {
    description: "Add multiple clauses (facts or rules) to the knowledge base",
    inputSchema: inputSchemas.dbAssertMany,
  },
  toolHandlers.dbAssertMany as any,
);

// Register db_retract tool
server.registerTool(
  "db_retract",
  {
    description: "Remove a single clause (fact or rule) from the knowledge base",
    inputSchema: inputSchemas.dbRetract,
  },
  toolHandlers.dbRetract as any,
);

// Register db_retract_many tool
server.registerTool(
  "db_retract_many",
  {
    description: "Remove multiple clauses (facts or rules) from the knowledge base",
    inputSchema: inputSchemas.dbRetractMany,
  },
  toolHandlers.dbRetractMany as any,
);

// Register db_retract_all tool
server.registerTool(
  "db_retract_all",
  {
    description: "Remove ALL user-defined facts and rules from the knowledge base",
    inputSchema: inputSchemas.dbRetractAll,
  },
  toolHandlers.dbRetractAll as any,
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

// Register db_dump tool
server.registerTool(
  "db_dump",
  {
    description: "Export current knowledge base as Prolog facts",
    inputSchema: inputSchemas.dbDump,
  },
  toolHandlers.dbDump as any,
);


// Register capabilities tool (machine-readable summary)
server.registerTool(
  "capabilities",
  {
    description: "Get a machine-readable summary of tools, modes, env, and safety",
    inputSchema: {} as any,
  },
  toolHandlers.capabilities as any,
);



/**
 * Start the MCP server
 */
async function main(): Promise<void> {
  const transport = new StdioServerTransport();
  await server.connect(transport);

  // Cleanup on exit
  process.on("SIGTERM", () => {
    prologInterface.stop();
    process.exit(0);
  });

  process.on("SIGINT", () => {
    prologInterface.stop();
    process.exit(0);
  });
}

// Start the server
main().catch((error) => {
  console.error("Server error:", error);
  process.exit(1);
});
