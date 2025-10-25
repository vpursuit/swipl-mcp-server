#!/usr/bin/env node
/**
 * @vpursuit/swipl-mcp-server
 *
 * MCP Server orchestrator that loads and integrates plugins:
 * - @vpursuit/mcp-roots: Dynamic filesystem root discovery
 * - @vpursuit/mcp-prolog: SWI-Prolog knowledge base and query tools
 */

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { loadPlugins } from "@vpursuit/mcp-core";
import { plugin as rootsPlugin } from "@vpursuit/mcp-roots";
import { plugin as prologPlugin, prologInterface } from "@vpursuit/mcp-prolog";
import { resolvePackageVersion } from "./meta.js";

/**
 * SWI-Prolog MCP Server
 *
 * Orchestrates multiple plugins to provide comprehensive Prolog and filesystem functionality:
 *
 * FROM @vpursuit/mcp-roots:
 * - roots_list: List filesystem roots for path validation
 *
 * FROM @vpursuit/mcp-prolog:
 * - knowledge_base_load: Load Prolog files
 * - knowledge_base_assert: Add facts/rules
 * - knowledge_base_assert_many: Batch add facts/rules
 * - knowledge_base_retract: Remove facts/rules
 * - knowledge_base_retract_many: Batch remove facts/rules
 * - knowledge_base_clear: Clear knowledge base
 * - knowledge_base_dump: Export knowledge base
 * - query_start: Start query session (call_nth/2 mode)
 * - query_startEngine: Start query session (engine mode)
 * - query_next: Get next solution
 * - query_close: Close query session
 * - symbols_list: List predicates
 * - capabilities: Get capabilities summary
 * - help: Get usage guidelines
 * - license: Get license text
 *
 * RESOURCES:
 * - prolog://knowledge_base/predicates
 * - prolog://knowledge_base/dump
 * - reference://help
 * - reference://license
 * - reference://logo
 * - reference://capabilities
 *
 * PROMPTS:
 * - prolog_init_expert: Initialize expert Prolog context
 * - prolog_quick_reference: Get quick reference
 * - prolog_analyze_knowledge_base: Analyze knowledge base
 * - prolog_knowledge_base_builder: Build knowledge base from requirements
 * - prolog_query_optimizer: Optimize query performance
 */

/**
 * Start the MCP server with all plugins loaded
 */
async function main(): Promise<void> {
  // Create server instance with logging capability
  const server = new McpServer({
    name: "swipl-mcp-server",
    version: resolvePackageVersion(),
    capabilities: {
      logging: {},
    },
  });

  // Load all plugins (they will set their own serverRefs in onInit)
  await loadPlugins(server, [rootsPlugin, prologPlugin], {
    continueOnError: false,
  });

  // Connect transport
  const transport = new StdioServerTransport();
  await server.connect(transport);

  console.error("[swipl-mcp-server] Server started successfully");

  // Setup graceful shutdown
  let shuttingDown = false;
  const shutdown = (reason: string) => {
    if (shuttingDown) return;
    shuttingDown = true;
    console.error(`[swipl-mcp-server] Shutting down (${reason})...`);

    try {
      prologInterface.stop();
    } catch {
      // Ignore errors during shutdown
    }

    process.exit(0);
  };

  // Handle termination signals
  process.on("SIGTERM", () => shutdown("SIGTERM"));
  process.on("SIGINT", () => shutdown("SIGINT"));

  // Handle stdin close (client disconnect)
  process.stdin.on("close", () => {
    const timer = setTimeout(() => shutdown("stdin_close"), 25);
    (timer as any).unref?.();
  });
}

// Start the server
main().catch((error) => {
  console.error("[swipl-mcp-server] Fatal error:", error);
  process.exit(1);
});
