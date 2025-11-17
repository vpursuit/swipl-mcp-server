/**
 * @vpursuit/swipl-mcp-server
 *
 * MCP Server orchestrator that loads and integrates plugins:
 * - @vpursuit/mcp-server-roots: Dynamic filesystem root discovery
 * - @vpursuit/mcp-server-prolog: SWI-Prolog knowledge base and query tools
 */

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { loadPlugins } from "@vpursuit/mcp-server-core";
import { plugin as rootsPlugin } from "@vpursuit/mcp-server-roots";
import { plugin as prologPlugin, prologInterface } from "@vpursuit/mcp-server-prolog";
import { resolvePackageVersion } from "./meta.js";

/**
 * SWI-Prolog MCP Server
 *
 * Orchestrates multiple plugins to provide comprehensive Prolog and filesystem functionality:
 *
 * FROM @vpursuit/mcp-server-roots:
 * - roots_list: List filesystem roots for path validation
 *
 * FROM @vpursuit/mcp-server-prolog:
 * - query: Unified tool to start queries, get next solutions, and close query sessions
 * - capabilities: Get machine-readable capabilities summary
 * - clauses: Unified tool to assert or retract facts/rules (with source preservation)
 * - files: Import/unimport Prolog files with provenance tracking
 * - workspace: Manage workspace snapshots and list predicates
 * - explain_error: Analyze and explain Prolog errors using domain expertise
 *
 * RESOURCES:
 * - mcp://workspace/symbols: List all predicates in workspace
 * - mcp://workspace/snapshot: Export workspace with original source text
 * - mcp://server/branding/logo: Server logo (SVG)
 * - mcp://server/capabilities: Capabilities summary (JSON)
 *
 * LICENSING:
 * - MCP server wrapper: BSD-3-Clause (see LICENSE file in package)
 * - SWI-Prolog: Separate license (see https://www.swi-prolog.org/license.html)
 * - This wrapper does not license SWI-Prolog - users must install separately
 *
 * PROMPTS:
 * - genealogy: Build and query family trees using relational logic
 * - scheduling: Schedule tasks with dependencies using CLP(FD)
 * - puzzle: Solve logic puzzles using constraint programming
 * - grammar: Parse natural language using Definite Clause Grammars (DCGs)
 */

/**
 * Start the MCP server with all plugins loaded
 */
async function main(): Promise<void> {
  // Create server instance with logging capability
  const server = new McpServer(
    {
      name: "swipl-mcp-server",
      version: resolvePackageVersion(),
    },
    {
      capabilities: {
        logging: {},
        prompts: {},
        resources: {},
        tools: {},
      },
    }
  );

  // Load all plugins (they will set their own serverRefs in onInit)
  await loadPlugins(server, [rootsPlugin, prologPlugin], {
    continueOnError: false,
  });

  // Setup graceful shutdown
  let shuttingDown = false;
  let startupComplete = false;

  const shutdown = async (reason: string) => {
    if (shuttingDown) return;
    shuttingDown = true;
    console.error(`[swipl-mcp-server] Shutting down (${reason})...`);

    try {
      await prologInterface.stop();
    } catch {
      // Ignore errors during shutdown
    }

    process.exit(0);
  };

  // Handle termination signals
  process.on("SIGTERM", () => void shutdown("SIGTERM"));
  process.on("SIGINT", () => void shutdown("SIGINT"));

  // Handle stdin close (client disconnect)
  // Only trigger shutdown after startup completes to avoid premature termination
  process.stdin.on("close", () => {
    if (!startupComplete) {
      console.error("[swipl-mcp-server] Warning: stdin closed during startup");
      return;
    }
    const timer = setTimeout(() => void shutdown("stdin_close"), 25);
    (timer as any).unref?.();
  });

  // Connect transport
  const transport = new StdioServerTransport();

  // Add transport lifecycle logging
  transport.onclose = () => {
    console.error("[swipl-mcp-server] Transport closed");
    if (startupComplete) {
      void shutdown("transport_close");
    }
  };

  transport.onerror = (error: Error) => {
    console.error("[swipl-mcp-server] Transport error:", error.message);
  };

  console.error("[swipl-mcp-server] Connecting transport...");
  await server.connect(transport);

  // Mark startup as complete
  startupComplete = true;
  console.error("[swipl-mcp-server] Server started successfully");
}

// Start the server
main().catch((error) => {
  console.error("[swipl-mcp-server] Fatal error:", error);
  process.exit(1);
});
