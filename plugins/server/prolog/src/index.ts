/**
 * @vpursuit/mcp-server-prolog
 *
 * SWI-Prolog MCP plugin providing Prolog knowledge base and query tools
 */

import type { Plugin, PromptDefinitions } from "@vpursuit/mcp-server-core";
import { tools } from "./tools.js";
import { resources } from "./resources.js";
import { prologPrompts } from "./prompts.js";
import { prologInterface } from "./tools.js";
import { serverRef, logger } from "./logger.js";

// Re-export tools, resources, and prompts for direct access
export { tools, resources, prologPrompts };
export { prologInterface, toolHandlers, getCapabilitiesSummary } from "./tools.js";
export { PrologInterface } from "./PrologInterface.js";
export { zodSchemas, jsonSchemas } from "./schemas.js";
export { serverRef, logger } from "./logger.js";
export type { CapabilitiesSummary, SessionState } from "./types.js";

/**
 * Convert internal prompt format to Plugin PromptDefinitions format
 * Handlers now match SDK's PromptCallback signature with extra parameter
 */
function convertPromptsToDefinitions(): PromptDefinitions {
  const promptDefs: PromptDefinitions = {};

  for (const [key, prompt] of Object.entries(prologPrompts)) {
    promptDefs[key] = {
      name: prompt.name,
      description: prompt.description,
      arguments: prompt.arguments,
      handler: async (args: Record<string, string | undefined>, _extra) => {
        const messages = prompt.messages(args);
        return {
          messages: messages.map((msg) => ({
            role: msg.role,
            content: msg.content,
          })),
        };
      },
    };
  }

  return promptDefs;
}

/**
 * SWI-Prolog MCP Plugin
 *
 * Provides comprehensive Prolog knowledge base management and query capabilities:
 *
 * TOOLS:
 * - knowledge_base_load: Load Prolog files from filesystem
 * - knowledge_base_load_library: Load safe Prolog library without file operations
 * - knowledge_base_assert: Add single fact/rule to knowledge base
 * - knowledge_base_assert_many: Batch add facts/rules
 * - knowledge_base_retract: Remove single fact/rule
 * - knowledge_base_retract_many: Batch remove facts/rules
 * - knowledge_base_clear: Clear all user-defined facts/rules
 * - knowledge_base_dump: Export knowledge base as Prolog clauses
 * - query_start: Start query session (call_nth/2 mode)
 * - query_startEngine: Start query session (engine mode with true backtracking)
 * - query_next: Get next solution from current query
 * - query_close: Close current query session
 * - symbols_list: List available predicates
 * - capabilities: Get machine-readable capabilities summary
 * - help: Get usage guidelines
 * - license: Get license text
 *
 * RESOURCES:
 * - prolog://knowledge_base/predicates: List predicates in knowledge_base module
 * - prolog://knowledge_base/dump: Export current knowledge base
 * - reference://help: Usage guidelines
 * - reference://license: License text
 * - reference://logo: Server logo (SVG)
 * - reference://capabilities: Capabilities summary (JSON)
 *
 * PROMPTS:
 * - prolog_init_expert: Initialize expert Prolog context
 * - prolog_quick_reference: Get quick reference for common tasks
 * - prolog_analyze_knowledge_base: Analyze current knowledge base
 * - prolog_knowledge_base_builder: Build knowledge base from requirements
 * - prolog_query_optimizer: Optimize Prolog query performance
 * - prolog_logic_puzzle_solver: Solve logic puzzles using CLP(FD)
 *
 * SECURITY:
 * - File operations restricted to allowed directories
 * - Dangerous predicates blocked (shell, system, call, halt)
 * - All queries executed in sandboxed environment
 * - Pre-execution validation via library(sandbox)
 */
export const plugin: Plugin = {
  name: "mcp-prolog",
  version: "3.0.0",
  description: "SWI-Prolog knowledge base and query tools for MCP",

  tools,
  resources,
  prompts: convertPromptsToDefinitions(),

  async onInit(server) {
    // Set server reference for MCP-aware logging
    serverRef.current = server;

    logger.info("Initializing Prolog interface...");

    // Warm-up Prolog process non-blocking to avoid first-call races
    try {
      void prologInterface.start();
    } catch (error) {
      logger.warn("Could not pre-start Prolog process", { error: error instanceof Error ? error.message : String(error) });
    }

    logger.info("Plugin initialized");
  },

  async onShutdown() {
    logger.info("Shutting down Prolog interface...");

    try {
      await prologInterface.stop();
    } catch (error) {
      logger.warn("Warning during shutdown", { error: error instanceof Error ? error.message : String(error) });
    }

    logger.info("Plugin shutdown complete");
  },
};
