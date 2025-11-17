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
export { prologInterface, toolHandlers, toolNames, getCapabilitiesSummary } from "./tools.js";
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
 * - query: Unified tool to start queries, get next solutions, and close query sessions
 * - capabilities: Get machine-readable capabilities summary
 * - clauses: Unified tool to assert, retract, or clear facts/rules (with source preservation)
 * - files: Import/unimport Prolog files with provenance tracking
 * - workspace: Save, load, or list workspace snapshots
 * - explain_error: Analyze and explain Prolog errors using domain expertise
 *
 * RESOURCES:
 * - mcp://workspace/symbols: List predicates in workspace
 * - mcp://workspace/snapshot: Export workspace with original source text
 * - mcp://server/branding/logo: Server logo (SVG)
 * - mcp://server/capabilities: Capabilities summary (JSON)
 *
 * Note: LICENSE file included in npm package. See README for details.
 *
 * PROMPTS:
 * - genealogy: Build and query family trees using relational logic
 * - scheduling: Schedule tasks with dependencies using CLP(FD)
 * - puzzle: Solve logic puzzles using constraint programming
 * - grammar: Parse natural language using Definite Clause Grammars (DCGs)
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

    // Start Prolog process and ensure it's ready before continuing
    // This blocks plugin initialization to catch startup failures early
    try {
      await prologInterface.start();
      logger.info("Prolog interface started successfully");

      // Validate that Prolog is actually healthy after startup
      if (!prologInterface.isHealthy()) {
        const healthStatus = prologInterface.getHealthStatus();
        throw new Error(`Prolog health check failed: ${JSON.stringify(healthStatus)}`);
      }

      logger.info("Prolog health check passed");
    } catch (error) {
      logger.error("Failed to start Prolog process", { error: error instanceof Error ? error.message : String(error) });
      throw new Error(`Prolog initialization failed: ${error instanceof Error ? error.message : String(error)}`);
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
