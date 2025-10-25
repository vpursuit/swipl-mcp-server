/**
 * @vpursuit/mcp-core
 *
 * Plugin system for Model Context Protocol servers
 * Provides types and utilities for building plugin-based MCP servers
 */

// Export types
export type {
  Plugin,
  PluginLoaderConfig,
  ToolDefinition,
  ToolDefinitions,
  ToolResponse,
  ResourceDefinition,
  ResourceDefinitions,
  PromptDefinition,
  PromptDefinitions,
  PromptArgument,
} from "./types.js";

// Export loader functions
export { loadPlugin, loadPlugins } from "./loader.js";
