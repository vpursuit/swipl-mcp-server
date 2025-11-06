/**
 * @vpursuit/mcp-server-core
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
  // Re-export SDK types for plugins
  CallToolResult,
  ReadResourceResult,
  GetPromptResult,
  RequestHandlerExtra,
  ServerRequest,
  ServerNotification,
} from "./types.js";

// Export loader functions
export { loadPlugin, loadPlugins } from "./loader.js";

// Export logger
export { createMcpLogger, type McpLogger, type LogLevel } from "./logger.js";

// Export executable finder utility
export { findExecutable, type FindExecutableOptions } from "./executable-finder.js";

// Export path utilities
export {
  redactPath,
  pathExists,
  getCurrentDir,
  getCwd,
  getModuleDir,
  getEntryDir,
  getFileCandidates,
  findFile,
  findNearestFile,
  resolvePackageVersion,
  type FindFileOptions
} from "./path-utils.js";
