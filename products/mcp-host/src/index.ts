/**
 * MCP Host - Generic MCP host for managing multiple Model Context Protocol servers
 *
 * This package provides a high-level API for connecting to and managing multiple
 * MCP servers, aggregating their capabilities, and integrating with OpenAI-compatible
 * LLM clients.
 *
 * @packageDocumentation
 */

// Re-export everything from the core implementation
export {
  // Main class
  McpHostManager,

  // Component classes (for advanced use)
  ServerConnection,
  SessionManager,
  CapabilityAggregator,
  OpenAIAdapter,
  SamplingHandler,

  // Utilities
  McpHostError,
  loadConfigFile,
  createNamespacedName,
  parseNamespacedName,
  withTimeout,
  sleep,
  validateServerName,
  sanitizeError,
  isTimeoutError,
  isConnectionError,
  toolResultToString,

  // Constants
  DEFAULT_CONNECTION_TIMEOUT_MS,
  DEFAULT_RECONNECT_DELAY_MS,
  DEFAULT_MAX_RECONNECT_ATTEMPTS,
  DEFAULT_TOOL_TIMEOUT_MS,
  DEFAULT_RESOURCE_TIMEOUT_MS,
  DEFAULT_CONFIG_FILE,
  NAMESPACE_SEPARATOR,
  ERROR_CODES,
  LOG_LEVELS,

  // Schemas
  serverConfigSchema,
  mcpConfigFileSchema,
  mcpHostConfigSchema,
  toolExecutionOptionsSchema,
  resourceReadOptionsSchema
} from '../../../plugins/client/mcp-host/dist/index.js';

// Re-export types separately
export type {
  ServerConfig,
  McpHostConfig,
  McpConfigFile,
  TransportType,
  ConnectionState,
  ServerInfo,
  NamespacedTool,
  NamespacedPrompt,
  NamespacedResource,
  AggregatedCapabilities,
  SamplingRequest,
  SamplingApproval,
  SamplingApprovalCallback,
  ToolExecutionOptions,
  ResourceReadOptions,
  ToolResult,
  PromptResult,
  ResourceResult,
  SamplingResult,
  OpenAIFunction,
  ProgressNotification,
  Tool,
  Prompt,
  Resource,
  CallToolResult,
  GetPromptResult,
  ReadResourceResult,
  SamplingMessage,
  CreateMessageResult,
  ClientCapabilities,
  ServerCapabilities,
  ServerConnectionEvent,
  SessionManagerEvent,
  CapabilityAggregatorEvent
} from '../../../plugins/client/mcp-host/dist/index.js';

/**
 * Convenience export: McpHost is an alias for McpHostManager
 */
export { McpHostManager as McpHost } from '../../../plugins/client/mcp-host/dist/index.js';
