import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type {
  CallToolResult,
  ReadResourceResult,
  GetPromptResult,
  ServerRequest,
  ServerNotification,
} from "@modelcontextprotocol/sdk/types.js";
import type { RequestHandlerExtra } from "@modelcontextprotocol/sdk/shared/protocol.js";
import type { z } from "zod";

/**
 * Re-export SDK types for use by plugins
 */
export type {
  CallToolResult,
  ReadResourceResult,
  GetPromptResult,
  RequestHandlerExtra,
  ServerRequest,
  ServerNotification,
};

/**
 * Legacy ToolResponse type for backward compatibility with tests
 * This is a simplified alias to CallToolResult
 * @deprecated Use CallToolResult from SDK instead
 */
export type ToolResponse = CallToolResult;

/**
 * Definition of a single tool
 *
 * Note: inputSchema should be a ZodRawShape (plain object with Zod schemas as values),
 * not a wrapped ZodObject. Example: { param: z.string() } not z.object({ param: z.string() })
 *
 * Handler signature: (args, extra) => Promise<CallToolResult>
 * - args: Parsed tool arguments (empty object {} for tools with no params)
 * - extra: Request context from MCP SDK
 * - Returns: CallToolResult with content array
 */
export interface ToolDefinition {
  title?: string;
  description: string;
  inputSchema: z.ZodRawShape;
  outputSchema?: z.ZodRawShape;
  handler: (
    args: any,
    extra: RequestHandlerExtra<ServerRequest, ServerNotification>
  ) => Promise<CallToolResult>;
}

/**
 * Collection of tool definitions
 */
export interface ToolDefinitions {
  [toolName: string]: ToolDefinition;
}

/**
 * Definition of a single resource
 *
 * Handler signature: (uri, extra) => Promise<ReadResourceResult>
 * - uri: URL object of the resource being read
 * - extra: Request context from MCP SDK
 * - Returns: ReadResourceResult with contents array
 */
export interface ResourceDefinition {
  uri: string;
  name: string;
  description?: string;
  mimeType?: string;
  handler: (
    uri: URL,
    extra: RequestHandlerExtra<ServerRequest, ServerNotification>
  ) => Promise<ReadResourceResult>;
}

/**
 * Collection of resource definitions
 */
export interface ResourceDefinitions {
  [resourceKey: string]: ResourceDefinition;
}

/**
 * Argument definition for prompts
 */
export interface PromptArgument {
  name: string;
  description?: string;
  required?: boolean;
}

/**
 * Definition of a single prompt
 *
 * Handler signature: (args, extra) => Promise<GetPromptResult>
 * - args: Prompt arguments (Record<string, string | undefined>)
 * - extra: Request context from MCP SDK
 * - Returns: GetPromptResult with messages array
 */
export interface PromptDefinition {
  name: string;
  title?: string;
  description?: string;
  arguments?: PromptArgument[];
  handler: (
    args: Record<string, string | undefined>,
    extra: RequestHandlerExtra<ServerRequest, ServerNotification>
  ) => Promise<GetPromptResult>;
}

/**
 * Collection of prompt definitions
 */
export interface PromptDefinitions {
  [promptName: string]: PromptDefinition;
}

/**
 * Main plugin interface
 */
export interface Plugin {
  /** Unique plugin name */
  name: string;

  /** Plugin version (semver) */
  version: string;

  /** Short description of plugin functionality */
  description: string;

  /** Tool definitions provided by this plugin */
  tools?: ToolDefinitions;

  /** Resource definitions provided by this plugin */
  resources?: ResourceDefinitions;

  /** Prompt definitions provided by this plugin */
  prompts?: PromptDefinitions;

  /**
   * Optional initialization hook called when plugin is loaded
   * @param server - MCP server instance
   */
  onInit?: (server: McpServer) => Promise<void> | void;

  /**
   * Optional cleanup hook called when server shuts down
   */
  onShutdown?: () => Promise<void> | void;
}

/**
 * Configuration for plugin loader
 */
export interface PluginLoaderConfig {
  /** Whether to continue loading other plugins if one fails */
  continueOnError?: boolean;

  /** Custom logger for plugin loading events */
  logger?: {
    info: (message: string) => void;
    error: (message: string, error?: Error) => void;
    warn: (message: string) => void;
  };
}
