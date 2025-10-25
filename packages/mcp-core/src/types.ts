import type { Server as McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { z } from "zod";

/**
 * Response from a tool handler
 */
export interface ToolResponse {
  success: boolean;
  data?: any;
  error?: string;
  metadata?: {
    processingTime?: number;
    [key: string]: any;
  };
}

/**
 * Definition of a single tool
 */
export interface ToolDefinition {
  description: string;
  inputSchema: z.ZodSchema;
  handler: (args: any) => Promise<ToolResponse>;
}

/**
 * Collection of tool definitions
 */
export interface ToolDefinitions {
  [toolName: string]: ToolDefinition;
}

/**
 * Definition of a single resource
 */
export interface ResourceDefinition {
  uri: string;
  name: string;
  description?: string;
  mimeType?: string;
  handler: () => Promise<{
    uri: string;
    name: string;
    description?: string;
    mimeType?: string;
    text?: string;
    blob?: string;
  }>;
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
 */
export interface PromptDefinition {
  name: string;
  description?: string;
  arguments?: PromptArgument[];
  handler: (args: Record<string, string>) => Promise<{
    description?: string;
    messages: Array<{
      role: "user" | "assistant";
      content: {
        type: "text" | "image" | "resource";
        text?: string;
        data?: string;
        mimeType?: string;
      };
    }>;
  }>;
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
