/**
 * MCP Client Host Library
 * Generic MCP client for connecting to Model Context Protocol servers
 */

import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import { SSEClientTransport } from "@modelcontextprotocol/sdk/client/sse.js";
import type {
  Tool,
  Prompt,
  Resource,
  CallToolResult,
  GetPromptResult,
  ReadResourceResult,
  ClientCapabilities,
  ServerCapabilities,
} from "@modelcontextprotocol/sdk/types.js";
import type {
  SamplingMessage,
  CreateMessageResult,
} from "@modelcontextprotocol/sdk/types.js";
import { z } from "zod";
import { mcpHostLogger } from "./logger.js";

// ============================================================================
// Constants
// ============================================================================

export const DEFAULT_CONNECTION_TIMEOUT_MS = 30000;
export const DEFAULT_RECONNECT_DELAY_MS = 5000;
export const DEFAULT_MAX_RECONNECT_ATTEMPTS = 3;
export const DEFAULT_TOOL_TIMEOUT_MS = 60000;
export const DEFAULT_RESOURCE_TIMEOUT_MS = 30000;
export const DEFAULT_CONFIG_FILE = "mcp-config.json";
export const NAMESPACE_SEPARATOR = ":";

export const ERROR_CODES = {
  CONNECTION_FAILED: "CONNECTION_FAILED",
  CONNECTION_TIMEOUT: "CONNECTION_TIMEOUT",
  SERVER_NOT_FOUND: "SERVER_NOT_FOUND",
  TOOL_EXECUTION_FAILED: "TOOL_EXECUTION_FAILED",
  RESOURCE_READ_FAILED: "RESOURCE_READ_FAILED",
  PROMPT_GET_FAILED: "PROMPT_GET_FAILED",
  INVALID_CONFIG: "INVALID_CONFIG",
  SAMPLING_FAILED: "SAMPLING_FAILED",
} as const;

export const LOG_LEVELS = {
  DEBUG: "debug",
  INFO: "info",
  WARN: "warn",
  ERROR: "error",
} as const;

// ============================================================================
// Types
// ============================================================================

export type TransportType = "stdio" | "sse";
export type ConnectionState = "disconnected" | "connecting" | "connected" | "reconnecting" | "failed";
export type LogLevel = typeof LOG_LEVELS[keyof typeof LOG_LEVELS];

export interface ServerConfig {
  name: string;
  command?: string;
  args?: string[];
  env?: Record<string, string>;
  url?: string;
  transport: TransportType;
  autoReconnect?: boolean;
  maxReconnectAttempts?: number;
  reconnectDelay?: number;
}

export interface McpHostConfig {
  servers: ServerConfig[];
  defaultTimeout?: number;
  logLevel?: LogLevel;
}

export interface McpConfigFile {
  mcpServers: Record<string, Omit<ServerConfig, "name">>;
}

export interface ServerInfo {
  name: string;
  state: ConnectionState;
  capabilities?: ServerCapabilities;
  lastError?: string;
}

export interface NamespacedTool extends Tool {
  serverName: string;
  namespacedName: string;
}

export interface NamespacedPrompt extends Prompt {
  serverName: string;
  namespacedName: string;
}

export interface NamespacedResource extends Resource {
  serverName: string;
  namespacedName: string;
}

export interface AggregatedCapabilities {
  tools: NamespacedTool[];
  prompts: NamespacedPrompt[];
  resources: NamespacedResource[];
}

export interface SamplingRequest {
  serverName: string;
  messages: SamplingMessage[];
  modelPreferences?: {
    hints?: Array<{ name?: string }>;
    costPriority?: number;
    speedPriority?: number;
    intelligencePriority?: number;
  };
  systemPrompt?: string;
  maxTokens?: number;
}

export interface SamplingApproval {
  approved: boolean;
  reason?: string;
  /**
   * Optional LLM client to use for the sampling request.
   * Should have OpenAI-compatible chat.completions.create() interface.
   */
  llmClient?: any;
}

export type SamplingApprovalCallback = (request: SamplingRequest) => Promise<SamplingApproval>;

export interface ToolExecutionOptions {
  timeout?: number;
  throwOnError?: boolean;
}

export interface ResourceReadOptions {
  timeout?: number;
  throwOnError?: boolean;
}

export interface ToolResult {
  serverName: string;
  toolName: string;
  result: CallToolResult;
  executionTime: number;
}

export interface PromptResult {
  serverName: string;
  promptName: string;
  result: GetPromptResult;
}

export interface ResourceResult {
  serverName: string;
  resourceUri: string;
  result: ReadResourceResult;
}

export interface SamplingResult {
  serverName: string;
  result: CreateMessageResult;
}

export interface OpenAIFunction {
  name: string;
  description?: string;
  parameters: {
    type: "object";
    properties: Record<string, unknown>;
    required?: string[];
  };
}

export interface ProgressNotification {
  serverName: string;
  progress: number;
  total?: number;
}

export type ServerConnectionEvent = {
  type: "connected" | "disconnected" | "error" | "reconnecting";
  serverName: string;
  error?: Error;
};

export type SessionManagerEvent = {
  type: "session_started" | "session_ended";
  sessionId: string;
};

export type CapabilityAggregatorEvent = {
  type: "capabilities_updated";
  serverName: string;
};

// Re-export MCP SDK types
export type {
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
};

// ============================================================================
// Schemas
// ============================================================================

export const serverConfigSchema = z.object({
  name: z.string().min(1),
  command: z.string().optional(),
  args: z.array(z.string()).optional(),
  env: z.record(z.string()).optional(),
  url: z.string().url().optional(),
  transport: z.enum(["stdio", "sse"]),
  autoReconnect: z.boolean().optional().default(true),
  maxReconnectAttempts: z.number().optional().default(DEFAULT_MAX_RECONNECT_ATTEMPTS),
  reconnectDelay: z.number().optional().default(DEFAULT_RECONNECT_DELAY_MS),
});

export const mcpConfigFileSchema = z.object({
  mcpServers: z.record(
    z.object({
      command: z.string().optional(),
      args: z.array(z.string()).optional(),
      env: z.record(z.string()).optional(),
      url: z.string().url().optional(),
      transport: z.enum(["stdio", "sse"]),
      autoReconnect: z.boolean().optional(),
      maxReconnectAttempts: z.number().optional(),
      reconnectDelay: z.number().optional(),
    })
  ),
});

export const mcpHostConfigSchema = z.object({
  servers: z.array(serverConfigSchema),
  defaultTimeout: z.number().optional(),
  logLevel: z.enum(["debug", "info", "warn", "error"]).optional(),
});

export const toolExecutionOptionsSchema = z.object({
  timeout: z.number().optional(),
  throwOnError: z.boolean().optional(),
});

export const resourceReadOptionsSchema = z.object({
  timeout: z.number().optional(),
  throwOnError: z.boolean().optional(),
});

// ============================================================================
// Error Classes
// ============================================================================

export class McpHostError extends Error {
  code: string;
  serverName?: string;

  constructor(message: string, code: string, serverName?: string) {
    super(message);
    this.name = "McpHostError";
    this.code = code;
    this.serverName = serverName;
  }
}

// ============================================================================
// Utility Functions
// ============================================================================

export function createNamespacedName(serverName: string, name: string): string {
  return `${serverName}${NAMESPACE_SEPARATOR}${name}`;
}

export function parseNamespacedName(namespacedName: string): { serverName: string; name: string } {
  const parts = namespacedName.split(NAMESPACE_SEPARATOR);
  if (parts.length !== 2) {
    throw new Error(`Invalid namespaced name: ${namespacedName}`);
  }
  return { serverName: parts[0], name: parts[1] };
}

export function validateServerName(name: string): boolean {
  return /^[a-zA-Z0-9_-]+$/.test(name);
}

export function sanitizeError(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
}

export function isTimeoutError(error: unknown): boolean {
  if (error instanceof Error) {
    return error.message.includes("timeout") || error.message.includes("ETIMEDOUT");
  }
  return false;
}

export function isConnectionError(error: unknown): boolean {
  if (error instanceof McpHostError) {
    return error.code === ERROR_CODES.CONNECTION_FAILED || error.code === ERROR_CODES.CONNECTION_TIMEOUT;
  }
  return false;
}

export function toolResultToString(result: CallToolResult): string {
  return result.content.map(c => {
    if (c.type === "text") return c.text;
    if (c.type === "image") return `[Image: ${c.mimeType}]`;
    if (c.type === "resource") return `[Resource: ${c.resource.uri}]`;
    return "[Unknown content type]";
  }).join("\n");
}

export async function withTimeout<T>(promise: Promise<T>, timeoutMs: number): Promise<T> {
  return Promise.race([
    promise,
    new Promise<T>((_, reject) =>
      setTimeout(() => reject(new Error(`Operation timed out after ${timeoutMs}ms`)), timeoutMs)
    ),
  ]);
}

export function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export async function loadConfigFile(path: string): Promise<McpHostConfig> {
  const { readFile } = await import("fs/promises");
  const content = await readFile(path, "utf-8");
  const data = JSON.parse(content);

  const parsed = mcpConfigFileSchema.parse(data);

  const servers: ServerConfig[] = Object.entries(parsed.mcpServers).map(([name, config]) => ({
    name,
    ...config,
  }));

  return { servers };
}

// ============================================================================
// ServerConnection Class
// ============================================================================

export class ServerConnection {
  private client: Client;
  private transport: StdioClientTransport | SSEClientTransport | null = null;
  private _state: ConnectionState = "disconnected";
  private reconnectAttempts = 0;
  private eventHandlers: Map<string, ((event: ServerConnectionEvent) => void)[]> = new Map();

  constructor(
    public readonly config: ServerConfig,
    private logger: (level: LogLevel, message: string) => void = () => {}
  ) {
    this.client = new Client(
      {
        name: `mcp-host-client-${config.name}`,
        version: "1.0.0",
      },
      {
        capabilities: {},
      }
    );
  }

  get state(): ConnectionState {
    return this._state;
  }

  get capabilities(): ServerCapabilities | undefined {
    return this.client.getServerCapabilities();
  }

  async connect(): Promise<void> {
    if (this._state === "connected") {
      return;
    }

    this._state = "connecting";
    this.emit({ type: "reconnecting", serverName: this.config.name });

    try {
      if (this.config.transport === "stdio") {
        if (!this.config.command) {
          throw new McpHostError("Stdio transport requires command", ERROR_CODES.INVALID_CONFIG, this.config.name);
        }
        this.transport = new StdioClientTransport({
          command: this.config.command,
          args: this.config.args || [],
          env: this.config.env,
        });
      } else if (this.config.transport === "sse") {
        if (!this.config.url) {
          throw new McpHostError("SSE transport requires URL", ERROR_CODES.INVALID_CONFIG, this.config.name);
        }
        this.transport = new SSEClientTransport(new URL(this.config.url));
      } else {
        throw new McpHostError(`Unsupported transport: ${this.config.transport}`, ERROR_CODES.INVALID_CONFIG, this.config.name);
      }

      await withTimeout(
        this.client.connect(this.transport),
        DEFAULT_CONNECTION_TIMEOUT_MS
      );

      this._state = "connected";
      this.reconnectAttempts = 0;
      this.emit({ type: "connected", serverName: this.config.name });
      this.logger("info", `Connected to server: ${this.config.name}`);
    } catch (error) {
      this._state = "failed";
      const mcpError = new McpHostError(
        `Failed to connect to ${this.config.name}: ${sanitizeError(error)}`,
        ERROR_CODES.CONNECTION_FAILED,
        this.config.name
      );
      this.emit({ type: "error", serverName: this.config.name, error: mcpError });
      throw mcpError;
    }
  }

  async disconnect(): Promise<void> {
    if (this.transport) {
      await this.client.close();
      this.transport = null;
    }
    this._state = "disconnected";
    this.emit({ type: "disconnected", serverName: this.config.name });
  }

  async callTool(name: string, args: Record<string, unknown>): Promise<CallToolResult> {
    if (this._state !== "connected") {
      throw new McpHostError(`Server ${this.config.name} is not connected`, ERROR_CODES.CONNECTION_FAILED, this.config.name);
    }
    const result = await this.client.callTool({ name, arguments: args });
    return result as CallToolResult;
  }

  async getPrompt(name: string, args?: Record<string, string>): Promise<GetPromptResult> {
    if (this._state !== "connected") {
      throw new McpHostError(`Server ${this.config.name} is not connected`, ERROR_CODES.CONNECTION_FAILED, this.config.name);
    }
    return await this.client.getPrompt({ name, arguments: args });
  }

  async readResource(uri: string): Promise<ReadResourceResult> {
    if (this._state !== "connected") {
      throw new McpHostError(`Server ${this.config.name} is not connected`, ERROR_CODES.CONNECTION_FAILED, this.config.name);
    }
    return await this.client.readResource({ uri });
  }

  async listTools(): Promise<Tool[]> {
    if (this._state !== "connected") {
      return [];
    }
    const result = await this.client.listTools();
    return result.tools;
  }

  async listPrompts(): Promise<Prompt[]> {
    if (this._state !== "connected") {
      return [];
    }
    const result = await this.client.listPrompts();
    return result.prompts || [];
  }

  async listResources(): Promise<Resource[]> {
    if (this._state !== "connected") {
      return [];
    }
    const result = await this.client.listResources();
    return result.resources || [];
  }

  on(handler: (event: ServerConnectionEvent) => void): void {
    const handlers = this.eventHandlers.get("*") || [];
    handlers.push(handler);
    this.eventHandlers.set("*", handlers);
  }

  private emit(event: ServerConnectionEvent): void {
    const handlers = this.eventHandlers.get("*") || [];
    handlers.forEach(h => h(event));
  }
}

// ============================================================================
// CapabilityAggregator Class
// ============================================================================

export class CapabilityAggregator {
  private eventHandlers: Map<string, ((event: CapabilityAggregatorEvent) => void)[]> = new Map();

  constructor(private connections: Map<string, ServerConnection>) {}

  async aggregateCapabilities(): Promise<AggregatedCapabilities> {
    const tools: NamespacedTool[] = [];
    const prompts: NamespacedPrompt[] = [];
    const resources: NamespacedResource[] = [];

    for (const [serverName, connection] of this.connections.entries()) {
      if (connection.state === "connected") {
        const serverTools = await connection.listTools();
        const serverPrompts = await connection.listPrompts();
        const serverResources = await connection.listResources();

        tools.push(...serverTools.map(tool => ({
          ...tool,
          serverName,
          namespacedName: createNamespacedName(serverName, tool.name),
        })));

        prompts.push(...serverPrompts.map(prompt => ({
          ...prompt,
          serverName,
          namespacedName: createNamespacedName(serverName, prompt.name),
        })));

        resources.push(...serverResources.map(resource => ({
          ...resource,
          serverName,
          namespacedName: createNamespacedName(serverName, resource.uri),
        })));
      }
    }

    return { tools, prompts, resources };
  }

  on(handler: (event: CapabilityAggregatorEvent) => void): void {
    const handlers = this.eventHandlers.get("*") || [];
    handlers.push(handler);
    this.eventHandlers.set("*", handlers);
  }

  private emit(event: CapabilityAggregatorEvent): void {
    const handlers = this.eventHandlers.get("*") || [];
    handlers.forEach(h => h(event));
  }
}

// ============================================================================
// SessionManager Class
// ============================================================================

export class SessionManager {
  private sessions: Map<string, { startTime: Date; servers: Set<string> }> = new Map();
  private eventHandlers: Map<string, ((event: SessionManagerEvent) => void)[]> = new Map();

  createSession(sessionId: string, servers: string[]): void {
    this.sessions.set(sessionId, {
      startTime: new Date(),
      servers: new Set(servers),
    });
    this.emit({ type: "session_started", sessionId });
  }

  endSession(sessionId: string): void {
    this.sessions.delete(sessionId);
    this.emit({ type: "session_ended", sessionId });
  }

  getSession(sessionId: string): { startTime: Date; servers: Set<string> } | undefined {
    return this.sessions.get(sessionId);
  }

  on(handler: (event: SessionManagerEvent) => void): void {
    const handlers = this.eventHandlers.get("*") || [];
    handlers.push(handler);
    this.eventHandlers.set("*", handlers);
  }

  private emit(event: SessionManagerEvent): void {
    const handlers = this.eventHandlers.get("*") || [];
    handlers.forEach(h => h(event));
  }
}

// ============================================================================
// McpHostManager Class
// ============================================================================

export class McpHostManager {
  private connections: Map<string, ServerConnection> = new Map();
  private capabilityAggregator: CapabilityAggregator;
  private sessionManager: SessionManager;
  private logger: (level: LogLevel, message: string) => void;

  constructor(
    private config: McpHostConfig,
    logger?: (level: LogLevel, message: string) => void
  ) {
    this.logger = logger || (() => {});
    this.capabilityAggregator = new CapabilityAggregator(this.connections);
    this.sessionManager = new SessionManager();

    for (const serverConfig of config.servers) {
      const connection = new ServerConnection(serverConfig, this.logger);
      this.connections.set(serverConfig.name, connection);
    }
  }

  async connectAll(): Promise<void> {
    const promises = Array.from(this.connections.values()).map(conn =>
      conn.connect().catch(err => {
        this.logger("error", `Failed to connect to ${conn.config.name}: ${sanitizeError(err)}`);
      })
    );
    await Promise.all(promises);
  }

  async disconnectAll(): Promise<void> {
    const promises = Array.from(this.connections.values()).map(conn => conn.disconnect());
    await Promise.all(promises);
  }

  getConnection(serverName: string): ServerConnection | undefined {
    return this.connections.get(serverName);
  }

  getAllServers(): ServerInfo[] {
    return Array.from(this.connections.entries()).map(([name, conn]) => ({
      name,
      state: conn.state,
      capabilities: conn.capabilities,
    }));
  }

  async getCapabilities(): Promise<AggregatedCapabilities> {
    return await this.capabilityAggregator.aggregateCapabilities();
  }

  getSessionManager(): SessionManager {
    return this.sessionManager;
  }

  getCapabilityAggregator(): CapabilityAggregator {
    return this.capabilityAggregator;
  }
}

// ============================================================================
// OpenAIAdapter Class
// ============================================================================

export class OpenAIAdapter {
  static toolToOpenAIFunction(tool: NamespacedTool): OpenAIFunction {
    return {
      name: tool.namespacedName,
      description: tool.description,
      parameters: {
        type: "object",
        properties: tool.inputSchema?.properties || {},
        required: tool.inputSchema?.required || [],
      },
    };
  }

  static aggregatedCapabilitiesToFunctions(capabilities: AggregatedCapabilities): OpenAIFunction[] {
    return capabilities.tools.map(tool => this.toolToOpenAIFunction(tool));
  }
}

// ============================================================================
// SamplingHandler Class
// ============================================================================

export class SamplingHandler {
  constructor(
    private connections: Map<string, ServerConnection>,
    private approvalCallback?: SamplingApprovalCallback,
    private timeoutMs: number = 60000
  ) {}

  /**
   * Convert MCP SamplingMessages to OpenAI chat format
   */
  private convertMcpMessagesToOpenAI(messages: SamplingMessage[]): Array<{ role: string; content: string }> {
    return messages.map(msg => {
      let content = "";

      if (msg.content.type === "text") {
        content = msg.content.text || "";
      } else if (msg.content.type === "image") {
        // For images, include metadata in text format
        content = `[Image: ${msg.content.mimeType || "unknown"}]`;
      } else {
        // For other content types (audio, resource, etc.), convert to text representation
        content = `[${msg.content.type}: ${JSON.stringify(msg.content)}]`;
      }

      return {
        role: msg.role,
        content,
      };
    });
  }

  /**
   * Convert OpenAI completion response to MCP CreateMessageResult format
   */
  private convertOpenAIResponseToMcp(response: any): CreateMessageResult {
    const choice = response.choices?.[0];
    const message = choice?.message;
    const content = message?.content || "";

    return {
      role: "assistant",
      content: {
        type: "text",
        text: content,
      },
      model: response.model || "unknown",
      stopReason: choice?.finish_reason || "unknown",
    };
  }

  async handleSamplingRequest(request: SamplingRequest): Promise<SamplingResult> {
    // Get approval and LLM client
    let llmClient: any;
    if (this.approvalCallback) {
      const approval = await this.approvalCallback(request);
      if (!approval.approved) {
        throw new McpHostError(
          `Sampling request denied: ${approval.reason || "No reason provided"}`,
          ERROR_CODES.SAMPLING_FAILED,
          request.serverName
        );
      }
      llmClient = approval.llmClient;
    }

    // Verify connection
    const connection = this.connections.get(request.serverName);
    if (!connection || connection.state !== "connected") {
      throw new McpHostError(
        `Server ${request.serverName} is not connected`,
        ERROR_CODES.CONNECTION_FAILED,
        request.serverName
      );
    }

    // Verify LLM client was provided
    if (!llmClient) {
      throw new McpHostError(
        "Sampling approved but no LLM client provided in approval",
        ERROR_CODES.SAMPLING_FAILED,
        request.serverName
      );
    }

    try {
      // Convert messages to OpenAI format
      const messages = this.convertMcpMessagesToOpenAI(request.messages);

      // Build LLM request parameters
      const llmParams: any = {
        messages,
        max_tokens: request.maxTokens || 2000,
      };

      // Add optional parameters
      if (request.systemPrompt) {
        // OpenAI uses a system message instead of systemPrompt parameter
        messages.unshift({
          role: "system",
          content: request.systemPrompt,
        });
      }

      // Add model hints if provided
      if (request.modelPreferences?.hints && request.modelPreferences.hints.length > 0) {
        const firstHint = request.modelPreferences.hints[0];
        const modelName = typeof firstHint === "string" ? firstHint : firstHint.name;
        if (modelName) {
          llmParams.model = modelName;
        }
      }

      // Set default model if not specified
      if (!llmParams.model) {
        llmParams.model = "gpt-4";
      }

      // Create AbortController for proper cancellation
      const abortController = new AbortController();
      const timeoutId = setTimeout(() => {
        abortController.abort();
      }, this.timeoutMs);

      try {
        // Make LLM call with abort signal
        const response = await llmClient.chat.completions.create({
          ...llmParams,
          signal: abortController.signal,
        });

        clearTimeout(timeoutId);
        return {
          serverName: request.serverName,
          result: this.convertOpenAIResponseToMcp(response),
        };
      } catch (error) {
        clearTimeout(timeoutId);

        // Check if it was aborted due to timeout
        if (error instanceof Error && error.name === 'AbortError') {
          throw new McpHostError(
            `Sampling request timed out after ${this.timeoutMs}ms`,
            ERROR_CODES.SAMPLING_FAILED,
            request.serverName
          );
        }
        throw error;
      }
    } catch (error) {
      mcpHostLogger.error("Sampling request failed:", error);

      // If it's already an McpHostError, rethrow it
      if (error instanceof McpHostError) {
        throw error;
      }

      // Otherwise wrap it
      throw new McpHostError(
        `Sampling request failed: ${error instanceof Error ? error.message : String(error)}`,
        ERROR_CODES.SAMPLING_FAILED,
        request.serverName
      );
    }
  }
}
