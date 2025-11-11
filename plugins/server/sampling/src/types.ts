/**
 * Types for MCP sampling functionality
 */

import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";

/**
 * Reference to the MCP server instance for sampling
 */
export interface ServerReference {
  current: McpServer | null;
}

/**
 * Options for creating a sampling request
 */
export interface SamplingRequestOptions {
  /** User messages to send */
  messages: Array<{
    role: "user" | "assistant";
    content: {
      type: "text" | "image" | "resource";
      text?: string;
      data?: string;
      mimeType?: string;
    };
  }>;

  /** System prompt to guide the LLM */
  systemPrompt?: string;

  /** Maximum tokens in response */
  maxTokens: number;

  /** Include server context (tools, resources, prompts) */
  includeContext?: "none" | "thisServer" | "allServers";

  /** Model preferences (hints, not requirements) */
  modelPreferences?: {
    hints?: string[];
    intelligencePriority?: number;  // 0-1
    speedPriority?: number;         // 0-1
    costPriority?: number;          // 0-1
  };

  /** Temperature for sampling (0-1) */
  temperature?: number;

  /** Metadata for the request */
  metadata?: Record<string, unknown>;
}

/**
 * Result from a sampling request
 */
export interface SamplingResult {
  /** Whether the request was successful */
  success: boolean;

  /** Response text from the LLM */
  text?: string;

  /** Model that was used */
  model?: string;

  /** Stop reason */
  stopReason?: string;

  /** Error message if failed */
  error?: string;
}

/**
 * Base structure for error explanation requests
 */
export interface ErrorExplanationRequest {
  /** Error message or structured error object */
  error: unknown;

  /** Context that caused the error (query, code, etc.) */
  context?: string;

  /** Domain-specific knowledge to include */
  domainKnowledge?: string;

  /** Additional metadata */
  metadata?: Record<string, unknown>;
}

/**
 * Structured error explanation response
 */
export interface ErrorExplanation {
  /** What went wrong */
  explanation: string;

  /** Root cause of the error */
  cause: string;

  /** Concrete suggestions to fix the error */
  suggestions: string[];

  /** Optional code examples */
  examples?: string[];

  /** Tool usage guidance */
  toolGuidance?: string;
}
