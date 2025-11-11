/**
 * Generic MCP sampling client utilities
 */

import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { SamplingRequestOptions, SamplingResult } from "./types.js";

/**
 * Check if MCP server reference is available
 */
export function isServerAvailable(serverRef: { current: McpServer | null }): boolean {
  return serverRef.current !== null;
}

/**
 * Check if sampling capability is supported by the client
 */
export function isSamplingSupported(server: McpServer): boolean {
  try {
    // Check if the server has the createMessage method
    return typeof (server as any).createMessage === "function";
  } catch {
    return false;
  }
}

/**
 * Create a sampling request to the MCP client
 */
export async function createSamplingRequest(
  server: McpServer,
  options: SamplingRequestOptions
): Promise<SamplingResult> {
  try {
    // Build the request parameters
    const requestParams: any = {
      messages: options.messages,
      maxTokens: options.maxTokens,
    };

    // Add optional parameters
    if (options.systemPrompt) {
      requestParams.systemPrompt = options.systemPrompt;
    }

    if (options.includeContext) {
      requestParams.includeContext = options.includeContext;
    }

    if (options.modelPreferences) {
      requestParams.modelPreferences = options.modelPreferences;
    }

    if (options.temperature !== undefined) {
      requestParams.temperature = options.temperature;
    }

    if (options.metadata) {
      requestParams.metadata = options.metadata;
    }

    // Make the sampling request
    const response = await (server as any).createMessage(requestParams);

    // Extract text from response
    let text = "";
    if (response.content) {
      if (response.content.type === "text") {
        text = response.content.text || "";
      } else if (Array.isArray(response.content)) {
        // Handle array of content blocks
        text = response.content
          .filter((block: any) => block.type === "text")
          .map((block: any) => block.text || "")
          .join("\n");
      }
    }

    return {
      success: true,
      text,
      model: response.model,
      stopReason: response.stopReason,
    };
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Create a simple text-based sampling request
 */
export async function requestTextCompletion(
  server: McpServer,
  prompt: string,
  options: {
    systemPrompt?: string;
    maxTokens?: number;
    temperature?: number;
    includeContext?: "none" | "thisServer" | "allServers";
    modelPreferences?: SamplingRequestOptions["modelPreferences"];
  } = {}
): Promise<SamplingResult> {
  return createSamplingRequest(server, {
    messages: [
      {
        role: "user",
        content: {
          type: "text",
          text: prompt,
        },
      },
    ],
    systemPrompt: options.systemPrompt,
    maxTokens: options.maxTokens || 2000,
    temperature: options.temperature,
    includeContext: options.includeContext || "thisServer",
    modelPreferences: options.modelPreferences || {
      intelligencePriority: 0.8,
      speedPriority: 0.5,
      costPriority: 0.5,
    },
  });
}
