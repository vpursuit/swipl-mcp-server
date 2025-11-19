/**
 * MCP Protocol-Based Sampling Provider
 *
 * This provider uses the MCP SDK's createMessage() method to send sampling
 * requests via the protocol to connected clients. This is the correct approach
 * for servers using stdio transport.
 */

import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type { SamplingProvider } from "./provider.js";
import type { SamplingRequestOptions, SamplingResult } from "./types.js";
import { samplingLogger } from "./logger.js";

/**
 * Sampling provider that uses MCP protocol's createMessage()
 *
 * This provider checks client capabilities and sends sampling requests
 * via the MCP SDK's built-in protocol methods. The client receives these
 * requests through its sampling handler and returns LLM results.
 */
export class McpProtocolSamplingProvider implements SamplingProvider {
  constructor(
    private mcpServer: McpServer,
    private timeoutMs: number = 60000
  ) {}

  /**
   * Check if the connected client supports sampling
   *
   * @returns true if client advertised sampling capability, false otherwise
   */
  isAvailable(): boolean {
    try {
      const capabilities = this.mcpServer.server.getClientCapabilities();
      return !!capabilities?.sampling;
    } catch (error) {
      samplingLogger.error("Error checking client sampling capabilities:", error);
      return false;
    }
  }

  /**
   * Request sampling from the client via MCP protocol
   *
   * Sends a sampling/createMessage request through the protocol. The client's
   * sampling handler receives this, performs the LLM call, and returns the result.
   *
   * @param options - Sampling request parameters
   * @returns Promise resolving to sampling result
   */
  async requestSampling(
    options: SamplingRequestOptions
  ): Promise<SamplingResult> {
    try {
      // Create timeout promise
      const timeoutPromise = new Promise<never>((_, reject) => {
        setTimeout(() => {
          reject(new Error(`Sampling request timed out after ${this.timeoutMs}ms`));
        }, this.timeoutMs);
      });

      // Map our internal types to SDK types
      const sdkMessages = options.messages.map(msg => ({
        role: msg.role,
        content: msg.content.type === "text"
          ? { type: "text" as const, text: msg.content.text || "" }
          : msg.content.type === "image"
          ? { type: "image" as const, data: msg.content.data || "", mimeType: msg.content.mimeType || "image/png" }
          : { type: "text" as const, text: "" }, // Fallback for unsupported types
      }));

      // Map model preferences hints from string[] to { name?: string }[]
      const sdkModelPreferences = options.modelPreferences ? {
        ...options.modelPreferences,
        hints: options.modelPreferences.hints?.map(hint =>
          typeof hint === "string" ? { name: hint } : hint
        ),
      } : undefined;

      // Send sampling request via MCP SDK
      const samplingPromise = this.mcpServer.server.createMessage({
        messages: sdkMessages,
        maxTokens: options.maxTokens,
        temperature: options.temperature,
        systemPrompt: options.systemPrompt,
        includeContext: options.includeContext,
        modelPreferences: sdkModelPreferences,
        metadata: options.metadata,
      });

      // Race between timeout and actual request
      const result = await Promise.race([samplingPromise, timeoutPromise]);

      // Extract text from content
      let text = "";
      if (result.content.type === "text") {
        text = result.content.text;
      } else {
        samplingLogger.warn("Non-text content received from LLM:", result.content.type);
      }

      return {
        success: true,
        text,
        model: result.model,
        stopReason: result.stopReason,
      };
    } catch (error) {
      samplingLogger.error("MCP sampling request failed:", error);
      return {
        success: false,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }
}
