/**
 * Generic MCP sampling client utilities
 *
 * Refactored to use SamplingProvider abstraction instead of direct McpServer access.
 * This follows proper MCP protocol where servers request sampling from clients via callbacks.
 */

import type { SamplingProvider } from "./provider.js";
import type { SamplingRequestOptions, SamplingResult } from "./types.js";
import { samplingLogger } from "./logger.js";

/**
 * Create a simple text-based sampling request using a SamplingProvider
 *
 * This is a convenience function that wraps a text prompt into a proper
 * sampling request format and delegates to the provider.
 *
 * @param provider - The sampling provider to use (callback-based or no-op)
 * @param prompt - The text prompt to send to the LLM
 * @param options - Additional sampling options
 * @returns Promise resolving to sampling result
 */
export async function requestTextCompletion(
  provider: SamplingProvider,
  prompt: string,
  options: {
    systemPrompt?: string;
    maxTokens?: number;
    temperature?: number;
    includeContext?: "none" | "thisServer" | "allServers";
    modelPreferences?: SamplingRequestOptions["modelPreferences"];
  } = {}
): Promise<SamplingResult> {
  samplingLogger.debug("Creating text completion request");

  if (!provider.isAvailable()) {
    samplingLogger.warn("Sampling provider not available");
    return {
      success: false,
      error: "Sampling provider is not available",
    };
  }

  const samplingOptions: SamplingRequestOptions = {
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
    // Use "none" for includeContext since domain-specific context (e.g., Prolog KB)
    // is already included in the prompt text. The LLM doesn't need to see the list
    // of MCP server tools/resources/prompts for error explanation tasks.
    includeContext: options.includeContext || "none",
    modelPreferences: options.modelPreferences || {
      intelligencePriority: 0.8,
      speedPriority: 0.5,
      costPriority: 0.5,
    },
  };

  const result = await provider.requestSampling(samplingOptions);

  if (result.success) {
    samplingLogger.debug("Sampling succeeded", {
      textLength: result.text?.length,
      model: result.model,
    });
  } else {
    samplingLogger.error("Sampling failed:", result.error);
  }

  return result;
}
