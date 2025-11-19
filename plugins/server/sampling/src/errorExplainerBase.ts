/**
 * Generic framework for error explanation using MCP sampling
 *
 * Refactored to use SamplingProvider abstraction for proper protocol compliance.
 */

import type { SamplingProvider } from "./provider.js";
import type { ErrorExplanation, ErrorExplanationRequest, ErrorExplanationResult } from "./types.js";
import { requestTextCompletion } from "./samplingClient.js";
import { toSamplingError } from "./errors.js";
import { samplingLogger } from "./logger.js";

/**
 * Abstract base class for domain-specific error explainers
 * Subclasses should override the domain-specific methods
 */
export abstract class ErrorExplainerBase {
  /**
   * Get domain-specific system prompt
   */
  protected abstract getSystemPrompt(): string;

  /**
   * Format the error into a structured description
   */
  protected abstract formatError(error: unknown): string;

  /**
   * Get domain-specific knowledge for this error type
   */
  protected abstract getDomainKnowledge(error: unknown): string;

  /**
   * Parse the LLM response into structured explanation
   */
  protected parseExplanation(text: string): ErrorExplanation {
    // Default parsing implementation
    const explanation = this.extractField(text, "EXPLANATION") || text;
    const cause = this.extractField(text, "CAUSE") || "";
    const suggestionText = this.extractField(text, "SUGGESTION") || this.extractField(text, "SUGGESTIONS") || "";
    const toolGuidance = this.extractField(text, "TOOL_GUIDANCE") || this.extractField(text, "TOOL USAGE") || "";

    // Parse suggestions (look for numbered lists or bullet points)
    const suggestions: string[] = [];
    const lines = suggestionText.split("\n");
    for (const line of lines) {
      const trimmed = line.trim();
      if (trimmed && (
        /^\d+\./.test(trimmed) ||  // Numbered: "1. "
        /^[-*•]/.test(trimmed)      // Bullet: "- " or "* " or "• "
      )) {
        suggestions.push(trimmed.replace(/^(\d+\.|\s*[-*•]\s*)/, "").trim());
      }
    }

    // If no structured suggestions found, use the whole text
    if (suggestions.length === 0 && suggestionText) {
      suggestions.push(suggestionText);
    }

    // Extract code examples if present
    const examples: string[] = [];
    const codeBlockRegex = /```(?:\w+)?\n([\s\S]*?)```/g;
    let match;
    while ((match = codeBlockRegex.exec(text)) !== null) {
      examples.push(match[1].trim());
    }

    return {
      explanation,
      cause,
      suggestions,
      examples: examples.length > 0 ? examples : undefined,
      toolGuidance: toolGuidance || undefined,
    };
  }

  /**
   * Extract a specific field from the response text
   */
  protected extractField(text: string, fieldName: string): string | null {
    // Try exact match first: "FIELD: content"
    const exactRegex = new RegExp(`${fieldName}:\\s*(.+?)(?=\\n[A-Z_\\s]+:|$)`, "is");
    const exactMatch = text.match(exactRegex);
    if (exactMatch) {
      return exactMatch[1].trim();
    }

    // Try section header match: "## Field" or "**Field:**"
    const sectionRegex = new RegExp(`(?:^|\\n)(?:##\\s*|\\*\\*)?${fieldName}(?::|\\*\\*)?\\s*\\n([\\s\\S]+?)(?=\\n(?:##|\\*\\*|[A-Z_\\s]+:)|$)`, "i");
    const sectionMatch = text.match(sectionRegex);
    if (sectionMatch) {
      return sectionMatch[1].trim();
    }

    return null;
  }

  /**
   * Build the complete prompt for error explanation
   */
  protected buildPrompt(request: ErrorExplanationRequest): string {
    const errorDescription = this.formatError(request.error);
    const domainKnowledge = this.getDomainKnowledge(request.error);

    let prompt = `Analyze this error and provide a detailed explanation.\n\n`;
    prompt += `ERROR:\n${errorDescription}\n\n`;

    if (request.context) {
      prompt += `CONTEXT:\n${request.context}\n\n`;
    }

    if (domainKnowledge) {
      prompt += `DOMAIN KNOWLEDGE:\n${domainKnowledge}\n\n`;
    }

    if (request.domainKnowledge) {
      prompt += `ADDITIONAL CONTEXT:\n${request.domainKnowledge}\n\n`;
    }

    prompt += `Provide your analysis in this format:\n\n`;
    prompt += `EXPLANATION: [What went wrong - be specific to this error type]\n\n`;
    prompt += `CAUSE: [Why it happened - explain the underlying semantics]\n\n`;
    prompt += `SUGGESTIONS:\n`;
    prompt += `1. [First actionable suggestion with concrete examples]\n`;
    prompt += `2. [Second actionable suggestion if applicable]\n\n`;
    prompt += `TOOL_GUIDANCE: [If applicable, explain correct tool usage or tool call order]\n\n`;
    prompt += `Be concise, accurate, and actionable. Include code examples where helpful.`;

    return prompt;
  }

  /**
   * Explain an error using MCP sampling via a SamplingProvider
   *
   * This method now returns a structured result with detailed error information
   * instead of null, enabling better error handling and debugging.
   *
   * @param provider - The sampling provider (callback-based or no-op)
   * @param request - Error explanation request with context
   * @returns Promise resolving to structured result (success or error with details)
   */
  async explainError(
    provider: SamplingProvider,
    request: ErrorExplanationRequest
  ): Promise<ErrorExplanationResult> {
    samplingLogger.debug("Starting error explanation");

    // Check if sampling is available
    if (!provider.isAvailable()) {
      samplingLogger.warn("Sampling provider not available for error explanation");
      return {
        success: false,
        error: {
          code: "SAMPLING_NOT_AVAILABLE",
          message: "Sampling provider is not available - no client callback registered",
        },
        samplingUsed: false,
      };
    }

    try {
      // Build the prompt
      const prompt = this.buildPrompt(request);
      samplingLogger.debug("Built prompt, requesting LLM explanation");

      // Request explanation via sampling
      const result = await requestTextCompletion(provider, prompt, {
        systemPrompt: this.getSystemPrompt(),
        maxTokens: 2000,
        temperature: 0.2,  // Low temperature for accurate analysis
        // Use "none" - domain context is in prompt, no need for MCP server capabilities
        includeContext: "none",
        modelPreferences: {
          intelligencePriority: 0.9,  // High intelligence for domain expertise
          speedPriority: 0.5,
          costPriority: 0.3,
        },
      });

      if (!result.success) {
        samplingLogger.error("LLM sampling failed:", result.error);
        return {
          success: false,
          error: {
            code: "SAMPLING_FAILED",
            message: result.error || "Sampling request failed with unknown error",
          },
          samplingUsed: true,
        };
      }

      if (!result.text) {
        samplingLogger.warn("LLM returned empty response");
        return {
          success: false,
          error: {
            code: "EMPTY_RESPONSE",
            message: "Sampling succeeded but returned empty response",
          },
          samplingUsed: true,
        };
      }

      // Parse the response
      samplingLogger.debug("Parsing LLM explanation");
      const explanation = this.parseExplanation(result.text);

      return {
        success: true,
        explanation,
        samplingUsed: true,
      };
    } catch (error) {
      samplingLogger.error("Error during explanation:", error);
      const samplingError = toSamplingError(error);
      return {
        success: false,
        error: {
          code: samplingError.code,
          message: samplingError.message,
          details: samplingError.details,
        },
        samplingUsed: true,
      };
    }
  }
}
