/**
 * Comprehensive test suite for SamplingHandler
 *
 * Tests cover:
 * - Approval/denial flow
 * - Successful sampling with LLM client
 * - Timeout handling
 * - Error scenarios
 * - Message format conversions
 */

import { describe, test, expect, beforeEach, vi } from "vitest";
import {
  SamplingHandler,
  McpHostError,
  ERROR_CODES,
  type SamplingRequest,
  type SamplingApproval,
  type ServerConnection,
} from "../src/index.js";

// Mock LLM client with OpenAI-compatible interface
function createMockLLMClient(options: {
  response?: string;
  delay?: number;
  shouldFail?: boolean;
  error?: Error;
} = {}) {
  const {
    response = "This is a test response",
    delay = 0,
    shouldFail = false,
    error = new Error("LLM call failed"),
  } = options;

  return {
    chat: {
      completions: {
        create: vi.fn(async (params: any) => {
          // Respect AbortController signal for timeout testing
          if (params?.signal) {
            // Check if already aborted
            if (params.signal.aborted) {
              const abortError = new Error('Request aborted');
              abortError.name = 'AbortError';
              throw abortError;
            }

            // Listen for abort during delay
            const abortPromise = new Promise((_, reject) => {
              params.signal.addEventListener('abort', () => {
                const abortError = new Error('Request aborted');
                abortError.name = 'AbortError';
                reject(abortError);
              });
            });

            if (delay > 0) {
              // Race between delay and abort
              await Promise.race([
                new Promise(resolve => setTimeout(resolve, delay)),
                abortPromise
              ]);
            }
          } else if (delay > 0) {
            await new Promise(resolve => setTimeout(resolve, delay));
          }

          if (shouldFail) {
            throw error;
          }
          return {
            id: "test-completion-id",
            object: "chat.completion",
            created: Date.now(),
            model: "gpt-4",
            choices: [
              {
                index: 0,
                message: {
                  role: "assistant",
                  content: response,
                },
                finish_reason: "stop",
              },
            ],
          };
        }),
      },
    },
  };
}

// Create mock connections map
function createMockConnections(): Map<string, ServerConnection> {
  const connections = new Map<string, ServerConnection>();
  connections.set("test-server", {
    name: "test-server",
    config: {
      command: "test",
      args: [],
    },
    state: "connected",
    client: {} as any,
    capabilities: {
      tools: [],
      prompts: [],
      resources: [],
      experimental: {},
    },
    transport: {} as any,
    stateHistory: [],
    healthCheck: {
      isHealthy: true,
      consecutiveFailures: 0,
    },
  });
  return connections;
}

// Create sample sampling request
function createSamplingRequest(overrides?: Partial<SamplingRequest>): SamplingRequest {
  return {
    serverName: "test-server",
    messages: [
      {
        role: "user",
        content: {
          type: "text",
          text: "Hello, this is a test message",
        },
      },
    ],
    maxTokens: 100,
    systemPrompt: "You are a helpful assistant",
    ...overrides,
  };
}

describe("SamplingHandler", () => {
  describe("Approval Flow", () => {
    test("should deny sampling when approval callback returns denied", async () => {
      const connections = createMockConnections();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: false,
        reason: "User denied the request",
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest();

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        McpHostError
      );
      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "Sampling request denied: User denied the request"
      );

      expect(approvalCallback).toHaveBeenCalledWith(request);
    });

    test("should throw error when no LLM client provided after approval", async () => {
      const connections = createMockConnections();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        // No llmClient provided
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest();

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "Sampling approved but no LLM client provided in approval"
      );
    });

    test("should throw error when server is not connected", async () => {
      const connections = new Map<string, ServerConnection>();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest({ serverName: "non-existent-server" });

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        McpHostError
      );
      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "is not connected"
      );
    });
  });

  describe("Successful Sampling", () => {
    test("should successfully handle sampling request with text messages", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient({
        response: "This is the AI response",
      });
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest();

      const result = await handler.handleSamplingRequest(request);

      expect(result).toMatchObject({
        serverName: "test-server",
        result: {
          role: "assistant",
          content: {
            type: "text",
            text: "This is the AI response",
          },
          model: "gpt-4",
          stopReason: "stop",
        },
      });

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          messages: expect.arrayContaining([
            { role: "system", content: "You are a helpful assistant" },
            { role: "user", content: "Hello, this is a test message" },
          ]),
          max_tokens: 100,
        })
      );
    });

    test("should handle image content types", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest({
        messages: [
          {
            role: "user",
            content: {
              type: "image",
              data: "base64data",
              mimeType: "image/png",
            },
          },
        ],
      });

      await handler.handleSamplingRequest(request);

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          messages: expect.arrayContaining([
            { role: "user", content: "[Image: image/png]" },
          ]),
        })
      );
    });

    test("should use model hints from modelPreferences", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest({
        modelPreferences: {
          hints: [{ name: "gpt-3.5-turbo" }],
          costPriority: 0.8,
          speedPriority: 0.6,
          intelligencePriority: 0.4,
        },
      });

      await handler.handleSamplingRequest(request);

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          model: "gpt-3.5-turbo",
        })
      );
    });

    test("should use default model when no hints provided", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest({
        systemPrompt: undefined,
        modelPreferences: undefined,
      });

      await handler.handleSamplingRequest(request);

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          model: "gpt-4",
        })
      );
    });
  });

  describe("Timeout Handling", () => {
    test("should timeout when LLM call exceeds timeout limit", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient({
        delay: 100, // 100ms delay
      });
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback, 50); // 50ms timeout
      const request = createSamplingRequest();

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "Sampling request timed out after 50ms"
      );
    });

    test("should complete successfully when LLM call is within timeout", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient({
        delay: 10, // 10ms delay
        response: "Quick response",
      });
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback, 100); // 100ms timeout
      const request = createSamplingRequest();

      const result = await handler.handleSamplingRequest(request);

      expect(result.result.content).toMatchObject({
        type: "text",
        text: "Quick response",
      });
    });
  });

  describe("Error Handling", () => {
    test("should handle LLM call failures gracefully", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient({
        shouldFail: true,
        error: new Error("Rate limit exceeded"),
      });
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest();

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        McpHostError
      );
      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "Sampling request failed: Rate limit exceeded"
      );
    });

    test("should rethrow McpHostError without wrapping", async () => {
      const connections = createMockConnections();
      const customError = new McpHostError(
        "Custom error",
        ERROR_CODES.SAMPLING_FAILED,
        "test-server"
      );
      const llmClient = createMockLLMClient({
        shouldFail: true,
        error: customError,
      });
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest();

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "Custom error"
      );
    });
  });

  describe("Message Format Conversion", () => {
    test("should handle multi-turn conversations", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest({
        messages: [
          {
            role: "user",
            content: { type: "text", text: "First message" },
          },
          {
            role: "assistant",
            content: { type: "text", text: "First response" },
          },
          {
            role: "user",
            content: { type: "text", text: "Second message" },
          },
        ],
      });

      await handler.handleSamplingRequest(request);

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          messages: expect.arrayContaining([
            { role: "system", content: "You are a helpful assistant" },
            { role: "user", content: "First message" },
            { role: "assistant", content: "First response" },
            { role: "user", content: "Second message" },
          ]),
        })
      );
    });

    test("should handle maxTokens parameter", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest({
        maxTokens: 500,
      });

      await handler.handleSamplingRequest(request);

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          max_tokens: 500,
        })
      );
    });

    test("should use default maxTokens when not provided", async () => {
      const connections = createMockConnections();
      const llmClient = createMockLLMClient();
      const approvalCallback = vi.fn(async (): Promise<SamplingApproval> => ({
        approved: true,
        llmClient,
      }));

      const handler = new SamplingHandler(connections, approvalCallback);
      const request = createSamplingRequest();
      delete (request as any).maxTokens;

      await handler.handleSamplingRequest(request);

      expect(llmClient.chat.completions.create).toHaveBeenCalledWith(
        expect.objectContaining({
          max_tokens: 2000,
        })
      );
    });
  });

  describe("Without Approval Callback", () => {
    test("should throw error when no LLM client and no approval callback", async () => {
      const connections = createMockConnections();
      const handler = new SamplingHandler(connections); // No approval callback
      const request = createSamplingRequest();

      await expect(handler.handleSamplingRequest(request)).rejects.toThrow(
        "Sampling approved but no LLM client provided in approval"
      );
    });
  });
});
