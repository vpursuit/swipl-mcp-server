import { describe, test, expect, beforeEach, afterEach } from "vitest";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { toolHandlers, getCapabilitiesSummary } from "../../src/tools.js";
import { prologPrompts } from "../../src/prompts.js";
import { zodSchemas } from "../../src/schemas.js";
import { z } from "zod";

// Mock the server setup to test prompt registration
function createTestServer() {
  const server = new McpServer({
    name: "swipl-mcp-server-test",
    version: "1.0.0-test",
  });

  // Create actual Zod schemas for testing
  const testSchemas = {
    prologInitExpert: z.object({}),
    prologQuickReference: z.object({}),
    prologAnalyzeKnowledgeBase: z.object({}),
    prologExpertReasoning: z.object({
      task: z.string().optional()
    }),
    prologKnowledgeBaseBuilder: z.object({
      domain: z.string().optional()
    }),
    prologQueryOptimizer: z.object({
      query: z.string().optional()
    })
  };

  // Register prompts like in the main index.ts
  const promptSchemaMap: Record<string, string> = {
    "prolog_init_expert": "prologInitExpert",
    "prolog_quick_reference": "prologQuickReference",
    "prolog_analyze_knowledge_base": "prologAnalyzeKnowledgeBase",
    // expert reasoning merged into init_expert
    "prolog_knowledge_base_builder": "prologKnowledgeBaseBuilder",
    "prolog_query_optimizer": "prologQueryOptimizer"
  };

  const registeredPrompts: string[] = [];

  for (const promptConfig of Object.values(prologPrompts)) {
    const schemaName = promptSchemaMap[promptConfig.name];
    if (schemaName) {
      server.registerPrompt(
        promptConfig.name,
        {
          title: promptConfig.title,
          description: promptConfig.description,
          argsSchema: (testSchemas as any)[schemaName]
        },
        async (args: Record<string, unknown>) => {
          const stringArgs = Object.fromEntries(
            Object.entries(args).map(([key, value]) => [key, String(value)])
          );
          const promptMessages = promptConfig.messages(stringArgs);
          return {
            messages: promptMessages.map((msg: any) => ({
              role: msg.role,
              content: msg.content
            }))
          };
        }
      );
      registeredPrompts.push(promptConfig.name);
    }
  }

  return { server, registeredPrompts, testSchemas };
}

describe("Prompts Integration", () => {
  describe("Server Registration", () => {
    test("should register all prompts without errors", () => {
      expect(() => {
        const { server, registeredPrompts } = createTestServer();
        expect(registeredPrompts).toHaveLength(6);
        expect(registeredPrompts).toContain("prolog_init_expert");
        expect(registeredPrompts).toContain("prolog_quick_reference");
        expect(registeredPrompts).toContain("prolog_analyze_knowledge_base");
        // merged into prolog_init_expert
        expect(registeredPrompts).toContain("prolog_knowledge_base_builder");
        expect(registeredPrompts).toContain("prolog_query_optimizer");
      }).not.toThrow();
    });

    test("should have valid schema mapping for all prompts", () => {
      const { testSchemas } = createTestServer();
      const expectedSchemas = [
        "prologInitExpert",
        "prologQuickReference",
        "prologAnalyzeKnowledgeBase",
        "prologExpertReasoning",
        "prologKnowledgeBaseBuilder",
        "prologQueryOptimizer"
      ];

      for (const schemaName of expectedSchemas) {
        expect(testSchemas).toHaveProperty(schemaName);
      }
    });
  });

  describe("Capabilities Integration", () => {
    test("capabilities should include prompts section", () => {
      const capabilities = getCapabilitiesSummary();

      expect(capabilities).toHaveProperty("prompts");
      const prompts = capabilities.prompts as any;

      expect(prompts).toHaveProperty("expert_guidance");
      expect(prompts).toHaveProperty("knowledge_base");
      expect(prompts).toHaveProperty("orientation");

      expect(prompts.expert_guidance).toContain("prolog_init_expert");
      // merged into prolog_init_expert
      expect(prompts.expert_guidance).toContain("prolog_query_optimizer");

      expect(prompts.knowledge_base).toContain("prolog_analyze_knowledge_base");
      expect(prompts.knowledge_base).toContain("prolog_knowledge_base_builder");

      expect(prompts.orientation).toContain("prolog_quick_reference");
    });

    test("capabilities should maintain other sections with prompts added", () => {
      const capabilities = getCapabilitiesSummary();

      // Original sections should still exist
      expect(capabilities).toHaveProperty("server");
      expect(capabilities).toHaveProperty("modes");
      expect(capabilities).toHaveProperty("tools");
      expect(capabilities).toHaveProperty("security");

      // New prompts section should be added
      expect(capabilities).toHaveProperty("prompts");
    });
  });

  describe("Prompt Handler Execution", () => {
    test("prompt handlers should execute without errors", async () => {
      const { server } = createTestServer();

      // Test each prompt handler
      for (const promptConfig of Object.values(prologPrompts)) {
        // Test with empty args (should work for all prompts now)
        expect(async () => {
          const messages = promptConfig.messages({});
          expect(Array.isArray(messages)).toBe(true);
          expect(messages.length).toBeGreaterThan(0);
        }).not.toThrow();

        // Test with undefined args
        expect(async () => {
          const messages = promptConfig.messages();
          expect(Array.isArray(messages)).toBe(true);
        }).not.toThrow();
      }
    });

    test("prompt messages should have correct format for MCP", async () => {
      for (const promptConfig of Object.values(prologPrompts)) {
        const messages = promptConfig.messages({});

        for (const message of messages) {
          // Verify MCP message format
          expect(message).toHaveProperty("role");
          expect(["user", "assistant"]).toContain(message.role);
          expect(message).toHaveProperty("content");
          expect(message.content).toHaveProperty("type", "text");
          expect(message.content).toHaveProperty("text");
          expect(typeof message.content.text).toBe("string");
        }
      }
    });
  });

  describe("Help Integration", () => {
    test("help tool should include prompts section", async () => {
      const helpResponse = await toolHandlers.help({});
      const helpText = helpResponse.content[0].text;

      expect(helpText).toContain("Expert Prompts");
      expect(helpText).toContain("prolog_init_expert");
      expect(helpText).toContain("USE THIS FIRST");
      expect(helpText).toContain("Prompt Usage Pattern");
    });

    test("help tool should support prompts topic", async () => {
      const helpResponse = await toolHandlers.help({ topic: "prompts" });
      const helpText = helpResponse.content[0].text;

      expect(helpText).toContain("Expert Prompts (Start Here!)");
      expect(helpText).toContain("prolog_init_expert");
      expect(helpText).toContain("prolog_quick_reference");
      expect(helpText).toContain("prolog_analyze_knowledge_base");
      // merged into prolog_init_expert
      expect(helpText).toContain("prolog_knowledge_base_builder");
      expect(helpText).toContain("prolog_query_optimizer");
    });
  });

  describe("Schema Validation", () => {
    test("all prompt schemas should be valid Zod schemas", () => {
      const { testSchemas } = createTestServer();
      const schemaNames = [
        "prologInitExpert",
        "prologQuickReference",
        "prologAnalyzeKnowledgeBase",
        "prologExpertReasoning",
        "prologKnowledgeBaseBuilder",
        "prologQueryOptimizer"
      ];

      for (const schemaName of schemaNames) {
        const schema = (testSchemas as any)[schemaName];
        expect(schema).toBeDefined();

        // Should be able to parse empty object (since all args are optional)
        expect(() => schema.parse({})).not.toThrow();
      }
    });

    test("prompts with arguments should validate correctly", () => {
      const { testSchemas } = createTestServer();
      const testCases = [
        { schema: "prologExpertReasoning", validArgs: { task: "test task" } },
        { schema: "prologKnowledgeBaseBuilder", validArgs: { domain: "test domain" } },
        { schema: "prologQueryOptimizer", validArgs: { query: "test query" } }
      ];

      for (const { schema, validArgs } of testCases) {
        const zodSchema = (testSchemas as any)[schema];

        // Should accept valid arguments
        expect(() => zodSchema.parse(validArgs)).not.toThrow();

        // Should accept empty object (optional args)
        expect(() => zodSchema.parse({})).not.toThrow();
      }
    });
  });

  describe("Resource-First Approach", () => {
    test("all prompts should mention resource discovery", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages({});
        const text = messages[0].content.text.toLowerCase();

        // Should mention reading resources
        expect(text).toMatch(/read.*resource|check.*resource|list.*resource|resource.*discovery/);
      }
    });

    test("prompts should reference specific resources", () => {
      const resourcePrompts = [
        "quickReference",
        "analyzeKnowledgeBase",
        "expertReasoning",
        "knowledgeBaseBuilder"
      ];

      for (const promptName of resourcePrompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages({});
        const text = messages[0].content.text;

        // Should reference specific resources
        expect(text).toMatch(/meta:\/\/|prolog:\/\/|knowledge-base-predicates|knowledge-base-dump|capabilities|help/);
      }
    });
  });
});
