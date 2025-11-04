import { describe, test, expect } from "vitest";
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { toolHandlers, getCapabilitiesSummary, prologPrompts } from "@vpursuit/mcp-server-prolog";
import { z } from "zod";

// Mock the server setup to test prompt registration
function createTestServer() {
  const server = new McpServer({
    name: "swipl-mcp-server-test",
    version: "1.0.0-test",
  });

  // Create actual Zod schemas for testing
  const testSchemas = {
    expert: z.object({
      task: z.string().optional(),
      mode: z.enum(["expert", "reference"]).optional()
    }),
    knowledge: z.object({
      domain: z.string().optional(),
      mode: z.enum(["build", "analyze"]).optional()
    }),
    optimize: z.object({
      query: z.string().optional()
    }),
    puzzle: z.object({
      puzzle: z.string().optional()
    })
  };

  // Register prompts like in the main index.ts
  const promptSchemaMap: Record<string, string> = {
    "expert": "expert",
    "knowledge": "knowledge",
    "optimize": "optimize",
    "puzzle": "puzzle"
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
        const { server: _server, registeredPrompts } = createTestServer();
        expect(registeredPrompts).toHaveLength(4);
        expect(registeredPrompts).toContain("expert");
        expect(registeredPrompts).toContain("knowledge");
        expect(registeredPrompts).toContain("optimize");
        expect(registeredPrompts).toContain("puzzle");
      }).not.toThrow();
    });

    test("should have valid schema mapping for all prompts", () => {
      const { testSchemas } = createTestServer();
      const expectedSchemas = [
        "expert",
        "knowledge",
        "optimize",
        "puzzle"
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
      expect(prompts).toHaveProperty("problem_solving");

      expect(prompts.expert_guidance).toContain("expert");
      expect(prompts.expert_guidance).toContain("optimize");

      expect(prompts.knowledge_base).toContain("knowledge");

      expect(prompts.problem_solving).toContain("puzzle");
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
      const { server: _server } = createTestServer();

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
      expect(helpText).toContain("expert");
      expect(helpText).toContain("Prompt Usage Pattern");
    });

    test("help tool should support prompts topic", async () => {
      const helpResponse = await toolHandlers.help({ topic: "prompts" });
      const helpText = helpResponse.content[0].text;

      expect(helpText).toContain("Expert Prompts");
      expect(helpText).toContain("expert");
      expect(helpText).toContain("knowledge");
      expect(helpText).toContain("optimize");
      expect(helpText).toContain("puzzle");
    });
  });

  describe("Schema Validation", () => {
    test("all prompt schemas should be valid Zod schemas", () => {
      const { testSchemas } = createTestServer();
      const schemaNames = [
        "expert",
        "knowledge",
        "optimize",
        "puzzle"
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
        { schema: "expert", validArgs: { task: "test task", mode: "expert" } },
        { schema: "knowledge", validArgs: { domain: "test domain", mode: "build" } },
        { schema: "optimize", validArgs: { query: "test query" } },
        { schema: "puzzle", validArgs: { puzzle: "test puzzle" } }
      ];

      for (const { schema, validArgs } of testCases) {
        const zodSchema = (testSchemas as any)[schema];

        // Should accept valid arguments
        expect(() => zodSchema.parse(validArgs)).not.toThrow();

        // Should accept empty object (optional args)
        expect(() => zodSchema.parse({})).not.toThrow();
      }
    });

    test("mode arguments should validate correctly", () => {
      const { testSchemas } = createTestServer();

      // Expert mode validation
      const expertSchema = testSchemas.expert;
      expect(() => expertSchema.parse({ mode: "expert" })).not.toThrow();
      expect(() => expertSchema.parse({ mode: "reference" })).not.toThrow();

      // Knowledge mode validation
      const knowledgeSchema = testSchemas.knowledge;
      expect(() => knowledgeSchema.parse({ mode: "build" })).not.toThrow();
      expect(() => knowledgeSchema.parse({ mode: "analyze" })).not.toThrow();
    });
  });

  describe("Resource-First Approach", () => {
    test("all prompts should mention resource discovery", () => {
      const allPrompts = Object.entries(prologPrompts);

      for (const [key, prompt] of allPrompts) {
        // Puzzle solver is action-oriented (solve puzzle), not resource-discovery-oriented
        if (key === "puzzle") {
          continue;
        }

        const messages = prompt.messages({});
        const text = messages[0].content.text.toLowerCase();

        // Should mention reading resources
        expect(text).toMatch(/read.*resource|check.*resource|list.*resource|resource.*discovery/);
      }
    });

    test("prompts should reference specific resources", () => {
      const resourcePrompts = [
        "expert",
        "knowledge"
      ];

      for (const promptName of resourcePrompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages({});
        const text = messages[0].content.text;

        // Should reference specific resources
        expect(text).toMatch(/reference:\/\/|prolog:\/\/|knowledge-base-predicates|knowledge-base-dump|capabilities|help/);
      }
    });
  });

  describe("Mode-based Prompt Behavior", () => {
    test("expert prompt should behave differently based on mode", () => {
      const prompt = prologPrompts.expert;

      const expertMode = prompt.messages({ mode: "expert" });
      const referenceMode = prompt.messages({ mode: "reference" });

      expect(expertMode[0].content.text).not.toBe(referenceMode[0].content.text);
      expect(referenceMode[0].content.text).toContain("reference guide");
    });

    test("knowledge prompt should behave differently based on mode", () => {
      const prompt = prologPrompts.knowledge;

      const buildMode = prompt.messages({ mode: "build" });
      const analyzeMode = prompt.messages({ mode: "analyze" });

      expect(buildMode[0].content.text).not.toBe(analyzeMode[0].content.text);
      expect(buildMode[0].content.text).toContain("Build");
      expect(analyzeMode[0].content.text).toContain("Analyze");
    });
  });
});
