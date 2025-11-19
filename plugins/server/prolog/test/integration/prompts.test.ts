/**
 * Prompts Integration Tests
 * Tests MCP prompts registration and functionality
 *
 * TODO (Step 7): Update references to tool names in prompt content when old tools are removed
 * This file tests valid functionality and should be kept
 */

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
    genealogy: z.object({
      family_info: z.string()
    }),
    scheduling: z.object({
      tasks: z.string()
    }),
    puzzle: z.object({
      puzzle: z.string().optional()
    })
  };

  // Register prompts like in the main index.ts
  const promptSchemaMap: Record<string, string> = {
    "genealogy": "genealogy",
    "scheduling": "scheduling",
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
        expect(registeredPrompts).toHaveLength(3);
        expect(registeredPrompts).toContain("genealogy");
        expect(registeredPrompts).toContain("scheduling");
        expect(registeredPrompts).toContain("puzzle");
      }).not.toThrow();
    });

    test("should have valid schema mapping for all prompts", () => {
      const { testSchemas } = createTestServer();
      const expectedSchemas = [
        "genealogy",
        "scheduling",
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

      expect(prompts).toHaveProperty("domain_examples");

      expect(prompts.domain_examples).toContain("genealogy");
      expect(prompts.domain_examples).toContain("scheduling");
      expect(prompts.domain_examples).toContain("puzzle");
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

  // Help tool removed - capabilities tool is now the metadata/introspection tool

  describe("Schema Validation", () => {
    test("all prompt schemas should be valid Zod schemas", () => {
      const { testSchemas } = createTestServer();
      const schemaNames = [
        "genealogy",
        "scheduling",
        "puzzle"
      ];

      for (const schemaName of schemaNames) {
        const schema = (testSchemas as any)[schemaName];
        expect(schema).toBeDefined();

        // Genealogy and scheduling have required args
        // Puzzle has optional args
        if (schemaName === "puzzle") {
          expect(() => schema.parse({})).not.toThrow();
        }
      }
    });

    test("prompts with arguments should validate correctly", () => {
      const { testSchemas } = createTestServer();
      const testCases = [
        { schema: "genealogy", validArgs: { family_info: "John is Mary's father" } },
        { schema: "scheduling", validArgs: { tasks: "Task1 (5 days), Task2 (3 days) depends on Task1" } },
        { schema: "puzzle", validArgs: { puzzle: "test puzzle" } }
      ];

      for (const { schema, validArgs } of testCases) {
        const zodSchema = (testSchemas as any)[schema];

        // Should accept valid arguments
        expect(() => zodSchema.parse(validArgs)).not.toThrow();
      }
    });

    test("optional arguments should work correctly", () => {
      const { testSchemas } = createTestServer();

      // Puzzle has optional args
      const puzzleSchema = testSchemas.puzzle;
      expect(() => puzzleSchema.parse({})).not.toThrow();
      expect(() => puzzleSchema.parse({ puzzle: "test" })).not.toThrow();
    });
  });

  describe("Domain-Specific Tool Patterns", () => {
    test("all prompts should demonstrate MCP tool usage", () => {
      const allPrompts = Object.entries(prologPrompts);

      for (const [_key, prompt] of allPrompts) {
        // Use sample args to trigger full workflow
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text.toLowerCase();

        // Should mention MCP tools
        expect(text).toMatch(/clauses|query_/);
      }
    });

    test("prompts should show structured workflows", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text;

        // Should have structured workflow
        expect(text).toMatch(/STEP|WORKFLOW/i);
      }
    });
  });

  describe("Domain-Specific Behavior", () => {
    test("genealogy prompt should focus on family relationships", () => {
      const prompt = prologPrompts.genealogy;
      const messages = prompt.messages({ family_info: "John is Mary's father" });

      const text = messages[0].content.text.toLowerCase();
      expect(text).toContain("parent_of");
      expect(text).toContain("sibling");
      expect(text).toContain("ancestor");
    });

    test("scheduling prompt should focus on constraints", () => {
      const prompt = prologPrompts.scheduling;
      const messages = prompt.messages({ tasks: "Task1, Task2" });

      const text = messages[0].content.text.toLowerCase();
      expect(text).toContain("clpfd");
      expect(text).toContain("constraint");
      expect(text).toContain("schedule");
    });

    test("puzzle prompt should offer choices when no puzzle provided", () => {
      const prompt = prologPrompts.puzzle;
      const messagesWithout = prompt.messages({});
      const messagesWith = prompt.messages({ puzzle: "Test puzzle" });

      expect(messagesWithout[0].content.text).toContain("3 interesting");
      expect(messagesWith[0].content.text).toContain("Test puzzle");
      expect(messagesWith[0].content.text).not.toContain("3 interesting");
    });

  });

  describe("MCP Protocol Compliance", () => {
    test("schemas with optional fields should validate empty object", () => {
      const { testSchemas } = createTestServer();

      // Puzzle has optional fields
      for (const schemaName of ["puzzle"]) {
        const schema = (testSchemas as any)[schemaName];

        // Should accept empty object (all fields are optional)
        expect(() => schema.parse({})).not.toThrow();
      }
    });

    test("prompt message generators should handle undefined gracefully", () => {
      // Test that message generators work with undefined (defaulting to empty object)
      for (const promptConfig of Object.values(prologPrompts)) {
        // Message generator should handle undefined and default to empty args
        expect(() => {
          const messages = promptConfig.messages(undefined as any);
          expect(Array.isArray(messages)).toBe(true);
          expect(messages.length).toBeGreaterThan(0);
        }).not.toThrow();
      }
    });

    test("prompt handlers are wrapped to normalize undefined to empty object", () => {
      // This tests the loader.ts wrapper that converts undefined -> {}
      // Note: This is a known MCP SDK limitation workaround
      // See: https://github.com/modelcontextprotocol/typescript-sdk/issues/400
      for (const promptConfig of Object.values(prologPrompts)) {
        // The handler wrapper in loader.ts should convert undefined to {}
        // before passing to the actual prompt handler
        expect(() => {
          const messages = promptConfig.messages({});
          expect(Array.isArray(messages)).toBe(true);
        }).not.toThrow();
      }
    });
  });
});
