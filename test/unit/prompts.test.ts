import { describe, test, expect } from "vitest";
import { prologPrompts } from "../../src/prompts.js";

describe("Prolog Prompts", () => {
  describe("Prompt Structure Validation", () => {
    test("all prompts should have required properties", () => {
      const expectedPrompts = [
        "initExpert",
        "quickReference",
        "analyzeKB",
        "expertReasoning",
        "kbBuilder",
        "queryOptimizer"
      ];

      const expectedNames = [
        "prolog_init_expert",
        "prolog_quick_reference",
        "prolog_analyze_kb",
        "prolog_expert_reasoning",
        "prolog_kb_builder",
        "prolog_query_optimizer"
      ];

      expect(Object.keys(prologPrompts)).toHaveLength(expectedPrompts.length);

      for (let i = 0; i < expectedPrompts.length; i++) {
        const promptKey = expectedPrompts[i];
        const expectedName = expectedNames[i];
        expect(prologPrompts).toHaveProperty(promptKey);

        const prompt = prologPrompts[promptKey];
        expect(prompt).toHaveProperty("name", expectedName);
        expect(prompt).toHaveProperty("title");
        expect(prompt).toHaveProperty("description");
        expect(prompt).toHaveProperty("arguments");
        expect(prompt).toHaveProperty("messages");

        expect(typeof prompt.title).toBe("string");
        expect(typeof prompt.description).toBe("string");
        expect(Array.isArray(prompt.arguments)).toBe(true);
        expect(typeof prompt.messages).toBe("function");
      }
    });

    test("prompts without arguments should have empty arguments array", () => {
      const noArgPrompts = ["initExpert", "quickReference", "analyzeKB"];

      for (const promptName of noArgPrompts) {
        const prompt = prologPrompts[promptName];
        expect(prompt.arguments).toHaveLength(0);
      }
    });

    test("prompts with arguments should have proper argument structure", () => {
      const argPrompts = [
        { name: "expertReasoning", argName: "task" },
        { name: "kbBuilder", argName: "domain" },
        { name: "queryOptimizer", argName: "query" }
      ];

      for (const { name, argName } of argPrompts) {
        const prompt = prologPrompts[name];
        expect(prompt.arguments).toHaveLength(1);

        const arg = prompt.arguments[0];
        expect(arg).toHaveProperty("name", argName);
        expect(arg).toHaveProperty("description");
        expect(arg).toHaveProperty("required", true);
        expect(typeof arg.description).toBe("string");
      }
    });
  });

  describe("Message Generation", () => {
    test("init expert prompt should generate proper message structure", () => {
      const prompt = prologPrompts.initExpert;
      const messages = prompt.messages();

      expect(Array.isArray(messages)).toBe(true);
      expect(messages).toHaveLength(1);

      const message = messages[0];
      expect(message).toHaveProperty("role", "user");
      expect(message).toHaveProperty("content");
      expect(message.content).toHaveProperty("type", "text");
      expect(message.content).toHaveProperty("text");
      expect(typeof message.content.text).toBe("string");

      // Check for key content elements
      const text = message.content.text;
      expect(text).toContain("Prolog");
      expect(text).toContain("expert");
      expect(text).toContain("resources");
      expect(text).toContain("capabilities");
      expect(text).toContain("security");
    });

    test("quick reference prompt should guide resource discovery", () => {
      const prompt = prologPrompts.quickReference;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("resources");
      expect(text).toContain("meta://help");
      expect(text).toContain("meta://capabilities");
      expect(text).toContain("prolog://kb/predicates");
      expect(text).toContain("prolog://kb/dump");
    });

    test("analyze kb prompt should emphasize resource reading", () => {
      const prompt = prologPrompts.analyzeKB;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("kb-predicates");
      expect(text).toContain("kb-dump");
      expect(text).toContain("capabilities");
      expect(text).toContain("STEP 1");
      expect(text).toContain("Resource Discovery");
    });

    test("expert reasoning prompt should handle missing task argument", () => {
      const prompt = prologPrompts.expertReasoning;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("[Please specify a reasoning task]");
      expect(text).toContain("DISCOVERY PHASE");
      expect(text).toContain("capabilities");
      expect(text).toContain("security");
    });

    test("expert reasoning prompt should use provided task argument", () => {
      const prompt = prologPrompts.expertReasoning;
      const messages = prompt.messages({ task: "solve family tree relationships" });

      const text = messages[0].content.text;
      expect(text).toContain("solve family tree relationships");
      expect(text).not.toContain("[Please specify a reasoning task]");
    });

    test("kb builder prompt should handle missing domain argument", () => {
      const prompt = prologPrompts.kbBuilder;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("[Please specify a domain to model]");
      expect(text).toContain("PREPARATION PHASE");
    });

    test("kb builder prompt should use provided domain argument", () => {
      const prompt = prologPrompts.kbBuilder;
      const messages = prompt.messages({ domain: "medical diagnosis" });

      const text = messages[0].content.text;
      expect(text).toContain("medical diagnosis");
      expect(text).not.toContain("[Please specify a domain to model]");
    });

    test("query optimizer prompt should handle missing query argument", () => {
      const prompt = prologPrompts.queryOptimizer;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("[Please provide a Prolog query to optimize]");
      expect(text).toContain("ANALYSIS PHASE");
    });

    test("query optimizer prompt should use provided query argument", () => {
      const prompt = prologPrompts.queryOptimizer;
      const messages = prompt.messages({ query: "findall(X, member(X, [1,2,3]), L)" });

      const text = messages[0].content.text;
      expect(text).toContain("findall(X, member(X, [1,2,3]), L)");
      expect(text).not.toContain("[Please provide a Prolog query to optimize]");
    });
  });

  describe("Content Validation", () => {
    test("all prompts should mention resources for context discovery", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages();
        const text = messages[0].content.text;

        // Each prompt should guide users to check resources
        expect(text.toLowerCase()).toMatch(/resource|capabilities|help|kb-/);
      }
    });

    test("prompts should include security awareness", () => {
      const securityAwarePrompts = [
        "initExpert",
        "expertReasoning",
        "kbBuilder"
      ];

      for (const promptName of securityAwarePrompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages();
        const text = messages[0].content.text.toLowerCase();

        expect(text).toMatch(/security|safe|dangerous|blocked/);
      }
    });

    test("prompts should emphasize Prolog expertise", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages();
        const text = messages[0].content.text.toLowerCase();

        expect(text).toMatch(/prolog|logic programming|expert/);
      }
    });

    test("workflow prompts should have structured phases", () => {
      const workflowPrompts = [
        "analyzeKB",
        "expertReasoning",
        "kbBuilder",
        "queryOptimizer"
      ];

      for (const promptName of workflowPrompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages();
        const text = messages[0].content.text;

        // Should have structured phases/steps
        expect(text).toMatch(/PHASE|STEP \d|1\.|2\./);
      }
    });
  });

  describe("Argument Handling", () => {
    test("prompts should handle undefined arguments gracefully", () => {
      const promptsWithArgs = [
        "expertReasoning",
        "kbBuilder",
        "queryOptimizer"
      ];

      for (const promptName of promptsWithArgs) {
        const prompt = prologPrompts[promptName];

        // Should not throw when called without arguments
        expect(() => prompt.messages()).not.toThrow();
        expect(() => prompt.messages({})).not.toThrow();
        expect(() => prompt.messages(undefined)).not.toThrow();
      }
    });

    test("prompts should handle extra arguments gracefully", () => {
      const prompt = prologPrompts.expertReasoning;

      expect(() => prompt.messages({
        task: "test task",
        extraArg: "should be ignored",
        anotherExtra: 123
      })).not.toThrow();

      const messages = prompt.messages({
        task: "test task",
        extraArg: "should be ignored"
      });

      expect(messages[0].content.text).toContain("test task");
    });
  });

  describe("Message Role Validation", () => {
    test("all prompts should only use valid MCP message roles", () => {
      const validRoles = ["user", "assistant"];
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages();

        for (const message of messages) {
          expect(validRoles).toContain(message.role);
        }
      }
    });

    test("all prompts should have consistent message structure", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages();

        for (const message of messages) {
          expect(message).toHaveProperty("role");
          expect(message).toHaveProperty("content");
          expect(message.content).toHaveProperty("type", "text");
          expect(message.content).toHaveProperty("text");
          expect(typeof message.content.text).toBe("string");
          expect(message.content.text.length).toBeGreaterThan(0);
        }
      }
    });
  });

  describe("Prompt Categories", () => {
    test("should have proper categorization of prompts", () => {
      const expertGuidance = ["initExpert", "expertReasoning", "queryOptimizer"];
      const knowledgeBase = ["analyzeKB", "kbBuilder"];
      const orientation = ["quickReference"];

      // Verify all categories are covered
      const allCategorized = [...expertGuidance, ...knowledgeBase, ...orientation];
      const allPromptNames = Object.keys(prologPrompts);

      expect(allCategorized.sort()).toEqual(allPromptNames.sort());
    });
  });
});