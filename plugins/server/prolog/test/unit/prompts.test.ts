import { describe, test, expect } from "vitest";
import { prologPrompts } from "../../src/prompts.js";

describe("Prolog Prompts", () => {
  describe("Prompt Structure Validation", () => {
    test("all prompts should have required properties", () => {
      const expectedPrompts = [
        "genealogy",
        "scheduling",
        "puzzle",
        "grammar"
      ];

      const expectedNames = [
        "genealogy",
        "scheduling",
        "puzzle",
        "grammar"
      ];

      expect(Object.keys(prologPrompts)).toHaveLength(expectedPrompts.length);

      for (let i = 0; i < expectedPrompts.length; i++) {
        const promptKey = expectedPrompts[i];
        const expectedName = expectedNames[i];
        expect(prologPrompts).toHaveProperty(promptKey);

        const prompt = prologPrompts[promptKey];
        expect(prompt).toHaveProperty("name", expectedName);
        expect(prompt).toHaveProperty("description");
        expect(prompt).toHaveProperty("arguments");
        expect(prompt).toHaveProperty("messages");

        expect(typeof prompt.description).toBe("string");
        expect(Array.isArray(prompt.arguments)).toBe(true);
        expect(typeof prompt.messages).toBe("function");
      }
    });

    test("prompts with arguments should have proper argument structure", () => {
      const argPrompts = [
        { name: "genealogy", args: ["family_info"], required: [true] },
        { name: "scheduling", args: ["tasks"], required: [true] },
        { name: "puzzle", args: ["puzzle"], required: [true] },
        { name: "grammar", args: ["sentence"], required: [false] }
      ] as const;

      for (const { name, args, required } of argPrompts) {
        const prompt = prologPrompts[name];
        expect(prompt.arguments).toHaveLength(args.length);

        for (let i = 0; i < args.length; i++) {
          const arg = prompt.arguments[i];
          expect(arg).toHaveProperty("name", args[i]);
          expect(arg).toHaveProperty("description");
          expect(arg).toHaveProperty("required", required[i]);
          expect(typeof arg.description).toBe("string");
        }
      }
    });
  });

  describe("Message Generation", () => {
    test("genealogy prompt should generate proper message structure", () => {
      const prompt = prologPrompts.genealogy;
      const messages = prompt.messages({ family_info: "John is Mary's father" });

      expect(Array.isArray(messages)).toBe(true);
      expect(messages).toHaveLength(1);

      const message = messages[0];
      expect(message).toHaveProperty("role", "user");
      expect(message).toHaveProperty("content");
      expect(message.content).toHaveProperty("type", "text");
      expect(message.content).toHaveProperty("text");
      expect(typeof message.content.text).toBe("string");

      const text = message.content.text;
      expect(text).toContain("family tree");
      expect(text).toContain("John is Mary's father");
      expect(text).toContain("clauses");
      expect(text).toContain("parent_of");
      expect(text).toContain("STEP");
    });

    test("genealogy prompt should handle missing family_info argument", () => {
      const prompt = prologPrompts.genealogy;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("[Please provide family members and their relationships]");
      expect(text).toContain("WORKFLOW");
    });

    test("scheduling prompt should generate proper message structure", () => {
      const prompt = prologPrompts.scheduling;
      const messages = prompt.messages({ tasks: "Task1 (5 days), Task2 (3 days) depends on Task1" });

      const text = messages[0].content.text;
      expect(text).toContain("scheduling");
      expect(text).toContain("Task1 (5 days)");
      expect(text).toContain("files");
      expect(text).toContain("clpfd");
      expect(text).toContain("STEP");
      expect(text).toContain("constraint");
    });

    test("scheduling prompt should handle missing tasks argument", () => {
      const prompt = prologPrompts.scheduling;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("[Please provide tasks with durations and dependencies]");
      expect(text).toContain("WORKFLOW");
    });

    test("puzzle prompt should handle missing puzzle argument", () => {
      const prompt = prologPrompts.puzzle;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      // When no puzzle provided, should suggest 3 puzzles
      expect(text).toContain("3 interesting logic puzzles");
      expect(text).toContain("constraint programming");
      expect(text).toContain("Zebra Puzzle");
      expect(text).toContain("N-Queens");
    });

    test("puzzle prompt should use provided puzzle argument", () => {
      const prompt = prologPrompts.puzzle;
      const puzzle = "1. The red house is next to the blue house.\n2. The green house is on the left end.";
      const messages = prompt.messages({ puzzle });

      const text = messages[0].content.text;
      expect(text).toContain(puzzle);
      expect(text).not.toContain("3 interesting logic puzzles");
      expect(text).toContain("files");
      expect(text).toContain("WORKFLOW");
    });

    test("puzzle prompt should suggest puzzles when given empty string", () => {
      const prompt = prologPrompts.puzzle;
      const messages = prompt.messages({ puzzle: "" });

      const text = messages[0].content.text;
      // Empty string should be treated as "not provided" → suggest puzzles
      expect(text).toContain("3 interesting logic puzzles");
      expect(text).not.toContain("WORKFLOW");
    });

    test("puzzle prompt should suggest puzzles when given whitespace-only string", () => {
      const prompt = prologPrompts.puzzle;
      const messages = prompt.messages({ puzzle: "   " });

      const text = messages[0].content.text;
      // Whitespace-only should be treated as "not provided" → suggest puzzles
      expect(text).toContain("3 interesting logic puzzles");
      expect(text).not.toContain("WORKFLOW");
    });

    test("puzzle prompt should include CLP(FD) workflow", () => {
      const prompt = prologPrompts.puzzle;
      const messagesWithPuzzle = prompt.messages({ puzzle: "Test puzzle" });

      const text = messagesWithPuzzle[0].content.text;
      expect(text).toContain("clauses");
      expect(text).toContain("all_different");
      expect(text).toContain("ins");
      expect(text).toContain("label");
      expect(text).toContain("solve/1");
      expect(text).toContain("query");
    });

    test("grammar prompt should generate proper message structure", () => {
      const prompt = prologPrompts.grammar;
      const messages = prompt.messages({ sentence: "the cat sat on the mat" });

      const text = messages[0].content.text;
      expect(text).toContain("the cat sat on the mat");
      expect(text).toContain("DCG");
      expect(text).toContain("phrase/2");
      expect(text).toContain("clauses");
      expect(text).toContain("STEP");
    });

    test("grammar prompt should handle missing sentence argument", () => {
      const prompt = prologPrompts.grammar;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      // Should use default sentence
      expect(text).toContain("the cat sat on the mat");
      expect(text).toContain("DCG");
    });
  });

  describe("Content Validation", () => {
    test("all prompts should demonstrate MCP tool usage patterns", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        // Use default args or test args to trigger full workflow
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text.toLowerCase();

        // Each prompt should mention specific MCP tools (unified API)
        expect(text).toMatch(/clauses|files|query|workspace/);
      }
    });

    test("prompts should emphasize Prolog patterns", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text.toLowerCase();

        expect(text).toMatch(/prolog|predicate|query|rules?|facts?/);
      }
    });

    test("all prompts should have structured workflows", () => {
      const allPrompts = Object.values(prologPrompts);

      for (const prompt of allPrompts) {
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text;

        // Should have structured steps/workflow
        expect(text).toMatch(/STEP|WORKFLOW/i);
      }
    });

    test("domain prompts should emphasize tool usage through examples", () => {
      // All new prompts should demonstrate MCP tools
      const prompts = ["genealogy", "scheduling", "puzzle", "grammar"];

      for (const promptName of prompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text.toLowerCase();

        // Check for unified tool names
        expect(text).toMatch(/clauses|files|workspace/);
        expect(text).toContain("query");
      }
    });

    test("all prompts should include communication directives", () => {
      // All prompts with workflows should have execution pattern and result markers
      const prompts = ["genealogy", "scheduling", "puzzle", "grammar"];

      for (const promptName of prompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages({
          family_info: "test",
          tasks: "test",
          puzzle: "test",
          sentence: "test"
        });
        const text = messages[0].content.text;

        // Should have execution pattern directive
        expect(text).toContain("EXECUTION PATTERN:");
        expect(text).toContain("announce action");
        expect(text).toContain("show tool calls/results");

        // Should have result markers (→) in workflow
        expect(text).toContain("→");
      }
    });
  });

  describe("Argument Handling", () => {
    test("prompts should handle undefined arguments gracefully", () => {
      const allPromptNames = Object.keys(prologPrompts);

      for (const promptName of allPromptNames) {
        const prompt = prologPrompts[promptName];

        // Should not throw when called without arguments
        expect(() => prompt.messages()).not.toThrow();
        expect(() => prompt.messages({})).not.toThrow();
        expect(() => prompt.messages(undefined)).not.toThrow();
      }
    });

    test("prompts should handle extra arguments gracefully", () => {
      const prompt = prologPrompts.genealogy;

      expect(() => prompt.messages({
        family_info: "John is Mary's father",
        extraArg: "should be ignored",
        anotherExtra: 123
      })).not.toThrow();

      const messages = prompt.messages({ family_info: "John is Mary's father", extraArg: "ignored" });
      expect(messages[0].content.text).toContain("John is Mary's father");
    });

    test("puzzle prompt should handle both provided and missing arguments", () => {
      const prompt = prologPrompts.puzzle;

      // Without puzzle - should suggest
      const withoutPuzzle = prompt.messages();
      expect(withoutPuzzle[0].content.text).toContain("3 interesting logic puzzles");

      // With puzzle - should solve
      const withPuzzle = prompt.messages({ puzzle: "Test puzzle" });
      expect(withPuzzle[0].content.text).toContain("Test puzzle");
      expect(withPuzzle[0].content.text).toContain("WORKFLOW");
    });

    test("grammar prompt should use default sentence when not provided", () => {
      const prompt = prologPrompts.grammar;

      const messages = prompt.messages();
      expect(messages[0].content.text).toContain("the cat sat on the mat");
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
      const domainExamples = ["genealogy", "scheduling", "puzzle", "grammar"];

      // Verify all prompts are in domain_examples category
      const allPromptNames = Object.keys(prologPrompts);

      expect(domainExamples.sort()).toEqual(allPromptNames.sort());
    });
  });
});
