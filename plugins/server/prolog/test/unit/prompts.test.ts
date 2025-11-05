import { describe, test, expect } from "vitest";
import { prologPrompts } from "../../src/prompts.js";

describe("Prolog Prompts", () => {
  describe("Prompt Structure Validation", () => {
    test("all prompts should have required properties", () => {
      const expectedPrompts = [
        "expert",
        "knowledge",
        "optimize",
        "puzzle"
      ];

      const expectedNames = [
        "expert",
        "knowledge",
        "optimize",
        "puzzle"
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
        { name: "expert", args: ["task", "mode"], required: [false, false] },
        { name: "knowledge", args: ["domain", "mode"], required: [false, false] },
        { name: "optimize", args: ["query"], required: [true] },
        { name: "puzzle", args: ["puzzle"], required: [false] }
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
    test("expert prompt should generate proper message structure", () => {
      const prompt = prologPrompts.expert;
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

    test("expert prompt in reference mode should guide resource discovery", () => {
      const prompt = prologPrompts.expert;
      const messages = prompt.messages({ mode: "reference" });

      const text = messages[0].content.text;
      expect(text).toContain("resources");
      expect(text).toContain("reference://help");
      expect(text).toContain("reference://capabilities");
      expect(text).toContain("prolog://knowledge_base/predicates");
      expect(text).toContain("prolog://knowledge_base/dump");
    });

    test("knowledge prompt in analyze mode should emphasize resource reading", () => {
      const prompt = prologPrompts.knowledge;
      const messages = prompt.messages({ mode: "analyze" });

      const text = messages[0].content.text;
      expect(text).toContain("knowledge-base-predicates");
      expect(text).toContain("knowledge-base-dump");
      expect(text).toContain("capabilities");
      expect(text).toContain("STEP 1");
      expect(text).toContain("Resource Discovery");
    });

    test("expert prompt should handle missing task argument", () => {
      const prompt = prologPrompts.expert;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      // No placeholder expected; should still include discovery cues
      expect(text).toContain("Discovery");
      expect(text).toContain("capabilities");
      expect(text).toContain("security");
    });

    test("expert prompt should use provided task argument", () => {
      const prompt = prologPrompts.expert;
      const messages = prompt.messages({ task: "solve family tree relationships" });

      const text = messages[0].content.text;
      expect(text).toContain("solve family tree relationships");
    });

    test("knowledge prompt in build mode should handle missing domain argument", () => {
      const prompt = prologPrompts.knowledge;
      const messages = prompt.messages({ mode: "build" });

      const text = messages[0].content.text;
      expect(text).toContain("[Please specify a domain to model]");
      expect(text).toContain("PREPARATION PHASE");
    });

    test("knowledge prompt in build mode should use provided domain argument", () => {
      const prompt = prologPrompts.knowledge;
      const messages = prompt.messages({ domain: "medical diagnosis", mode: "build" });

      const text = messages[0].content.text;
      expect(text).toContain("medical diagnosis");
      expect(text).not.toContain("[Please specify a domain to model]");
    });

    test("optimize prompt should handle missing query argument", () => {
      const prompt = prologPrompts.optimize;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      expect(text).toContain("[Please provide a Prolog query to optimize]");
      expect(text).toContain("ANALYSIS PHASE");
    });

    test("optimize prompt should use provided query argument", () => {
      const prompt = prologPrompts.optimize;
      const messages = prompt.messages({ query: "findall(X, member(X, [1,2,3]), L)" });

      const text = messages[0].content.text;
      expect(text).toContain("findall(X, member(X, [1,2,3]), L)");
      expect(text).not.toContain("[Please provide a Prolog query to optimize]");
    });

    test("puzzle prompt should handle missing puzzle argument", () => {
      const prompt = prologPrompts.puzzle;
      const messages = prompt.messages();

      const text = messages[0].content.text;
      // When no puzzle provided, should offer 3 choices
      expect(text).toContain("3 different interesting logic puzzles");
      expect(text).toContain("constraint programming");
      expect(text).toContain("which puzzle");
      expect(text).toContain("Zebra Puzzle");
      expect(text).toContain("N-Queens");
    });

    test("puzzle prompt should use provided puzzle argument", () => {
      const prompt = prologPrompts.puzzle;
      const puzzle = "1. The red house is next to the blue house.\n2. The green house is on the left end.";
      const messages = prompt.messages({ puzzle });

      const text = messages[0].content.text;
      expect(text).toContain(puzzle);
      // When puzzle is provided, should not offer choices
      expect(text).not.toContain("3 different interesting logic puzzles");
      expect(text).toContain("knowledge_base_load_library");
      expect(text).toContain("WORKFLOW");
    });

    test("puzzle prompt should include CLP(FD) workflow", () => {
      const prompt = prologPrompts.puzzle;
      // When puzzle is provided, should include workflow details
      const messagesWithPuzzle = prompt.messages({ puzzle: "Test puzzle" });

      const text = messagesWithPuzzle[0].content.text;
      expect(text).toContain("knowledge_base_assert_many");
      expect(text).toContain("all_different");
      expect(text).toContain("ins");
      expect(text).toContain("label");
      expect(text).toContain("solve/1");
      expect(text).toContain("query_startEngine");
    });
  });

  describe("Content Validation", () => {
    test("all prompts should mention resources for context discovery", () => {
      const allPrompts = Object.entries(prologPrompts);

      for (const [key, prompt] of allPrompts) {
        const messages = prompt.messages();
        const text = messages[0].content.text;

        // Puzzle solver is focused on problem solving, not resource discovery
        if (key === "puzzle") {
          continue;
        }

        // Each prompt should guide users to check resources
        expect(text.toLowerCase()).toMatch(/resource|capabilities|help|knowledge-base-/);
      }
    });

    test("prompts should include security awareness", () => {
      const securityAwarePrompts = [
        "expert",
        "knowledge"
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
        "expert",
        "knowledge",
        "optimize"
      ];

      for (const promptName of workflowPrompts) {
        const prompt = prologPrompts[promptName];
        const messages = prompt.messages();
        const text = messages[0].content.text;

        // Should have structured phases/steps
        expect(text).toMatch(/PHASE|STEP \d|1\.|2\./);
      }
    });

    test("puzzle prompt should emphasize MCP tool usage", () => {
      const prompt = prologPrompts.puzzle;
      // When puzzle is provided, should emphasize MCP tools
      const messagesWithPuzzle = prompt.messages({ puzzle: "Test puzzle" });
      const text = messagesWithPuzzle[0].content.text.toLowerCase();

      expect(text).toContain("server");
      expect(text).toContain("knowledge_base_assert_many");
      expect(text).toContain("workflow");
      expect(text).toContain("query_startengine");
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
      const prompt = prologPrompts.expert;

      expect(() => prompt.messages({
        task: "test task",
        extraArg: "should be ignored",
        anotherExtra: 123
      })).not.toThrow();

      const messages = prompt.messages({ task: "test task", extraArg: "ignored" });
      expect(messages[0].content.text).toContain("test task");
    });

    test("expert prompt modes should work correctly", () => {
      const prompt = prologPrompts.expert;

      // Default (expert mode)
      const expertMessages = prompt.messages();
      expect(expertMessages[0].content.text).toContain("expert");

      // Reference mode
      const refMessages = prompt.messages({ mode: "reference" });
      expect(refMessages[0].content.text).toContain("PHASE 1");
      expect(refMessages[0].content.text).toContain("reference guide");
    });

    test("knowledge prompt modes should work correctly", () => {
      const prompt = prologPrompts.knowledge;

      // Default (build mode)
      const buildMessages = prompt.messages();
      expect(buildMessages[0].content.text).toContain("Build");

      // Analyze mode
      const analyzeMessages = prompt.messages({ mode: "analyze" });
      expect(analyzeMessages[0].content.text).toContain("Analyze");
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
      const expertGuidance = ["expert", "optimize"];
      const knowledgeBase = ["knowledge"];
      const problemSolving = ["puzzle"];

      // Verify all categories are covered
      const allCategorized = [...expertGuidance, ...knowledgeBase, ...problemSolving];
      const allPromptNames = Object.keys(prologPrompts);

      expect(allCategorized.sort()).toEqual(allPromptNames.sort());
    });
  });
});
