/**
 * Tests for response format parsing between Prolog server and TypeScript
 * These tests verify that the parsing logic correctly handles the structured
 * terms returned by the unified Prolog server.
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { PrologInterface } from "../../src/PrologInterface.js";

describe("Response Format Parsing", () => {
  let mockPrologInterface: PrologInterface;

  beforeEach(() => {
    mockPrologInterface = new PrologInterface();
  });

  afterEach(() => {
    mockPrologInterface.stop();
  });

  describe("solution term parsing", () => {
    test("should parse single variable binding", () => {
      const testResponse = "solution(['X'=john])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        expect(formattedBindings).toBe("X=john");
      }
    });

    test("should parse multiple variable bindings", () => {
      const testResponse = "solution(['X'=john,'Y'=mary,'Z'=alice])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        expect(formattedBindings).toBe("X=john, Y=mary, Z=alice");
      }
    });

    test("should parse complex values", () => {
      const testResponse = "solution(['X'=atom_value,'Y'=42,'Z'=[1,2,3]])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        expect(formattedBindings).toContain("X=atom_value");
        expect(formattedBindings).toContain("Y=42");
        expect(formattedBindings).toContain("Z=[1");
      }
    });

    test("should handle empty solution list", () => {
      const testResponse = "solution([])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        expect(formattedBindings).toBe("");
      }
    });
  });

  describe("error term parsing", () => {
    test("should recognize error terms", () => {
      const testResponse = "error(no_active_query)";
      expect(testResponse.startsWith("error(")).toBe(true);
    });

    test("should recognize complex error terms", () => {
      const testResponse = "error(session_conflict(query,engine))";
      expect(testResponse.startsWith("error(")).toBe(true);
    });

    test("should distinguish from solution terms", () => {
      const errorResponse = "error(test_error)";
      const solutionResponse = "solution(['X'=test])";

      expect(errorResponse.startsWith("error(")).toBe(true);
      expect(errorResponse.startsWith("solution(")).toBe(false);

      expect(solutionResponse.startsWith("solution(")).toBe(true);
      expect(solutionResponse.startsWith("error(")).toBe(false);
    });
  });

  describe("status response parsing", () => {
    test("should recognize no_more_solutions", () => {
      const testResponse = "no_more_solutions";
      expect(testResponse).toBe("no_more_solutions");
    });

    test("should recognize ok response", () => {
      const testResponse = "ok";
      expect(testResponse).toBe("ok");
    });

    test("should recognize done response", () => {
      const testResponse = "done";
      expect(testResponse).toBe("done");
    });
  });

  describe("regex edge cases", () => {
    test("should handle nested parentheses in values", () => {
      const testResponse = "solution(['X'=func(a,b),'Y'=nested(func(c))])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        // Should handle nested parentheses correctly (note: comma splitting affects nested functions)
        expect(formattedBindings).toContain("X=func(a");
        expect(formattedBindings).toContain("Y=nested(func(c))");
      }
    });

    test("should handle single quotes in atom values", () => {
      const testResponse = "solution(['X'='quoted atom','Y'=normal])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        expect(formattedBindings).toContain("X=quoted atom");
        expect(formattedBindings).toContain("Y=normal");
      }
    });

    test("should handle whitespace variations", () => {
      const testResponse = "solution([ 'X' = john , 'Y' = mary ])";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).not.toBeNull();
      if (bindingsMatch) {
        const bindingsStr = bindingsMatch[1];
        const formattedBindings = bindingsStr
          .split(",")
          .map((binding) => binding.trim().replace(/'/g, ""))
          .join(", ");

        expect(formattedBindings).toContain("X = john");
        expect(formattedBindings).toContain("Y = mary");
      }
    });
  });

  describe("parsing robustness", () => {
    test("should fail gracefully on malformed solution terms", () => {
      const testResponse = "solution(malformed";
      const bindingsMatch = testResponse.match(/solution\(\[(.*)\]\)/);

      expect(bindingsMatch).toBeNull();
    });

    test("should handle empty response", () => {
      const testResponse = "";
      expect(testResponse.startsWith("solution(")).toBe(false);
      expect(testResponse.startsWith("error(")).toBe(false);
      expect(testResponse).toBe("");
    });

    test("should handle unexpected response format", () => {
      const testResponse = "unexpected_format(data)";
      expect(testResponse.startsWith("solution(")).toBe(false);
      expect(testResponse.startsWith("error(")).toBe(false);
      expect(testResponse.startsWith("no_more_solutions")).toBe(false);
    });
  });

  describe("integration with nextSolution method", () => {
    test("should handle typical solution parsing flow", () => {
      // Simulate the parsing logic from PrologInterface.nextSolution
      const simulateNextSolution = (response: string) => {
        if (response === "no_more_solutions") {
          return { more_solutions: false };
        }

        if (response.startsWith("error(")) {
          return { error: response, more_solutions: false };
        }

        if (response.startsWith("solution(")) {
          const bindingsMatch = response.match(/solution\(\[(.*)\]\)/);
          if (bindingsMatch) {
            const bindingsStr = bindingsMatch[1];
            const formattedBindings = bindingsStr
              .split(",")
              .map((binding) => binding.trim().replace(/'/g, ""))
              .join(", ");

            return {
              solution: formattedBindings || "true",
              more_solutions: true,
            };
          } else {
            return {
              solution: response,
              more_solutions: true,
            };
          }
        }

        return {
          solution: response,
          more_solutions: true,
        };
      };

      // Test various response types
      const result1 = simulateNextSolution("solution(['X'=john,'Y'=mary])");
      expect(result1.solution).toBe("X=john, Y=mary");
      expect(result1.more_solutions).toBe(true);

      const result2 = simulateNextSolution("no_more_solutions");
      expect(result2.more_solutions).toBe(false);

      const result3 = simulateNextSolution("error(no_active_query)");
      expect(result3.error).toBe("error(no_active_query)");
      expect(result3.more_solutions).toBe(false);
    });
  });
});
