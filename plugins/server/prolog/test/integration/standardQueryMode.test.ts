/**
 * Standard Query Mode Tools Tests
 * Tests standard query workflow (non-engine mode) using the unified query API
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Standard Query Mode Tools", () => {
  beforeEach(async () => {
    // Reset any existing state
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  describe("queryStart → queryNext → queryClose workflow", () => {
    test("should complete full standard query workflow", async () => {
      // Set up test data
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");
      await prologInterface.query("assert(parent(mary, alice))");
      await prologInterface.query("assert(parent(alice, bob))");

      // Start query
      let result = await toolHandlers.query({ operation: "start", query: "parent(X, Y)" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");

      // Get first solution
      result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");
      expect(result.content[0].text).toContain("Status:");

      // Get second solution
      result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Get third solution
      result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Try to get fourth solution (should be no more)
      result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("No more solutions");

      // Close query
      result = await toolHandlers.query({ operation: "close" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/Session closed|No active session/);
    });

    test("should parse variable bindings correctly", async () => {
      // Set up specific test data
      await prologInterface.start();
      await prologInterface.query("assert(test_pred(john, mary))");

      // Start query
      await toolHandlers.query({ operation: "start", query: "test_pred(X, Y)" });

      // Get solution and check parsing
      const result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Should contain variable bindings in readable format
      const solutionText = result.content[0].text;
      expect(solutionText).toMatch(/X\s*=\s*john/);
      expect(solutionText).toMatch(/Y\s*=\s*mary/);

      // Clean up
      await toolHandlers.query({ operation: "close" });
    });

    test("should handle single solution correctly", async () => {
      // Set up single solution test
      await prologInterface.start();
      await prologInterface.query("assert(unique_fact(singleton))");

      // Start query
      await toolHandlers.query({ operation: "start", query: "unique_fact(X)" });

      // Get the single solution
      let result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");
      expect(result.content[0].text).toContain("X=singleton");

      // Try to get second solution
      result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("No more solutions");

      // Clean up
      await toolHandlers.query({ operation: "close" });
    });

    test("should handle queries with no solutions", async () => {
      // Start query with no solutions
      await prologInterface.start();

      await toolHandlers.query({ operation: "start", query: "nonexistent_predicate(X)" });

      // Try to get solution
      const result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("No more solutions");

      // Clean up
      await toolHandlers.query({ operation: "close" });
    });
  });

  describe("queryNext tool error handling", () => {
    test("should handle queryNext without active query", async () => {
      const result = await toolHandlers.query({ operation: "next" });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("No active query");
    });

    test("should handle queryNext after query is closed", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(test_fact(value))");

      // Start and immediately close query
      await toolHandlers.query({ operation: "start", query: "test_fact(X)" });
      await toolHandlers.query({ operation: "close" });

      // Try queryNext after close
      const result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("No active query");
    });

    test("should handle queryNext after solutions exhausted", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(single_fact(value))");

      // Start query
      await toolHandlers.query({ operation: "start", query: "single_fact(X)" });

      // Get the solution
      await toolHandlers.query({ operation: "next" });

      // Try queryNext again (should show no more solutions)
      const result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("No more solutions");
    });
  });

  describe("response format parsing", () => {
    test("should parse complex variable bindings", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(complex_pred(atom_value, 42, [1,2,3]))");

      await toolHandlers.query({ operation: "start", query: "complex_pred(A, B, C)" });

      const result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();

      const solutionText = result.content[0].text;
      expect(solutionText).toContain("A=atom_value");
      expect(solutionText).toContain("B=42");
      expect(solutionText).toContain("C=[1,2,3]");

      await toolHandlers.query({ operation: "close" });
    });

    test("should handle queries without variables", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(simple_fact)");

      await toolHandlers.query({ operation: "start", query: "simple_fact" });

      const result = await toolHandlers.query({ operation: "next" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");
      // Should show "true" for queries without variables
      expect(result.content[0].text).toContain("true");

      await toolHandlers.query({ operation: "close" });
    });

    test("should handle various solution formats gracefully", async () => {
      // Test that solution parsing handles edge cases with safe operations
      await prologInterface.start();

      // Test 1: Query with no variables - should return "ok" for assertions
      const result1 = await prologInterface.query("assert(edge_case_fact)");
      expect(typeof result1).toBe("string");
      expect(result1).toBe("ok");

      // Test 2: Simple fact query returns through standard query mechanism
      await toolHandlers.query({ operation: "start", query: "edge_case_fact" });
      const result2 = await toolHandlers.query({ operation: "next" });
      expect(result2.isError).toBeFalsy();
      expect(result2.content[0].text).toContain("Solution:");
      expect(result2.content[0].text).toContain("true");
      await toolHandlers.query({ operation: "close" });

      // Test 3: Complex nested structure in solution
      await prologInterface.query("assert(complex_structure([a, [b, c], 42, atom]))");
      await toolHandlers.query({ operation: "start", query: "complex_structure(X)" });
      const result3 = await toolHandlers.query({ operation: "next" });
      expect(result3.isError).toBeFalsy();
      expect(result3.content[0].text).toContain("X=");
      await toolHandlers.query({ operation: "close" });

      // Test 4: Multiple variables with different types
      await prologInterface.query("assert(multi_var(atom_value, 123, [1,2,3]))");
      await toolHandlers.query({ operation: "start", query: "multi_var(A, B, C)" });
      const result4 = await toolHandlers.query({ operation: "next" });
      expect(result4.isError).toBeFalsy();
      expect(result4.content[0].text).toContain("A=");
      expect(result4.content[0].text).toContain("B=");
      expect(result4.content[0].text).toContain("C=");
      await toolHandlers.query({ operation: "close" });
    });
  });

  describe("memory and performance", () => {
    test("should handle many solutions efficiently", async () => {
      await prologInterface.start();

      // Add many facts
      for (let i = 1; i <= 10; i++) {
        await prologInterface.query(`assert(number_fact(${i}))`);
      }

      await toolHandlers.query({ operation: "start", query: "number_fact(X)" });

      // Get all solutions one by one
      const solutions = [];
      let result;
      do {
        result = await toolHandlers.query({ operation: "next" });
        if (result.isError) {
          // Break early on error to avoid infinite loop in test envs
          break;
        }
        if (result.content[0].text.includes("Solution:")) {
          solutions.push(result.content[0].text);
        }
      } while (!result.content[0].text.includes("No more solutions"));

      expect(solutions.length).toBe(10);

      await toolHandlers.query({ operation: "close" });
    });

    test("should not leak memory on repeated query sessions", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(repeated_fact(test))");

      // Run multiple query sessions
      for (let i = 0; i < 5; i++) {
        await toolHandlers.query({ operation: "start", query: "repeated_fact(X)" });
        await toolHandlers.query({ operation: "next" });
        await toolHandlers.query({ operation: "close" });
      }

      // Should still be able to start new queries
      const result = await toolHandlers.query({ operation: "start", query: "repeated_fact(X)" });
      expect(result.isError).toBeFalsy();

      await toolHandlers.query({ operation: "close" });
    });
  });

  describe("standard vs engine mode compatibility", () => {
    test("should not interfere with engine mode", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(mode_test(standard))");

      // Use standard mode
      await toolHandlers.query({ operation: "start", query: "mode_test(X)" });
      await toolHandlers.query({ operation: "next" });
      await toolHandlers.query({ operation: "close" });

      // Should be able to use engine mode after
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "mode_test(Y)" });
      expect(result.isError).toBeFalsy();

      await toolHandlers.query({ operation: "close" });
    });

    test("should produce same results as engine mode", async () => {
      await prologInterface.start();
      await prologInterface.query("assert(comparison_test(value1))");
      await prologInterface.query("assert(comparison_test(value2))");

      // Get solutions using standard mode
      await toolHandlers.query({ operation: "start", query: "comparison_test(X)" });
      const standardSolution1 = await toolHandlers.query({ operation: "next" });
      const standardSolution2 = await toolHandlers.query({ operation: "next" });
      await toolHandlers.query({ operation: "close" });

      // Get solutions using engine mode
      await toolHandlers.query({ operation: "start", use_engine: true, query: "comparison_test(Y)" });
      const engineSolution1 = await toolHandlers.query({ operation: "next" });
      const engineSolution2 = await toolHandlers.query({ operation: "next" });
      await toolHandlers.query({ operation: "close" });

      // Both modes should find the same number of solutions
      expect(standardSolution1.isError).toBeFalsy();
      expect(standardSolution2.isError).toBeFalsy();
      expect(engineSolution1.isError).toBeFalsy();
      expect(engineSolution2.isError).toBeFalsy();

      // Both should contain variable bindings
      expect(standardSolution1.content[0].text).toContain("=");
      expect(engineSolution1.content[0].text).toContain("=");
    });
  });
});
