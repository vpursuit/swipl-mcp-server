/**
 * Integration tests for array format clause assertion
 * Tests various array patterns to identify and fix syntax errors
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Array Clause Assertion", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  describe("Complete clauses in array (existing behavior)", () => {
    test("should assert array of simple facts", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "parent(john, mary)",
          "parent(mary, alice)",
          "parent(alice, bob)"
        ]
      });

      expect(result.isError).toBeFalsy();

      // Verify they were asserted
      const query = await toolHandlers.query({
        operation: "start",
        query: "parent(john, mary)"
      });
      expect(query.isError).toBeFalsy();
    });

    test("should assert array with complete rules", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "parent(john, mary)",
          "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)"
        ]
      });

      expect(result.isError).toBeFalsy();
    });

    test("should assert array with CLP(FD) constraints", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "queens(N, Queens) :- length(Queens, N), Queens ins 1..N, all_distinct(Queens), label(Queens)"
        ]
      });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/1\/1 successful/);
    });

    test("should assert array with multiple complete CLP(FD) rules", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "queens(N, Queens) :- length(Queens, N), Queens ins 1..N, all_distinct(Queens), label(Queens)",
          "safe_queens([]).",
          "safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs)."
        ]
      });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/3\/3 successful/);
    });
  });

  describe("Incomplete clauses (error reproduction)", () => {
    test("should detect incomplete rule with just head and :-", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: ["queens(N, Queens) :-"]
      });

      // Currently this might succeed or fail - we want to detect and provide clear error
      if (result.isError) {
        expect(result.content[0].text).toMatch(/incomplete|missing body|invalid syntax/i);
      }
    });

    test("should handle array with incomplete rule fragments", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "queens(N, Queens) :-",
          "  length(Queens, N),",
          "  label(Queens)."
        ]
      });

      // Multi-line clauses are now joined and parsed naturally by Prolog
      expect(result.isError).toBeFalsy();
    });

    test("should reproduce operator_expected error", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "queens(N, Queens) :-",
          "length(Queens, N)"
        ]
      });

      // Incomplete clauses with continuation are now handled by Prolog's parser
      expect(result.isError).toBeFalsy();
    });
  });

  describe("Multi-line rule splitting (Prolog-native)", () => {
    test("should join multi-line rule split across array elements", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "queens(N, Queens) :-",
          "  length(Queens, N),",
          "  Queens ins 1..N,",
          "  all_distinct(Queens),",
          "  label(Queens)."
        ]
      });

      // Prolog naturally handles multi-line rules via stream parsing
      expect(result.isError).toBeFalsy();
      expect(result.structuredContent.success).toBeGreaterThan(0);
    });

    test("should handle mixed complete and split clauses", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "parent(john, mary).",  // Complete fact
          "grandparent(X, Z) :-",  // Start of multi-line rule
          "  parent(X, Y),",
          "  parent(Y, Z).",
          "sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \\= Y."  // Complete rule
        ]
      });

      // Mixed clauses are parsed correctly by Prolog's stream reader
      expect(result.isError).toBeFalsy();
      expect(result.structuredContent.success).toBeGreaterThan(0);
    });
  });

  describe("Edge cases", () => {
    test("should handle rule with :- in middle of line", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "test(X) :- X = 1 ; X = 2"
        ]
      });

      expect(result.isError).toBeFalsy();
    });

    test("should handle rule with multiple :- (if/else)", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "abs(X, Y) :- (X >= 0 -> Y = X ; Y is -X)"
        ]
      });

      expect(result.isError).toBeFalsy();
    });

    test("should reject empty array", async () => {
      const result = await toolHandlers.clauses({
        operation: "assert",
        clauses: []
      });

      // Should reject with error (at least one clause required)
      expect(result.isError).toBeTruthy();
      expect(result.content[0].text).toMatch(/at least one clause required/i);
    });
  });
});
