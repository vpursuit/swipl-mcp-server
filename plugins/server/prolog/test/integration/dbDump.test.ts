/**
 * Integration tests for workspace snapshot tool
 * Tests the workspace snapshot functionality with source preservation
 * Updated for Step 5: Now uses workspace({operation: 'snapshot'}) tool
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Workspace Snapshot Tool", () => {
  beforeEach(async () => {
    // Reset any existing state
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  describe("workspace snapshot operation", () => {
    test("should return empty message when workspace is empty", async () => {
      const result = await toolHandlers.workspace({ operation: "snapshot" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("(empty workspace)");
      expect(result.structuredContent.has_content).toBe(false);
    });

    test("should preserve original source text for facts", async () => {
      // Add some facts to the knowledge base with original formatting
      await toolHandlers.clauses({ operation: "assert", clauses: "parent(john, mary)" });
      await toolHandlers.clauses({ operation: "assert", clauses: "parent(mary, alice)" });
      await toolHandlers.clauses({ operation: "assert", clauses: "age(john, 45)" });

      const result = await toolHandlers.workspace({ operation: "snapshot" });

      expect(result.isError).toBeFalsy();
      expect(result.structuredContent.has_content).toBe(true);

      // Snapshot preserves original source with spacing
      const snapshot = result.content[0].text;
      expect(snapshot).toContain("parent(john, mary)");
      expect(snapshot).toContain("parent(mary, alice)");
      expect(snapshot).toContain("age(john, 45)");
    });

    test("should preserve original source text for rules", async () => {
      // Add a rule to the knowledge base with original formatting
      await toolHandlers.clauses({ operation: "assert", clauses: "parent(tom, bob)" });
      await toolHandlers.clauses({ operation: "assert", clauses: "parent(bob, charlie)" });
      await toolHandlers.clauses({ operation: "assert", clauses: "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)" });

      const result = await toolHandlers.workspace({ operation: "snapshot" });

      expect(result.isError).toBeFalsy();
      expect(result.structuredContent.has_content).toBe(true);

      // Snapshot preserves original source with variable names and spacing
      const snapshot = result.content[0].text;
      expect(snapshot).toContain("parent(tom, bob)");
      expect(snapshot).toContain("parent(bob, charlie)");
      expect(snapshot).toContain("grandparent(X, Z) :- parent(X, Y), parent(Y, Z)");
    });

    test("should preserve source for mixed facts and rules", async () => {
      // Add both facts and rules with original formatting
      await toolHandlers.clauses({
        operation: "assert",
        clauses: [
          "likes(mary, food)",
          "likes(mary, wine)",
          "likes(john, wine)",
          "happy(X) :- likes(X, wine)"
        ]
      });

      const result = await toolHandlers.workspace({ operation: "snapshot" });

      expect(result.isError).toBeFalsy();
      expect(result.structuredContent.has_content).toBe(true);

      // Snapshot preserves original formatting
      const snapshot = result.content[0].text;
      expect(snapshot).toContain("likes(mary, food)");
      expect(snapshot).toContain("likes(mary, wine)");
      expect(snapshot).toContain("likes(john, wine)");
      expect(snapshot).toContain("happy(X) :- likes(X, wine)");
    });

    test("should include processing time in response", async () => {
      const result = await toolHandlers.workspace({ operation: "snapshot" });

      expect(result.isError).toBeFalsy();
      expect(result.structuredContent.processing_time_ms).toBeGreaterThanOrEqual(0);
    });

    test("should reflect changes after asserting and retracting facts", async () => {
      // Add some facts
      await toolHandlers.clauses({ operation: "assert", clauses: "test_fact(a)" });
      await toolHandlers.clauses({ operation: "assert", clauses: "test_fact(b)" });

      // Check initial snapshot
      let result = await toolHandlers.workspace({ operation: "snapshot" });
      expect(result.content[0].text).toContain("test_fact(a)");
      expect(result.content[0].text).toContain("test_fact(b)");

      // Retract one fact
      await toolHandlers.clauses({ operation: "retract", clauses: "test_fact(a)" });

      // Check updated snapshot
      result = await toolHandlers.workspace({ operation: "snapshot" });
      expect(result.content[0].text).not.toContain("test_fact(a)");
      expect(result.content[0].text).toContain("test_fact(b)");
    });
  });
});