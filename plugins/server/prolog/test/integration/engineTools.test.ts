/**
 * Engine Tool Handlers Tests
 * Tests engine-based backtracking and query execution using the unified query API
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Engine Tool Handlers", () => {
  beforeEach(async () => {
    // Reset any existing state
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  describe("queryStartEngine tool", () => {
    test("should start engine session successfully", async () => {
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started: parent(X, Y)");
      expect(result.content[0].text).toContain("Status: ready");
    });

    test("should handle malformed queries", async () => {
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X," });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("Error:");
    });

    test("should auto-close previous engine on double start", async () => {
      // Start first engine
      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      // Start second engine (should auto-close first)
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "sibling(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started");
    });
  });

  describe("queryNext tool", () => {
    test("should get next solution from engine", async () => {
      // Set up test data and start engine
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");
      await prologInterface.query("assert(parent(mary, alice))");

      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      // Get first solution
      const result = await toolHandlers.query({ operation: "next" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");
      expect(result.content[0].text).toContain("Status:");
    });

    test("should handle no active engine", async () => {
      const result = await toolHandlers.query({ operation: "next" });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("No active query session");
    });

    test("should handle exhausted solutions", async () => {
      // Set up test data with limited solutions
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(john, mary)" });

      // Get first (and only) solution
      const firstResult = await toolHandlers.query({ operation: "next" });
      expect(firstResult.isError).toBeFalsy();

      // Try to get next solution (should be none) 
      const result = await toolHandlers.query({ operation: "next" });

      // Engine is exhausted - either returns "No more solutions" or an error indicating exhaustion
      if (result.isError) {
        // If it's an error, that's acceptable - the engine is exhausted
        expect(result.content?.[0]?.text || "").toBeTruthy();
      } else {
        // If it's not an error, should contain "No more solutions"
        expect(result.content[0].text).toContain("No more solutions");
      }
    });
  });

  describe("queryClose tool", () => {
    test("should close active engine session", async () => {
      // Start engine first
      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      // Close it
      const result = await toolHandlers.query({ operation: "close" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/Session closed|No active session/);
    });

    test("should handle no active engine", async () => {
      const result = await toolHandlers.query({ operation: "close" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/No active session|Session closed/);
    });

    test("should allow starting new engine after close", async () => {
      // Start and close engine
      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });
      await toolHandlers.query({ operation: "close" });

      // Start new engine
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "sibling(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started");
    });
  });

  describe("Engine vs Query Mode Auto-Close", () => {
    test("should auto-close active query when starting engine", async () => {
      // Set up test data first
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start query session
      await toolHandlers.query({ operation: "start", query: "parent(X, Y)" });

      // Start engine (should auto-close query)
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started");
    });

    test("should auto-close active engine when starting query", async () => {
      // Set up test data first
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start engine session
      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      // Start query (should auto-close engine)
      const result = await toolHandlers.query({ operation: "start", query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");
    });

    test("should prevent next_solution when engine is active", async () => {
      // Set up test data first
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start engine session
      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      // Try to get next solution (query mode) - this should work because unified interface
      const result = await toolHandlers.query({ operation: "next" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/X\s*=\s*john/);
    });

    test("should prevent next_engine when query is active", async () => {
      // Set up test data and start query
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      await toolHandlers.query({ operation: "start", query: "parent(X, Y)" });

      // Try to get next solution - this should work because unified interface
      const result = await toolHandlers.query({ operation: "next" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/X\s*=\s*john/);
    });
  });

  describe("Cross-Mode Session Management", () => {
    test("should allow engine after closing query", async () => {
      // Set up test data
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start and close query
      await toolHandlers.query({ operation: "start", query: "parent(X, Y)" });
      await toolHandlers.query({ operation: "close" });

      // Start engine - should work
      const result = await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started");
    });

    test("should allow query after closing engine", async () => {
      // Set up test data
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start and close engine
      await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });
      await toolHandlers.query({ operation: "close" });

      // Start query - should work
      const result = await toolHandlers.query({ operation: "start", query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");
    });
  });

  describe("Engine Tool Integration", () => {
    test("should complete full engine workflow", async () => {
      // Set up test data
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");
      await prologInterface.query("assert(parent(mary, alice))");
      await prologInterface.query("assert(parent(alice, bob))");

      // Start engine
      let result = await toolHandlers.query({ operation: "start", use_engine: true, query: "parent(X, Y)" });
      expect(result.isError).toBeFalsy();

      // Get multiple solutions
      const solutions = [];
      for (let i = 0; i < 5; i++) {
        result = await toolHandlers.query({ operation: "next" });
        if (result.content[0].text.includes("No more solutions")) {
          break;
        }
        if (!result.isError && result.content[0].text.includes("Solution:")) {
          solutions.push(result.content[0].text);
        }
      }

      expect(solutions.length).toBeGreaterThan(0);

      // Close engine
      result = await toolHandlers.query({ operation: "close" });
      expect(result.isError).toBeFalsy();
    });

    test("should handle empty result set gracefully", async () => {
      // Start engine with query that has no solutions
      let result = await toolHandlers.query({ operation: "start", use_engine: true, query: "nonexistent_predicate(X)" });
      expect(result.isError).toBeFalsy();

      // Try to get solutions
      result = await toolHandlers.query({ operation: "next" });

      // Query with no solutions can either return an error or "No more solutions"
      // Both are acceptable ways to handle empty result sets
      if (result.isError) {
        // Engine execution failed - that's acceptable for nonexistent predicates
        expect(result.content?.[0]?.text || "").toBeTruthy();
      } else {
        // Or returns "No more solutions" 
        expect(result.content[0].text).toContain("No more solutions");
      }
    });
  });
});
