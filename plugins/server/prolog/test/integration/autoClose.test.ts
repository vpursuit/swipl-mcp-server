import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Auto-Close Query Lifecycle", () => {
  beforeEach(async () => {
    // Reset any existing state
    await prologInterface.stop();
    await prologInterface.start();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  describe("Query → Query transitions", () => {
    test("should automatically close active query when starting new query", async () => {
      // Set up test data
      await prologInterface.query("assert(test_fact(first))");
      await prologInterface.query("assert(test_fact(second))");

      // Start first query
      let result = await toolHandlers.queryStart({ query: "test_fact(X)" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");

      // Get first solution from first query
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Start second query WITHOUT closing first (should auto-close)
      result = await toolHandlers.queryStart({ query: "test_fact(Y)" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");

      // Should be able to get solutions from second query
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      await toolHandlers.queryClose();
    });

    test("should handle auto-close when query is completed but not closed", async () => {
      await prologInterface.query("assert(single_fact(value))");

      // Start query and exhaust all solutions
      await toolHandlers.queryStart({ query: "single_fact(X)" });
      await toolHandlers.queryNext(); // Get the solution
      let result = await toolHandlers.queryNext(); // Exhaust (no more solutions)
      expect(result.content[0].text).toContain("No more solutions");

      // Start new query WITHOUT explicit close (should auto-close completed query)
      result = await toolHandlers.queryStart({ query: "single_fact(Y)" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");

      await toolHandlers.queryClose();
    });
  });

  describe("Engine → Engine transitions", () => {
    test("should automatically close active engine when starting new engine", async () => {
      await prologInterface.query("assert(engine_test(first))");
      await prologInterface.query("assert(engine_test(second))");

      // Start first engine
      let result = await toolHandlers.queryStartEngine({ query: "engine_test(X)" });
      expect(result.isError).toBeFalsy();

      // Get first solution
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Start second engine WITHOUT closing first (should auto-close)
      result = await toolHandlers.queryStartEngine({ query: "engine_test(Y)" });
      expect(result.isError).toBeFalsy();

      // Should be able to get solutions from second engine
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      await toolHandlers.queryClose();
    });

    test("should handle auto-close when engine is completed but not closed", async () => {
      await prologInterface.query("assert(engine_single(value))");

      // Start engine and exhaust all solutions
      await toolHandlers.queryStartEngine({ query: "engine_single(X)" });
      await toolHandlers.queryNext(); // Get the solution
      let result = await toolHandlers.queryNext(); // Exhaust (no more solutions)
      expect(result.content[0].text).toContain("No more solutions");

      // Start new engine WITHOUT explicit close (should auto-close completed engine)
      result = await toolHandlers.queryStartEngine({ query: "engine_single(Y)" });
      expect(result.isError).toBeFalsy();

      await toolHandlers.queryClose();
    });
  });

  describe("Query ⇄ Engine transitions", () => {
    test("should automatically close active query when starting engine", async () => {
      await prologInterface.query("assert(transition_test(value))");

      // Start query
      let result = await toolHandlers.queryStart({ query: "transition_test(X)" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");

      // Get first solution
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Start engine WITHOUT closing query (should auto-close query)
      result = await toolHandlers.queryStartEngine({ query: "transition_test(Y)" });
      expect(result.isError).toBeFalsy();

      // Should be able to get solutions from engine
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      await toolHandlers.queryClose();
    });

    test("should automatically close active engine when starting query", async () => {
      await prologInterface.query("assert(reverse_transition(value))");

      // Start engine
      let result = await toolHandlers.queryStartEngine({ query: "reverse_transition(X)" });
      expect(result.isError).toBeFalsy();

      // Get first solution
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      // Start query WITHOUT closing engine (should auto-close engine)
      result = await toolHandlers.queryStart({ query: "reverse_transition(Y)" });
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Query started");

      // Should be able to get solutions from query
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      await toolHandlers.queryClose();
    });

    test("should handle multiple rapid mode switches", async () => {
      await prologInterface.query("assert(rapid_test(value1))");
      await prologInterface.query("assert(rapid_test(value2))");

      // Query → Engine → Query → Engine (without explicit closes)
      let result = await toolHandlers.queryStart({ query: "rapid_test(A)" });
      expect(result.isError).toBeFalsy();

      result = await toolHandlers.queryStartEngine({ query: "rapid_test(B)" });
      expect(result.isError).toBeFalsy();

      result = await toolHandlers.queryStart({ query: "rapid_test(C)" });
      expect(result.isError).toBeFalsy();

      result = await toolHandlers.queryStartEngine({ query: "rapid_test(D)" });
      expect(result.isError).toBeFalsy();

      // Final query should work
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      await toolHandlers.queryClose();
    });
  });

  describe("Auto-close error handling", () => {
    test("should recover gracefully if auto-close fails", async () => {
      await prologInterface.query("assert(recovery_test(value))");

      // Start a query
      let result = await toolHandlers.queryStart({ query: "recovery_test(X)" });
      expect(result.isError).toBeFalsy();

      // Even if there's an issue, starting new query should work
      result = await toolHandlers.queryStart({ query: "recovery_test(Y)" });
      expect(result.isError).toBeFalsy();

      // New query should be functional
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");

      await toolHandlers.queryClose();
    });

    test("should maintain state consistency after auto-close", async () => {
      await prologInterface.query("assert(state_test(first))");
      await prologInterface.query("assert(state_test(second))");

      // Start query, get partial results
      await toolHandlers.queryStart({ query: "state_test(X)" });
      let result = await toolHandlers.queryNext();
      expect(result.content[0].text).toContain("first");

      // Auto-close by starting new query
      await toolHandlers.queryStart({ query: "state_test(Y)" });

      // Should start fresh iteration
      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("first"); // Fresh start

      await toolHandlers.queryClose();
    });
  });

  describe("Backward compatibility", () => {
    test("should still work with explicit close calls", async () => {
      await prologInterface.query("assert(explicit_test(value))");

      // Traditional explicit close workflow
      let result = await toolHandlers.queryStart({ query: "explicit_test(X)" });
      expect(result.isError).toBeFalsy();

      result = await toolHandlers.queryNext();
      expect(result.isError).toBeFalsy();

      result = await toolHandlers.queryClose();
      expect(result.isError).toBeFalsy();

      // Start new query after explicit close
      result = await toolHandlers.queryStart({ query: "explicit_test(Y)" });
      expect(result.isError).toBeFalsy();

      await toolHandlers.queryClose();
    });

    test("should not break existing test patterns", async () => {
      // This mimics the pattern used in existing tests
      await prologInterface.query("assert(compat_test(a))");
      await prologInterface.query("assert(compat_test(b))");

      // Start query
      await toolHandlers.queryStart({ query: "compat_test(X)" });

      // Get all solutions
      const solutions = [];
      let result;
      do {
        result = await toolHandlers.queryNext();
        if (!result.isError && result.content[0].text.includes("Solution:")) {
          solutions.push(result.content[0].text);
        }
      } while (!result.content[0].text.includes("No more solutions"));

      expect(solutions.length).toBe(2);

      // Close explicitly (still supported)
      await toolHandlers.queryClose();
    });
  });
});
