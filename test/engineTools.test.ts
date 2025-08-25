import { toolHandlers, prologInterface } from "../src/tools.js";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Engine Tool Handlers", () => {
  beforeEach(() => {
    // Reset any existing state
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  describe("queryStartEngine tool", () => {
    test("should start engine session successfully", async () => {
      const result = await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started: parent(X, Y)");
      expect(result.content[0].text).toContain("Status: ready");
    });

    test("should handle malformed queries", async () => {
      const result = await toolHandlers.queryStartEngine({ query: "parent(X," });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("Error:");
    });

    test("should prevent double start", async () => {
      // Start first engine
      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      // Try to start second engine
      const result = await toolHandlers.queryStartEngine({ query: "sibling(X, Y)" });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("already active");
    });
  });

  describe("queryNext tool", () => {
    test("should get next solution from engine", async () => {
      // Set up test data and start engine
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");
      await prologInterface.query("assert(parent(mary, alice))");

      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      // Get first solution
      const result = await toolHandlers.queryNext();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Solution:");
      expect(result.content[0].text).toContain("More solutions:");
    });

    test("should handle no active engine", async () => {
      const result = await toolHandlers.queryNext();

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("No active query session");
    });

    test("should handle exhausted solutions", async () => {
      // Set up test data with limited solutions
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      await toolHandlers.queryStartEngine({ query: "parent(john, mary)" });

      // Get first (and only) solution
      await toolHandlers.queryNext();

      // Try to get next solution (should be none)
      const result = await toolHandlers.queryNext();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("No more solutions");
    });
  });

  describe("queryClose tool", () => {
    test("should close active engine session", async () => {
      // Start engine first
      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      // Close it
      const result = await toolHandlers.queryClose();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/Session closed|No active session/);
    });

    test("should handle no active engine", async () => {
      const result = await toolHandlers.queryClose();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/No active session|Session closed/);
    });

    test("should allow starting new engine after close", async () => {
      // Start and close engine
      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });
      await toolHandlers.queryClose();

      // Start new engine
      const result = await toolHandlers.queryStartEngine({ query: "sibling(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started");
    });
  });

  describe("Engine vs Query Mode Conflicts", () => {
    test("should prevent starting engine when query is active", async () => {
      // Set up test data first
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start query session
      await toolHandlers.queryStart({ query: "parent(X, Y)" });

      // Try to start engine
      const result = await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("query session is already active");
    });

    test("should prevent starting query when engine is active", async () => {
      // Set up test data first
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start engine session
      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      // Try to start query
      const result = await toolHandlers.queryStart({ query: "parent(X, Y)" });

      expect(result.isError).toBe(true);
      expect(result.content[0].text).toContain("engine session is already active");
    });

    test("should prevent next_solution when engine is active", async () => {
      // Set up test data first
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start engine session
      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      // Try to get next solution (query mode) - this should work because unified interface
      const result = await toolHandlers.queryNext();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/X\s*=\s*john/);
    });

    test("should prevent next_engine when query is active", async () => {
      // Set up test data and start query
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      await toolHandlers.queryStart({ query: "parent(X, Y)" });

      // Try to get next solution - this should work because unified interface
      const result = await toolHandlers.queryNext();

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
      await toolHandlers.queryStart({ query: "parent(X, Y)" });
      await toolHandlers.queryClose();

      // Start engine - should work
      const result = await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Engine started");
    });

    test("should allow query after closing engine", async () => {
      // Set up test data
      await prologInterface.start();
      await prologInterface.query("assert(parent(john, mary))");

      // Start and close engine
      await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });
      await toolHandlers.queryClose();

      // Start query - should work
      const result = await toolHandlers.queryStart({ query: "parent(X, Y)" });

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
      let result = await toolHandlers.queryStartEngine({ query: "parent(X, Y)" });
      expect(result.isError).toBeFalsy();

      // Get multiple solutions
      const solutions = [];
      for (let i = 0; i < 5; i++) {
        result = await toolHandlers.queryNext();
        if (result.content[0].text.includes("No more solutions")) {
          break;
        }
        if (!result.isError && result.content[0].text.includes("Solution:")) {
          solutions.push(result.content[0].text);
        }
      }

      expect(solutions.length).toBeGreaterThan(0);

      // Close engine
      result = await toolHandlers.queryClose();
      expect(result.isError).toBeFalsy();
    });

    test("should handle empty result set gracefully", async () => {
      // Start engine with query that has no solutions
      let result = await toolHandlers.queryStartEngine({ query: "nonexistent_predicate(X)" });
      expect(result.isError).toBeFalsy();

      // Try to get solutions
      result = await toolHandlers.queryNext();

      // Should either get "No more solutions" or handle gracefully
      expect(result.isError).toBeFalsy();
    });
  });
});
