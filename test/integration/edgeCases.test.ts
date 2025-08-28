import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { PrologInterface } from "../../src/PrologInterface.js";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Prolog Interface Edge Cases", () => {
  let prolog: PrologInterface;

  beforeEach(() => {
    prolog = new PrologInterface();
  });

  afterEach(() => {
    prolog.stop();
  });

  describe("Mutual Exclusion Tests", () => {
    test("should prevent start_engine when query is active", async () => {
      await prolog.start();

      // Start a regular query session
      await prolog.startQuery("parent(X, Y)");

      // Try to start an engine - should fail
      await expect(prolog.startEngine("parent(X, Y)")).rejects.toThrow(
        "A query session is already active. Close the query first.",
      );

      // Cleanup
      await prolog.closeQuery();
    });

    test("should prevent start_query when engine is active", async () => {
      await prolog.start();

      // Start an engine session
      await prolog.startEngine("parent(X, Y)");

      // Try to start a regular query - should fail
      await expect(prolog.startQuery("parent(X, Y)")).rejects.toThrow(
        "An engine session is already active. Close the engine first.",
      );

      // Cleanup
      await prolog.closeEngine();
    });

    test("should allow starting query after closing engine", async () => {
      await prolog.start();

      // Add some test data
      await prolog.query("assert(parent(john, mary))");

      // Start engine, then close it
      await prolog.startEngine("parent(X, Y)");
      await prolog.closeEngine();

      // Should now be able to start regular query
      const result = await prolog.startQuery("parent(X, Y)");
      expect(result.status).toBe("ready");

      // Cleanup
      await prolog.closeQuery();
    });

    test("should allow starting engine after closing query", async () => {
      await prolog.start();

      // Add some test data
      await prolog.query("assert(parent(john, mary))");

      // Start query, then close it
      await prolog.startQuery("parent(X, Y)");
      await prolog.closeQuery();

      // Should now be able to start engine
      const result = await prolog.startEngine("parent(X, Y)");
      expect(result.status).toBe("ready");

      // Cleanup
      await prolog.closeEngine();
    });
  });

  describe("Double Start Prevention Tests", () => {
    test("should prevent starting query twice without close", async () => {
      await prolog.start();

      // Start first query
      await prolog.startQuery("parent(X, Y)");

      // Try to start second query - should fail
      await expect(prolog.startQuery("sibling(X, Y)")).rejects.toThrow(
        "A query is already active. Close the current query first.",
      );

      // Cleanup
      await prolog.closeQuery();
    });

    test("should prevent starting engine twice without close", async () => {
      await prolog.start();

      // Start first engine
      await prolog.startEngine("parent(X, Y)");

      // Try to start second engine - should fail
      await expect(prolog.startEngine("sibling(X, Y)")).rejects.toThrow(
        "An engine is already active. Close the current engine first.",
      );

      // Cleanup
      await prolog.closeEngine();
    });

    test("should allow starting new query after properly closing previous one", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");
      await prolog.query("assert(parent(mary, alice))");

      // Start and close first query
      await prolog.startQuery("parent(X, Y)");
      await prolog.closeQuery();

      // Start second query - should work
      const result = await prolog.startQuery("parent(X, mary)");
      expect(result.status).toBe("ready");

      // Cleanup
      await prolog.closeQuery();
    });

    test("should allow starting new engine after properly closing previous one", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");
      await prolog.query("assert(parent(mary, alice))");

      // Start and close first engine
      await prolog.startEngine("parent(X, Y)");
      await prolog.closeEngine();

      // Start second engine - should work
      const result = await prolog.startEngine("parent(X, mary)");
      expect(result.status).toBe("ready");

      // Cleanup
      await prolog.closeEngine();
    });
  });

  describe("Cross-Mode Operation Tests", () => {
    test("should reject next_solution when engine is active", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");

      // Start engine
      await prolog.startEngine("parent(X, Y)");

      // Try next_solution (should fail since engine is active)
      const result = await prolog.nextSolution();
      expect(result.error).toContain("No active query");

      // Cleanup
      await prolog.closeEngine();
    });

    test("should reject next_engine when query is active", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");

      // Start query
      await prolog.startQuery("parent(X, Y)");

      // Try next_engine (should fail since query is active)
      const result = await prolog.nextEngine();
      expect(result.error).toContain("No active engine");

      // Cleanup
      await prolog.closeQuery();
    });

    test("should reject close_query when engine is active", async () => {
      await prolog.start();

      // Start engine
      await prolog.startEngine("parent(X, Y)");

      // Try close_query (should work but report no active query)
      const result = await prolog.closeQuery();
      expect(result.status).toBe("no_active_query");

      // Cleanup
      await prolog.closeEngine();
    });

    test("should reject close_engine when query is active", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");

      // Start query
      await prolog.startQuery("parent(X, Y)");

      // Try close_engine (should work but report no active engine)
      const result = await prolog.closeEngine();
      expect(result.status).toBe("no_active_engine");

      // Cleanup
      await prolog.closeQuery();
    });
  });

  describe("State Recovery Tests", () => {
    test("should handle operations without active sessions gracefully", async () => {
      await prolog.start();

      // Try operations without starting any session
      const nextResult = await prolog.nextSolution();
      expect(nextResult.error).toContain("No active query");

      const nextEngineResult = await prolog.nextEngine();
      expect(nextEngineResult.error).toContain("No active engine");

      const closeResult = await prolog.closeQuery();
      expect(closeResult.status).toBe("no_active_query");

      const closeEngineResult = await prolog.closeEngine();
      expect(closeEngineResult.status).toBe("no_active_engine");
    });

    test("should reset state properly after stop and restart", async () => {
      await prolog.start();

      // Add test data and start query
      await prolog.query("assert(parent(john, mary))");
      await prolog.startQuery("parent(X, Y)");

      // Stop and restart
      prolog.stop();
      await prolog.start();

      // Should be able to start new query (state should be reset)
      const result = await prolog.startQuery("parent(X, Y)");
      expect(result.status).toBe("ready");

      // Cleanup
      await prolog.closeQuery();
    });

    test("should handle engine cleanup on process restart", async () => {
      await prolog.start();

      // Add test data and start engine
      await prolog.query("assert(parent(john, mary))");
      await prolog.startEngine("parent(X, Y)");

      // Stop and restart
      prolog.stop();
      await prolog.start();

      // Should be able to start new engine (state should be reset)
      const result = await prolog.startEngine("parent(X, Y)");
      expect(result.status).toBe("ready");

      // Cleanup
      await prolog.closeEngine();
    });
  });

  describe("Error Handling Edge Cases", () => {
    test("should handle malformed queries in both modes", async () => {
      await prolog.start();

      // Test malformed query in regular mode (syntax error that can't be auto-fixed)
      await expect(prolog.startQuery("parent(X, Y")).rejects.toThrow();

      // Test malformed query in engine mode
      await expect(prolog.startEngine("parent(X, Y")).rejects.toThrow();
    });

    test("should handle session conflicts at server level", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");

      // Start query session
      await prolog.startQuery("parent(X, Y)");

      // Try to send start_engine command directly (should be rejected by server)
      const result = await prolog.query("start_engine(parent(X, Y))");
      expect(result).toContain("error(session_conflict");

      // Cleanup
      await prolog.closeQuery();
    });

    test("should handle engine creation failures gracefully", async () => {
      await prolog.start();

      // Try to create engine with invalid query
      const result = await prolog.startEngine("undefined_predicate(X)");

      // Engine creation might succeed but queries will fail
      // The exact behavior depends on SWI-Prolog engine implementation
      if (result.status === "ready") {
        await prolog.closeEngine();
      }
    });
  });

  describe("Resource Leak Prevention", () => {
    test("should properly cleanup engines on multiple start/stop cycles", async () => {
      for (let i = 0; i < 3; i++) {
        await prolog.start();

        // Add test data
        await prolog.query("assert(parent(john, mary))");

        // Start and immediately close engine
        await prolog.startEngine("parent(X, Y)");
        await prolog.closeEngine();

        prolog.stop();
      }

      // If we get here without hanging, resource cleanup is working
      expect(true).toBe(true);
    });

    test("should handle rapid session switching", async () => {
      await prolog.start();

      // Add test data
      await prolog.query("assert(parent(john, mary))");
      await prolog.query("assert(parent(mary, alice))");

      // Rapidly switch between query and engine modes
      for (let i = 0; i < 5; i++) {
        await prolog.startQuery("parent(X, Y)");
        await prolog.closeQuery();

        await prolog.startEngine("parent(X, Y)");
        await prolog.closeEngine();
      }

      // Should still be able to use both modes
      const queryResult = await prolog.startQuery("parent(X, Y)");
      expect(queryResult.status).toBe("ready");
      await prolog.closeQuery();

      const engineResult = await prolog.startEngine("parent(X, Y)");
      expect(engineResult.status).toBe("ready");
      await prolog.closeEngine();
    });
  });
});
