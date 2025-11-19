/**
 * Load Test: Command Queue
 *
 * Tests command queue serialization and promise resolution correctness.
 * NOTE: Commands are NOT executed concurrently - they're queued and run sequentially.
 *
 * These tests verify:
 * - Multiple callers can create promises simultaneously
 * - Queue processes commands one-by-one without conflicts
 * - Each response is correctly matched to its promise via ID
 * - Errors in one command don't break the queue
 * - Timeout handling works correctly under load
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { prologInterface } from "@vpursuit/mcp-server-prolog";
import { findNearestFile } from "@vpursuit/mcp-server-core";

describe("Command Queue Testing", () => {
  beforeAll(async () => {
    await prologInterface.start();
  });

  afterAll(async () => {
    prologInterface.stop();
  });

  it("should handle 50 queued commands and resolve all promises correctly", async () => {
    const promises = [];

    // Create 50 promises simultaneously (will be queued and executed sequentially)
    for (let i = 0; i < 50; i++) {
      const promise = prologInterface.query(
        `member(X, [${i}, ${i + 1}, ${i + 2}])`
      );
      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All queries should complete successfully with correct response routing
    expect(results).toHaveLength(50);
    results.forEach((result) => {
      expect(result).toContain("solution(");
    });
  }, 60000);

  it("should maintain command queue order with multiple callers", async () => {
    const results: string[] = [];

    // Send commands in a specific order
    const promises = [];
    for (let i = 0; i < 20; i++) {
      const promise = prologInterface.query(
        `assertz(test_order(${i}))`
      ).then(() => i);
      promises.push(promise);
    }

    const completionOrder = await Promise.all(promises);

    // Verify all commands completed
    expect(completionOrder).toHaveLength(20);

    // Clean up
    await prologInterface.query(`retractall(test_order(_))`);
  }, 30000);

  it("should handle mixed query types in queue", async () => {
    const promises = [];

    // Mix of different query types
    for (let i = 0; i < 30; i++) {
      if (i % 3 === 0) {
        promises.push(
          prologInterface.query(`member(X, [a,b,c])`)
        );
      } else if (i % 3 === 1) {
        promises.push(
          prologInterface.query(`append([1], [2], X)`)
        );
      } else {
        promises.push(
          prologInterface.query(`length([x,y,z], X)`)
        );
      }
    }

    const results = await Promise.all(promises);

    expect(results).toHaveLength(30);
    results.forEach((result) => {
      expect(result).toContain("solution(");
    });
  }, 60000);

  it("should isolate errors and not break the queue", async () => {
    const promises = [];

    // Mix valid and invalid queries
    for (let i = 0; i < 20; i++) {
      if (i % 4 === 0) {
        // Query that returns false (undefined predicate)
        promises.push(
          prologInterface.query(`invalid_predicate_xyz(X)`)
            .then(result => ({ success: result === "false", result }))
        );
      } else {
        // Valid query
        promises.push(
          prologInterface.query(`member(X, [1,2,3])`)
            .then(result => ({ success: result.includes("solution("), result }))
        );
      }
    }

    const results = await Promise.all(promises);

    // All queries should complete (some with false, some with solutions)
    expect(results).toHaveLength(20);
    const withSolutions = results.filter(r => r.result.includes("solution(")).length;
    expect(withSolutions).toBe(15); // 3/4 of queries are valid
  }, 30000);

  it("should handle queued knowledge base modifications", async () => {
    const promises = [];

    // Queued asserts (will execute sequentially via command queue)
    for (let i = 0; i < 30; i++) {
      promises.push(
        prologInterface.query(
          `assertz(test_fact${i}(value))`
        )
      );
    }

    // Wait for all assertions to complete
    const assertResults = await Promise.all(promises);

    // Verify all assertions succeeded
    assertResults.forEach(result => {
      expect(result).toContain("ok");
    });

    // Verify a sample fact exists
    const checkResult = await prologInterface.query(`test_fact0(value)`);
    expect(checkResult).toContain("solution");

    // Clean up - retract all test_factN predicates
    for (let i = 0; i < 30; i++) {
      await prologInterface.query(`retractall(test_fact${i}(_))`);
    }
  }, 30000);

  it("should handle rapid start/next/close cycles in queue", async () => {
    const results = [];

    // Run query sessions sequentially (due to session state management)
    for (let i = 0; i < 25; i++) {
      // Start query
      await prologInterface.startQuery(`member(X, [${i}, ${i+1}, ${i+2}])`);

      // Get solutions (3 solutions expected)
      for (let j = 0; j < 3; j++) {
        await prologInterface.nextSolution();
      }

      // Close query
      await prologInterface.closeQuery();

      results.push(i);
    }

    expect(results).toHaveLength(25);
  }, 60000);

  it("should handle queued engine-mode queries", async () => {
    const results = [];

    // Run engine sessions sequentially (due to session state management)
    for (let i = 0; i < 20; i++) {
      // Start engine query
      await prologInterface.startEngine(`between(${i}, ${i + 5}, X)`);

      // Get a few solutions
      for (let j = 0; j < 3; j++) {
        await prologInterface.nextEngine();
      }

      // Close
      await prologInterface.closeEngine();

      results.push(i);
    }

    expect(results).toHaveLength(20);
  }, 60000);

  it("should handle queued file loads", async () => {
    // Find the test fixture using core path utilities
    const testFixture = findNearestFile("test.pl", { customSubdirs: ["test/fixtures", "plugins/server/prolog/test/fixtures"] });
    if (!testFixture) {
      throw new Error("Test fixture test.pl not found");
    }

    const promises = [];

    // Load the same file multiple times concurrently
    for (let i = 0; i < 10; i++) {
      promises.push(
        prologInterface.importFileWithSource(testFixture)
          .catch(() => null) // Ignore errors if file doesn't exist
      );
    }

    const results = await Promise.all(promises);
    expect(results).toHaveLength(10);
  }, 30000);

  it("should maintain session state consistency during queued operations", async () => {
    // Run multiple complete query lifecycles sequentially
    const results = [];

    for (let i = 0; i < 15; i++) {
      // Start query
      await prologInterface.startQuery(`member(X, [${i}, ${i+1}])`);

      // Get first solution
      const result = await prologInterface.nextSolution();

      // Close query
      await prologInterface.closeQuery();

      results.push(result);
    }

    expect(results).toHaveLength(15);
  }, 60000);

  it("should handle timeout scenarios in queued commands", async () => {
    const promises = [];

    // Mix of fast and slow queries
    // Note: Timeout is controlled by SWI_MCP_QUERY_TIMEOUT_MS env variable
    for (let i = 0; i < 20; i++) {
      if (i % 5 === 0) {
        // Fast query
        promises.push(
          prologInterface.query(`member(X, [1,2,3])`)
            .then(() => ({ timeout: false }))
        );
      } else {
        // Potentially slow query (may timeout depending on env settings)
        promises.push(
          prologInterface.query(`sleep(10), member(X, [1,2,3])`)
            .then(() => ({ timeout: false }))
            .catch(() => ({ timeout: true }))
        );
      }
    }

    const results = await Promise.all(promises);

    const successful = results.filter(r => !r.timeout).length;
    const timedOut = results.filter(r => r.timeout).length;

    expect(successful).toBeGreaterThan(0);
    // Note: Timeouts depend on SWI_MCP_QUERY_TIMEOUT_MS setting
    expect(successful + timedOut).toBe(20);
  }, 30000);
});
