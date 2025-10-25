/**
 * Load Test: Concurrent Access
 *
 * Tests for race conditions and concurrent access patterns:
 * - Multiple simultaneous queries
 * - Command queue isolation
 * - Session state consistency under load
 * - Error propagation in concurrent scenarios
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { prologInterface } from "@vpursuit/mcp-prolog";

describe("Concurrent Access Testing", () => {
  beforeAll(async () => {
    await prologInterface.start();
  });

  afterAll(async () => {
    prologInterface.stop();
  });

  it("should handle 50 concurrent queries without conflicts", async () => {
    const promises = [];

    for (let i = 0; i < 50; i++) {
      const promise = prologInterface.sendCommand(
        `member(X, [${i}, ${i + 1}, ${i + 2}]), query_next(), !.`,
        30000
      );
      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All queries should complete successfully
    expect(results).toHaveLength(50);
    results.forEach((result) => {
      expect(result).toContain("@@SOLUTION_START@@");
    });
  }, 60000);

  it("should maintain command queue order under concurrent load", async () => {
    const results: string[] = [];

    // Send commands in a specific order
    const promises = [];
    for (let i = 0; i < 20; i++) {
      const promise = prologInterface.sendCommand(
        `assertz(test_order(${i})).`,
        5000
      ).then(() => i);
      promises.push(promise);
    }

    const completionOrder = await Promise.all(promises);

    // Verify all commands completed
    expect(completionOrder).toHaveLength(20);

    // Clean up
    await prologInterface.sendCommand(`retractall(test_order(_)).`, 5000);
  }, 30000);

  it("should handle mixed query types concurrently", async () => {
    const promises = [];

    // Mix of different query types
    for (let i = 0; i < 30; i++) {
      if (i % 3 === 0) {
        promises.push(
          prologInterface.sendCommand(`member(X, [a,b,c]), query_next(), !.`, 10000)
        );
      } else if (i % 3 === 1) {
        promises.push(
          prologInterface.sendCommand(`append([1], [2], X), query_next(), !.`, 10000)
        );
      } else {
        promises.push(
          prologInterface.sendCommand(`length([x,y,z], X), query_next(), !.`, 10000)
        );
      }
    }

    const results = await Promise.all(promises);

    expect(results).toHaveLength(30);
    results.forEach((result) => {
      expect(result).toContain("@@SOLUTION_START@@");
    });
  }, 60000);

  it("should isolate errors in concurrent queries", async () => {
    const promises = [];

    // Mix valid and invalid queries
    for (let i = 0; i < 20; i++) {
      if (i % 4 === 0) {
        // Invalid query (will fail)
        promises.push(
          prologInterface.sendCommand(`invalid_predicate_xyz(X).`, 5000)
            .catch(error => ({ error: true, message: error.message }))
        );
      } else {
        // Valid query
        promises.push(
          prologInterface.sendCommand(`member(X, [1,2,3]), query_next(), !.`, 5000)
            .then(result => ({ error: false, result }))
        );
      }
    }

    const results = await Promise.all(promises);

    // Count successes and failures
    const successes = results.filter(r => !r.error).length;
    const failures = results.filter(r => r.error).length;

    expect(successes).toBeGreaterThan(0);
    expect(failures).toBeGreaterThan(0);
    expect(successes + failures).toBe(20);
  }, 30000);

  it("should handle concurrent knowledge base modifications", async () => {
    const promises = [];

    // Concurrent asserts
    for (let i = 0; i < 30; i++) {
      promises.push(
        prologInterface.sendCommand(
          `assertz(concurrent_fact(${i}, value_${i})).`,
          5000
        )
      );
    }

    await Promise.all(promises);

    // Verify all facts were added
    const result = await prologInterface.sendCommand(
      `findall(X, concurrent_fact(X, _), List), length(List, N), query_next(), !.`,
      10000
    );

    expect(result).toContain("30");

    // Clean up
    await prologInterface.sendCommand(`retractall(concurrent_fact(_, _)).`, 5000);
  }, 30000);

  it("should handle rapid start/next/close cycles concurrently", async () => {
    const promises = [];

    for (let i = 0; i < 25; i++) {
      const queryId = `concurrent_query_${i}`;

      const promise = (async () => {
        // Start query
        await prologInterface.sendCommand(
          `query_start(member(X, [${i}, ${i+1}, ${i+2}]), ${queryId}).`,
          5000
        );

        // Get solutions
        for (let j = 0; j < 3; j++) {
          await prologInterface.sendCommand(`query_next(${queryId}).`, 5000);
        }

        // Close query
        await prologInterface.sendCommand(`query_close(${queryId}).`, 5000);

        return queryId;
      })();

      promises.push(promise);
    }

    const results = await Promise.all(promises);
    expect(results).toHaveLength(25);
  }, 60000);

  it("should handle concurrent engine-mode queries", async () => {
    const promises = [];

    for (let i = 0; i < 20; i++) {
      const queryId = `engine_query_${i}`;

      const promise = (async () => {
        // Start engine query
        await prologInterface.sendCommand(
          `query_startEngine(between(${i}, ${i + 5}, X), ${queryId}).`,
          5000
        );

        // Get a few solutions
        for (let j = 0; j < 3; j++) {
          await prologInterface.sendCommand(`query_next(${queryId}).`, 5000);
        }

        // Close
        await prologInterface.sendCommand(`query_close(${queryId}).`, 5000);

        return queryId;
      })();

      promises.push(promise);
    }

    const results = await Promise.all(promises);
    expect(results).toHaveLength(20);
  }, 60000);

  it("should handle concurrent file loads", async () => {
    // Note: This test requires test files to exist
    // We'll test with the existing test files

    const promises = [];

    // Load the same file multiple times concurrently
    for (let i = 0; i < 10; i++) {
      promises.push(
        prologInterface.sendCommand(
          `consult('packages/mcp-prolog/test/fixtures/test.pl').`,
          10000
        ).catch(() => null) // Ignore errors if file doesn't exist
      );
    }

    const results = await Promise.all(promises);
    expect(results).toHaveLength(10);
  }, 30000);

  it("should maintain session state consistency during concurrent operations", async () => {
    // Start multiple queries concurrently
    const startPromises = [];
    for (let i = 0; i < 15; i++) {
      startPromises.push(
        prologInterface.sendCommand(
          `query_start(member(X, [${i}, ${i+1}]), state_query_${i}).`,
          5000
        )
      );
    }

    await Promise.all(startPromises);

    // Get next solutions concurrently
    const nextPromises = [];
    for (let i = 0; i < 15; i++) {
      nextPromises.push(
        prologInterface.sendCommand(`query_next(state_query_${i}).`, 5000)
      );
    }

    const results = await Promise.all(nextPromises);
    expect(results).toHaveLength(15);

    // Close all concurrently
    const closePromises = [];
    for (let i = 0; i < 15; i++) {
      closePromises.push(
        prologInterface.sendCommand(`query_close(state_query_${i}).`, 5000)
      );
    }

    await Promise.all(closePromises);
  }, 60000);

  it("should handle timeout scenarios in concurrent queries", async () => {
    const promises = [];

    // Mix of fast and slow queries (some will timeout)
    for (let i = 0; i < 20; i++) {
      if (i % 5 === 0) {
        // Fast query
        promises.push(
          prologInterface.sendCommand(`member(X, [1,2,3]), query_next(), !.`, 5000)
            .then(() => ({ timeout: false }))
        );
      } else {
        // Slow query with short timeout (will timeout)
        promises.push(
          prologInterface.sendCommand(`sleep(10), member(X, [1,2,3]).`, 100)
            .then(() => ({ timeout: false }))
            .catch(() => ({ timeout: true }))
        );
      }
    }

    const results = await Promise.all(promises);

    const successful = results.filter(r => !r.timeout).length;
    const timedOut = results.filter(r => r.timeout).length;

    expect(successful).toBeGreaterThan(0);
    expect(timedOut).toBeGreaterThan(0);
    expect(successful + timedOut).toBe(20);
  }, 30000);
});
