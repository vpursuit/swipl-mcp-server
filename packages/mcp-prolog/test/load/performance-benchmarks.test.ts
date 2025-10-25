/**
 * Load Test: Performance Benchmarks
 *
 * Benchmarks for:
 * - Query throughput
 * - Response parsing speed
 * - Command queue performance
 * - Large data handling
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { prologInterface } from "@vpursuit/mcp-prolog";

describe("Performance Benchmarks", () => {
  beforeAll(async () => {
    await prologInterface.start();
  });

  afterAll(async () => {
    prologInterface.stop();
  });

  it("should process 100 simple queries in < 5 seconds", async () => {
    const startTime = Date.now();

    for (let i = 0; i < 100; i++) {
      await prologInterface.sendCommand(
        `member(X, [1,2,3]), query_next(), !.`,
        30000
      );
    }

    const duration = Date.now() - startTime;
    const qps = (100 / duration) * 1000;

    console.log(`\n  Performance: 100 queries in ${duration}ms (${qps.toFixed(2)} queries/sec)`);
    expect(duration).toBeLessThan(5000);
  }, 30000);

  it("should handle complex queries efficiently", async () => {
    // Setup: Create facts for complex queries
    await prologInterface.sendCommand(
      `assertz(parent(john, mary)).`,
      5000
    );
    await prologInterface.sendCommand(
      `assertz(parent(john, tom)).`,
      5000
    );
    await prologInterface.sendCommand(
      `assertz(parent(mary, ann)).`,
      5000
    );
    await prologInterface.sendCommand(
      `assertz(grandparent(X, Z) :- parent(X, Y), parent(Y, Z)).`,
      5000
    );

    const startTime = Date.now();

    // Run 50 complex queries
    for (let i = 0; i < 50; i++) {
      await prologInterface.sendCommand(
        `grandparent(john, X), query_next(), !.`,
        10000
      );
    }

    const duration = Date.now() - startTime;
    const qps = (50 / duration) * 1000;

    console.log(`\n  Performance: 50 complex queries in ${duration}ms (${qps.toFixed(2)} queries/sec)`);

    // Cleanup
    await prologInterface.sendCommand(`retractall(parent(_, _)).`, 5000);
    await prologInterface.sendCommand(`retractall(grandparent(_, _)).`, 5000);

    expect(duration).toBeLessThan(8000);
  }, 30000);

  it("should efficiently handle large list operations", async () => {
    const startTime = Date.now();

    // Test with lists of varying sizes
    const sizes = [100, 500, 1000, 2000];

    for (const size of sizes) {
      const list = Array.from({ length: size }, (_, i) => i).join(",");
      await prologInterface.sendCommand(
        `length([${list}], X), query_next(), !.`,
        10000
      );
    }

    const duration = Date.now() - startTime;
    console.log(`\n  Performance: Large list operations in ${duration}ms`);

    expect(duration).toBeLessThan(5000);
  }, 30000);

  it("should maintain performance with knowledge base growth", async () => {
    // Add 1000 facts
    for (let i = 0; i < 1000; i++) {
      await prologInterface.sendCommand(
        `assertz(perf_fact(${i}, data_${i})).`,
        5000
      );
    }

    const startTime = Date.now();

    // Query the large knowledge base 50 times
    for (let i = 0; i < 50; i++) {
      await prologInterface.sendCommand(
        `perf_fact(${i * 20}, X), query_next(), !.`,
        5000
      );
    }

    const duration = Date.now() - startTime;
    const qps = (50 / duration) * 1000;

    console.log(`\n  Performance: 50 queries with 1000 facts in ${duration}ms (${qps.toFixed(2)} queries/sec)`);

    // Cleanup
    await prologInterface.sendCommand(`retractall(perf_fact(_, _)).`, 5000);

    expect(duration).toBeLessThan(10000);
  }, 60000);

  it("should efficiently parse large responses", async () => {
    // Create a large structure
    const largeList = Array.from({ length: 5000 }, (_, i) => i).join(",");
    await prologInterface.sendCommand(
      `assertz(large_response([${largeList}])).`,
      10000
    );

    const startTime = Date.now();

    // Query the large structure 20 times
    for (let i = 0; i < 20; i++) {
      const result = await prologInterface.sendCommand(
        `large_response(X), query_next(), !.`,
        10000
      );
      expect(result).toContain("@@SOLUTION_START@@");
    }

    const duration = Date.now() - startTime;
    console.log(`\n  Performance: 20 large responses in ${duration}ms`);

    // Cleanup
    await prologInterface.sendCommand(`retractall(large_response(_)).`, 5000);

    expect(duration).toBeLessThan(8000);
  }, 30000);

  it("should handle rapid assert/retract cycles efficiently", async () => {
    const startTime = Date.now();

    for (let i = 0; i < 200; i++) {
      await prologInterface.sendCommand(
        `assertz(temp_fact(${i})).`,
        5000
      );
      await prologInterface.sendCommand(
        `retract(temp_fact(${i})).`,
        5000
      );
    }

    const duration = Date.now() - startTime;
    const ops = (400 / duration) * 1000;

    console.log(`\n  Performance: 400 assert/retract operations in ${duration}ms (${ops.toFixed(2)} ops/sec)`);

    expect(duration).toBeLessThan(15000);
  }, 30000);

  it("should efficiently handle findall operations", async () => {
    // Setup: Create 500 facts
    for (let i = 0; i < 500; i++) {
      await prologInterface.sendCommand(
        `assertz(findall_fact(${i}, value_${i})).`,
        5000
      );
    }

    const startTime = Date.now();

    // Run 30 findall operations
    for (let i = 0; i < 30; i++) {
      await prologInterface.sendCommand(
        `findall(X, findall_fact(X, _), List), length(List, N), query_next(), !.`,
        10000
      );
    }

    const duration = Date.now() - startTime;
    const ops = (30 / duration) * 1000;

    console.log(`\n  Performance: 30 findall operations on 500 facts in ${duration}ms (${ops.toFixed(2)} ops/sec)`);

    // Cleanup
    await prologInterface.sendCommand(`retractall(findall_fact(_, _)).`, 5000);

    expect(duration).toBeLessThan(10000);
  }, 30000);

  it("should benchmark query session lifecycle", async () => {
    const startTime = Date.now();

    for (let i = 0; i < 100; i++) {
      const queryId = `bench_query_${i}`;

      // Start
      await prologInterface.sendCommand(
        `query_start(member(X, [a,b,c]), ${queryId}).`,
        5000
      );

      // Next
      await prologInterface.sendCommand(`query_next(${queryId}).`, 5000);

      // Close
      await prologInterface.sendCommand(`query_close(${queryId}).`, 5000);
    }

    const duration = Date.now() - startTime;
    const cycles = (100 / duration) * 1000;

    console.log(`\n  Performance: 100 query lifecycles in ${duration}ms (${cycles.toFixed(2)} cycles/sec)`);

    expect(duration).toBeLessThan(15000);
  }, 30000);

  it("should benchmark engine-mode query performance", async () => {
    const startTime = Date.now();

    for (let i = 0; i < 50; i++) {
      const queryId = `engine_bench_${i}`;

      // Start engine query
      await prologInterface.sendCommand(
        `query_startEngine(between(1, 10, X), ${queryId}).`,
        5000
      );

      // Get solutions
      for (let j = 0; j < 5; j++) {
        await prologInterface.sendCommand(`query_next(${queryId}).`, 5000);
      }

      // Close
      await prologInterface.sendCommand(`query_close(${queryId}).`, 5000);
    }

    const duration = Date.now() - startTime;
    const qps = (50 / duration) * 1000;

    console.log(`\n  Performance: 50 engine-mode queries in ${duration}ms (${qps.toFixed(2)} queries/sec)`);

    expect(duration).toBeLessThan(20000);
  }, 40000);

  it("should handle mixed workload efficiently", async () => {
    const startTime = Date.now();

    // Mixed workload: queries, asserts, retracts
    for (let i = 0; i < 100; i++) {
      const operation = i % 3;

      if (operation === 0) {
        // Query
        await prologInterface.sendCommand(
          `member(X, [1,2,3]), query_next(), !.`,
          5000
        );
      } else if (operation === 1) {
        // Assert
        await prologInterface.sendCommand(
          `assertz(mixed_fact(${i})).`,
          5000
        );
      } else {
        // Retract
        await prologInterface.sendCommand(
          `retractall(mixed_fact(${i - 1})).`,
          5000
        ).catch(() => {}); // Ignore if fact doesn't exist
      }
    }

    const duration = Date.now() - startTime;
    const ops = (100 / duration) * 1000;

    console.log(`\n  Performance: 100 mixed operations in ${duration}ms (${ops.toFixed(2)} ops/sec)`);

    // Cleanup
    await prologInterface.sendCommand(`retractall(mixed_fact(_)).`, 5000);

    expect(duration).toBeLessThan(12000);
  }, 30000);
});
