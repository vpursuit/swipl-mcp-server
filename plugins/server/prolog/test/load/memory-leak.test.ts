/**
 * Load Test: Memory Leak Detection
 *
 * Tests for memory leaks in:
 * - Response buffer accumulation
 * - Query promise map growth
 * - Command queue cleanup
 * - Large response handling
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { prologInterface } from "@vpursuit/mcp-server-prolog";

describe("Memory Leak Detection", () => {
  beforeAll(async () => {
    await prologInterface.start();
  });

  afterAll(async () => {
    prologInterface.stop();
  });

  it("should handle 1000 queries without memory growth", async () => {
    // Get initial memory usage
    const initialMemory = process.memoryUsage();

    // Run 1000 simple queries
    for (let i = 0; i < 1000; i++) {
      const result = await prologInterface.sendCommand(
        `member(X, [1,2,3]), query_next(), !.`,
        30000
      );
      expect(result).toBeDefined();
    }

    // Force garbage collection if available
    if (global.gc) {
      global.gc();
    }

    // Check memory growth (should be < 10MB growth)
    const finalMemory = process.memoryUsage();
    const heapGrowth = finalMemory.heapUsed - initialMemory.heapUsed;
    const heapGrowthMB = heapGrowth / 1024 / 1024;

    console.log(`Heap growth after 1000 queries: ${heapGrowthMB.toFixed(2)} MB`);
    expect(heapGrowthMB).toBeLessThan(10);
  }, 60000);

  it("should handle rapid query start/close cycles", async () => {
    const initialMemory = process.memoryUsage();

    // Simulate rapid query start/close cycles (stress test for query promise map)
    for (let i = 0; i < 500; i++) {
      const queryId = `test_query_${i}`;

      // Start query
      await prologInterface.sendCommand(
        `query_start(member(X, [a,b,c]), ${queryId}).`,
        5000
      );

      // Close query immediately
      await prologInterface.sendCommand(
        `query_close(${queryId}).`,
        5000
      );
    }

    if (global.gc) {
      global.gc();
    }

    const finalMemory = process.memoryUsage();
    const heapGrowth = finalMemory.heapUsed - initialMemory.heapUsed;
    const heapGrowthMB = heapGrowth / 1024 / 1024;

    console.log(`Heap growth after 500 start/close cycles: ${heapGrowthMB.toFixed(2)} MB`);
    expect(heapGrowthMB).toBeLessThan(5);
  }, 60000);

  it("should clean up after query timeouts", async () => {
    const initialMemory = process.memoryUsage();

    // Create queries that will timeout
    const timeoutPromises = [];
    for (let i = 0; i < 100; i++) {
      // Create a very short timeout that will likely fail
      const promise = prologInterface.sendCommand(
        `sleep(10), member(X, [1,2,3]).`,
        100 // Very short timeout
      ).catch(() => {
        // Expected to timeout, ignore errors
        return null;
      });
      timeoutPromises.push(promise);
    }

    await Promise.all(timeoutPromises);

    if (global.gc) {
      global.gc();
    }

    const finalMemory = process.memoryUsage();
    const heapGrowth = finalMemory.heapUsed - initialMemory.heapUsed;
    const heapGrowthMB = heapGrowth / 1024 / 1024;

    console.log(`Heap growth after 100 timeouts: ${heapGrowthMB.toFixed(2)} MB`);
    expect(heapGrowthMB).toBeLessThan(5);
  }, 30000);

  it("should handle alternating queries without accumulating state", async () => {
    const initialMemory = process.memoryUsage();

    // Alternate between different query types
    for (let i = 0; i < 200; i++) {
      if (i % 3 === 0) {
        await prologInterface.sendCommand(`member(X, [1,2,3]), query_next(), !.`, 5000);
      } else if (i % 3 === 1) {
        await prologInterface.sendCommand(`append([1], [2], X), query_next(), !.`, 5000);
      } else {
        await prologInterface.sendCommand(`length([a,b,c], X), query_next(), !.`, 5000);
      }
    }

    if (global.gc) {
      global.gc();
    }

    const finalMemory = process.memoryUsage();
    const heapGrowth = finalMemory.heapUsed - initialMemory.heapUsed;
    const heapGrowthMB = heapGrowth / 1024 / 1024;

    console.log(`Heap growth after 200 alternating queries: ${heapGrowthMB.toFixed(2)} MB`);
    expect(heapGrowthMB).toBeLessThan(8);
  }, 60000);
});
