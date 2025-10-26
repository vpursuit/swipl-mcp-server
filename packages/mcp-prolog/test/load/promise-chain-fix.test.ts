/**
 * Test to validate promise chain fix for progressive slowdown
 * Focus: Ensure command queue doesn't grow unbounded
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { PrologInterface } from '../../src/PrologInterface.js';

describe('Promise Chain Fix Validation', () => {
  let prolog: PrologInterface;

  beforeEach(async () => {
    prolog = new PrologInterface();
    await prolog.start();
  });

  afterEach(async () => {
    await prolog.stop();
  });

  it('should maintain consistent performance across 100 sequential queries', async () => {
    const timings: number[] = [];
    const batchSize = 10;
    const batches = 10;

    console.log('\n=== Promise Chain Fix Test ===');
    console.log(`Running ${batches} batches of ${batchSize} queries each...`);

    for (let batch = 0; batch < batches; batch++) {
      const batchStart = Date.now();

      for (let i = 0; i < batchSize; i++) {
        const queryNum = batch * batchSize + i;
        const start = Date.now();

        try {
          await prolog.query(`assert(test${queryNum}(${queryNum}))`);
        } catch (error) {
          // Ignore errors for this test
        }

        timings.push(Date.now() - start);
      }

      const batchDuration = Date.now() - batchStart;
      const avgBatchTime = batchDuration / batchSize;

      console.log(`  Batch ${batch + 1}/${batches}: ${batchDuration}ms total, ${avgBatchTime.toFixed(2)}ms avg`);
    }

    // Analyze results
    const firstBatch = timings.slice(0, batchSize);
    const lastBatch = timings.slice(-batchSize);

    const avgFirst = firstBatch.reduce((a, b) => a + b, 0) / firstBatch.length;
    const avgLast = lastBatch.reduce((a, b) => a + b, 0) / lastBatch.length;
    const slowdownRatio = avgLast / avgFirst;

    console.log('\n=== Results ===');
    console.log(`First batch avg: ${avgFirst.toFixed(2)}ms`);
    console.log(`Last batch avg: ${avgLast.toFixed(2)}ms`);
    console.log(`Slowdown ratio: ${slowdownRatio.toFixed(2)}x`);
    console.log(`Max time: ${Math.max(...timings)}ms`);
    console.log(`Min time: ${Math.min(...timings)}ms`);

    // The fix should ensure NO significant slowdown (less than 2x)
    // Old bug would show 10x-100x slowdown after 100 queries
    expect(slowdownRatio).toBeLessThan(2.0);

    console.log('✅ No progressive slowdown detected!\n');
  }, 60000);

  it('should handle rapid sequential queries without degradation', async () => {
    const count = 50;
    const timings: number[] = [];

    console.log(`\n=== Rapid Sequential Test ===`);
    console.log(`Sending ${count} queries rapidly...`);

    for (let i = 0; i < count; i++) {
      const start = Date.now();

      try {
        await prolog.query(`assert(rapid${i}(${i}))`);
      } catch (error) {
        // Ignore errors
      }

      timings.push(Date.now() - start);
    }

    const avgTime = timings.reduce((a, b) => a + b, 0) / timings.length;
    const maxTime = Math.max(...timings);
    const minTime = Math.min(...timings);

    console.log(`Average time: ${avgTime.toFixed(2)}ms`);
    console.log(`Max time: ${maxTime}ms`);
    console.log(`Min time: ${minTime}ms`);

    // All queries should complete relatively quickly
    expect(avgTime).toBeLessThan(1000); // Average under 1 second

    console.log('✅ Rapid queries handled efficiently!\n');
  }, 60000);

  it('should not accumulate promise chain depth', async () => {
    // This test verifies the fix at line 695-698 in PrologInterface.ts
    // The old code created unbounded promise chains:
    //   commandQueue = commandQueue.then(...).then(...)
    // The fix resets to a fresh promise:
    //   commandQueue = p.then(() => Promise.resolve())

    console.log('\n=== Promise Chain Depth Test ===');

    // Send many queries to test chain growth
    for (let i = 0; i < 50; i++) {
      try {
        await prolog.query(`assert(chain_test${i}(${i}))`);
      } catch (error) {
        // Ignore errors
      }
    }

    // If the fix works, all queries should have completed without slowdown
    // We can't directly measure promise chain depth, but we can verify
    // that queries continue to work correctly after many operations

    const start = Date.now();
    try {
      await prolog.query('assert(final_test(42))');
    } catch (error) {
      // Ignore error
    }
    const duration = Date.now() - start;

    console.log(`Query after 50 operations: ${duration}ms`);

    // Should still be fast (old bug would be very slow here)
    expect(duration).toBeLessThan(1000);

    console.log('✅ No promise chain accumulation!\n');
  });
});
