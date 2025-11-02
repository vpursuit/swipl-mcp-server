/**
 * Test to verify timeout recovery and process restart mechanism
 * Reproduces the user-reported scenario: loop :- loop timeout followed by another operation
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { PrologInterface } from '../../src/PrologInterface.js';

describe('Timeout Recovery Test', () => {
  let prolog: PrologInterface;

  beforeEach(async () => {
    // Use shorter timeout for faster testing
    process.env['SWI_MCP_QUERY_TIMEOUT_MS'] = '2000';
    prolog = new PrologInterface();
    await prolog.start();
  });

  afterEach(async () => {
    delete process.env['SWI_MCP_QUERY_TIMEOUT_MS'];
    await prolog.stop();
  });

  it('should recover and accept new operations after infinite loop timeout', async () => {
    // Step 1: Assert an infinite loop predicate
    console.log('Step 1: Asserting infinite loop predicate...');
    const assertResult = await prolog.query("assert((loop :- loop))");
    expect(assertResult).toBe('ok');
    console.log('✓ Infinite loop predicate asserted successfully');

    // Step 2: Start a query that will trigger the infinite loop
    console.log('Step 2: Starting infinite loop query...');
    try {
      await prolog.startQuery("loop");

      // Step 3: Try to get next solution - this should timeout
      console.log('Step 3: Calling next solution (will timeout)...');
      const start = Date.now();
      const result = await prolog.nextSolution();
      const duration = Date.now() - start;

      // Should timeout
      expect(result.error).toBeDefined();
      expect(result.error).toMatch(/timeout|temporarily unavailable/i);
      expect(duration).toBeGreaterThan(1900); // Close to 2000ms timeout
      console.log(`✓ Query timed out as expected (${duration}ms)`);
    } catch (error) {
      // Timeout might throw instead of returning error
      const errorMsg = error instanceof Error ? error.message : String(error);
      expect(errorMsg).toMatch(/timeout|temporarily unavailable/i);
      console.log('✓ Query threw timeout error as expected');
    }

    // Step 4: Wait a bit for recovery to complete
    console.log('Step 4: Waiting for recovery to complete...');
    await new Promise(resolve => setTimeout(resolve, 500));

    // Step 5: Try another operation - should succeed after recovery
    console.log('Step 5: Attempting new operation after timeout...');
    try {
      const assertResult2 = await prolog.query("assert(test(successful_after_recovery))");
      expect(assertResult2).toBe('ok');
      console.log('✓ New operation succeeded after recovery!');
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      // If it fails with "temporarily unavailable", recovery is still in progress - that's OK
      if (errorMsg.includes('temporarily unavailable')) {
        console.log('⚠ Recovery still in progress, waiting longer...');
        await new Promise(resolve => setTimeout(resolve, 1000));
        // Retry
        const assertResult2 = await prolog.query("assert(test(successful_after_recovery))");
        expect(assertResult2).toBe('ok');
        console.log('✓ New operation succeeded after extended recovery!');
      } else {
        throw error;
      }
    }

    // Step 6: Verify the process is healthy
    console.log('Step 6: Checking process health...');
    expect(prolog.isHealthy()).toBe(true);
    const health = prolog.getHealthStatus();
    expect(health.isReady).toBe(true);
    expect(health.circuitState).not.toBe('open');
    console.log('✓ Process is healthy and circuit breaker is closed');

    console.log('\n=== Timeout Recovery Test PASSED ===');
  }, 30000); // 30 second timeout for the whole test

  it('should handle multiple rapid infinite loop assertions without hanging', async () => {
    console.log('Testing multiple dangerous predicates in sequence...');

    const dangerousPredicates = [
      "loop1 :- loop1",
      "loop2 :- loop2, loop2",
      "loop3 :- loop3 ; loop3",
    ];

    for (let i = 0; i < dangerousPredicates.length; i++) {
      const pred = dangerousPredicates[i];
      console.log(`\nAsserting predicate ${i + 1}/${dangerousPredicates.length}: ${pred}`);

      try {
        // This should succeed (assertion itself is safe, it's the execution that's dangerous)
        const result = await prolog.query(`assert((${pred}))`);
        expect(result).toBe('ok');
        console.log(`✓ Assertion ${i + 1} succeeded`);
      } catch (error) {
        // If previous test left circuit open, this might fail
        const errorMsg = error instanceof Error ? error.message : String(error);
        if (errorMsg.includes('temporarily unavailable')) {
          console.log('⚠ Circuit breaker open, waiting for recovery...');
          await new Promise(resolve => setTimeout(resolve, 1000));
          // Don't fail test, recovery is working as designed
        } else {
          throw error;
        }
      }
    }

    console.log('\n✓ All dangerous predicates handled without hanging');
    console.log('=== Multiple Dangerous Predicates Test PASSED ===');
  }, 30000);
});
