/**
 * Ancestor Rules Test
 * Tests recursive rule handling and query functionality
 *
 * TODO (Step 3): Update to use clauses tool instead of knowledgeBaseAssert
 * This file tests valid functionality and should be kept, but API will change
 */

import { describe, beforeEach, afterEach, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Ancestor Rules Test", () => {

  beforeEach(async () => {
    // Use normal timeout for this test
    delete process.env.SWI_MCP_QUERY_TIMEOUT_MS;
    await prologInterface.stop();
    await prologInterface.start();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  it("should handle complete ancestor rules correctly", async () => {
    const startTime = Date.now();

    // Add parent facts
    await toolHandlers.clauses({ operation: "assert", clauses: "parent(tom, bob)" });
    await toolHandlers.clauses({ operation: "assert", clauses: "parent(bob, alice)" });
    await toolHandlers.clauses({ operation: "assert", clauses: "parent(alice, charlie)" });

    // Add the complete ancestor rules (base case + recursive case)
    await toolHandlers.clauses({ operation: "assert", clauses: "ancestor(X, Y) :- parent(X, Y)" });
    await toolHandlers.clauses({ operation: "assert", clauses: "ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)" });

    // Test direct parent relationship (ground query - should succeed)
    let result = await toolHandlers.query({ operation: "start", query: "ancestor(tom, bob)" });
    expect(result.isError).toBeFalsy();
    result = await toolHandlers.query({ operation: "next" });
    expect(result.isError).toBeFalsy();
    expect(result.content[0].text).toMatch(/Solution:\s*true/);
    await toolHandlers.query({ operation: "close" });

    // Test transitive relationship (grandparent - ground query)
    result = await toolHandlers.query({ operation: "start", query: "ancestor(tom, alice)" });
    expect(result.isError).toBeFalsy();
    result = await toolHandlers.query({ operation: "next" });
    expect(result.isError).toBeFalsy();
    expect(result.content[0].text).toMatch(/Solution:\s*true/);
    await toolHandlers.query({ operation: "close" });

    // Test longer chain (great-grandparent - ground query)
    result = await toolHandlers.query({ operation: "start", query: "ancestor(tom, charlie)" });
    expect(result.isError).toBeFalsy();
    result = await toolHandlers.query({ operation: "next" });
    expect(result.isError).toBeFalsy();
    expect(result.content[0].text).toMatch(/Solution:\s*true/);
    await toolHandlers.query({ operation: "close" });

    // Test finding all ancestors of charlie
    result = await toolHandlers.query({ operation: "start", query: "ancestor(X, charlie)" });
    expect(result.isError).toBeFalsy();

    // Collect all solutions
    const ancestors = [];
    let currentResult = await toolHandlers.query({ operation: "next" });
    while (!currentResult.isError && currentResult.content[0].text.includes("Solution:")) {
      // Extract X value from solution text like "Solution: X = alice, more solutions: true"
      const match = currentResult.content[0].text.match(/X\s*=\s*(\w+)/);
      if (match) {
        ancestors.push(match[1]);
      }
      try {
        currentResult = await toolHandlers.query({ operation: "next" });
      } catch {
        break;
      }
    }
    await toolHandlers.query({ operation: "close" });

    // Should find alice, bob, and tom as ancestors of charlie
    expect(ancestors).toContain("alice");
    expect(ancestors).toContain("bob");
    expect(ancestors).toContain("tom");

    const elapsed = Date.now() - startTime;

    // Should complete quickly without timeout
    expect(elapsed).toBeLessThan(5000);
  });

  it("should handle ancestor queries without infinite loops", async () => {
    const startTime = Date.now();

    // Add parent facts
    await toolHandlers.clauses({ operation: "assert", clauses: "parent(john, mary)" });
    await toolHandlers.clauses({ operation: "assert", clauses: "parent(mary, susan)" });

    // Add the complete ancestor rules
    await toolHandlers.clauses({ operation: "assert", clauses: "ancestor(X, Y) :- parent(X, Y)" });
    await toolHandlers.clauses({ operation: "assert", clauses: "ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)" });

    // This query should find solutions without hanging
    let result = await toolHandlers.query({ operation: "start", query: "ancestor(john, susan)" });
    expect(result.isError).toBeFalsy();
    result = await toolHandlers.query({ operation: "next" });
    expect(result.isError).toBeFalsy();
    expect(result.content[0].text).toMatch(/Solution:/);
    await toolHandlers.query({ operation: "close" });

    // Test query that has no solution (should terminate quickly)
    result = await toolHandlers.query({ operation: "start", query: "ancestor(susan, john)" });
    expect(result.isError).toBeFalsy();
    result = await toolHandlers.query({ operation: "next" });
    expect(result.content[0].text).toMatch(/No more solutions/);
    await toolHandlers.query({ operation: "close" });

    const elapsed = Date.now() - startTime;

    // Should complete quickly
    expect(elapsed).toBeLessThan(2000);
  });
});