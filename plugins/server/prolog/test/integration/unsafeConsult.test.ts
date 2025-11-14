/**
 * Unsafe File Consultation Tests
 * Tests security validation during file loading
 * Updated for Step 4: Uses unified files tool
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";
import { findNearestFile } from "@vpursuit/mcp-server-core";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Unsafe File Consultation", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  test("should detect and reject unsafe directives during file consultation", async () => {
    await prologInterface.start();

    // Find the unsafe test fixture using core path utilities
    const unsafeFixture = findNearestFile("unsafe_test.pl", { customSubdirs: ["test/fixtures", "plugins/server/prolog/test/fixtures"] });
    if (!unsafeFixture) {
      throw new Error("Test fixture unsafe_test.pl not found");
    }

    // Try to consult the unsafe file that contains directives - should be blocked by Prolog
    const result = await toolHandlers.files({ operation: "import", filename: unsafeFixture });

    // The consult should be blocked by Prolog's security validation
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Error");
    // The error should mention either permission denied or directives not allowed
    const errorText = result.content[0].text;
    const hasSecurityError = errorText.includes("permission") || 
                           errorText.includes("directive") || 
                           errorText.includes("Security Error");
    expect(hasSecurityError).toBeTruthy();
  });

  test("should block critical blacklisted dangerous predicates", async () => {
    await prologInterface.start();

    // Test a key dangerous predicate from each category to ensure blacklist works
    // Use individual sessions to avoid process crashes affecting other tests

    // Test call/* (arbitrary code execution)
    const callResult = await toolHandlers.query({ operation: "start", query: "call(true)" });
    expect(callResult.isError).toBeTruthy();
    expect(callResult.content[0].text).toMatch(/unsafe_goal|Error:/i);

    // Test assert/* (database modification) 
    const assertResult = await toolHandlers.query({ operation: "start", query: "assert(test(fact))" });
    expect(assertResult.isError).toBeTruthy();
    expect(assertResult.content[0].text).toMatch(/unsafe_goal|Error:/i);

    // Test system/* (system calls)
    const systemResult = await toolHandlers.query({ operation: "start", query: "system('echo test')" });
    expect(systemResult.isError).toBeTruthy();
    expect(systemResult.content[0].text).toMatch(/unsafe_goal|Error:/i);
  });

  test("should validate safe consultation works with clean files", async () => {
    await prologInterface.start();

    // Create a temporary safe file content and verify manual assertion works
    // (This tests that our security model allows safe recursive rules)
    await toolHandlers.clauses({ operation: "assert", clauses: "parent(alice, bob)" });
    await toolHandlers.clauses({ operation: "assert", clauses: "ancestor(X, Y) :- parent(X, Y)" });

    // Test that the safe recursive rule works
    const result = await toolHandlers.query({ operation: "start", query: "ancestor(alice, bob)" });
    expect(result.isError).toBeFalsy();

    const solution = await toolHandlers.query({ operation: "next" });
    expect(solution.isError).toBeFalsy();
    expect(solution.content[0].text).toMatch(/Solution:\s*true/);

    await toolHandlers.query({ operation: "close" });
  });
});