import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "../../src/tools.js";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Unsafe File Consultation", () => {
  beforeEach(() => {
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  test("should detect and reject unsafe directives during file consultation", async () => {
    await prologInterface.start();

    // Try to consult the unsafe file that contains directives
    const result = await toolHandlers.dbLoad({ filename: "test/unsafe_test.pl" });

    // The consult "succeeds" at tool level but returns an error from Prolog
    expect(result.isError).toBeFalsy();
    expect(result.content[0].text).toMatch(/Successfully consulted file/);

    // But the Prolog result should show permission error for directives
    expect(result.content[0].text).toMatch(/permission_error.*directive/i);
    expect(result.content[0].text).toMatch(/Directives are not allowed/i);

    // Verify the structured content also shows the error
    expect(result.structuredContent.result).toMatch(/error.*permission_error.*directive/i);
  });

  test("should block critical blacklisted dangerous predicates", async () => {
    await prologInterface.start();

    // Test a key dangerous predicate from each category to ensure blacklist works
    // Use individual sessions to avoid process crashes affecting other tests

    // Test call/* (arbitrary code execution)
    const callResult = await toolHandlers.queryStart({ query: "call(true)" });
    expect(callResult.isError).toBeTruthy();
    expect(callResult.content[0].text).toMatch(/unsafe_goal|Error:/i);

    // Test assert/* (database modification) 
    const assertResult = await toolHandlers.queryStart({ query: "assert(test(fact))" });
    expect(assertResult.isError).toBeTruthy();
    expect(assertResult.content[0].text).toMatch(/unsafe_goal|Error:/i);

    // Test system/* (system calls)
    const systemResult = await toolHandlers.queryStart({ query: "system('echo test')" });
    expect(systemResult.isError).toBeTruthy();
    expect(systemResult.content[0].text).toMatch(/unsafe_goal|Error:/i);
  });

  test("should validate safe consultation works with clean files", async () => {
    await prologInterface.start();

    // Create a temporary safe file content and verify manual assertion works
    // (This tests that our security model allows safe recursive rules)
    await toolHandlers.dbAssert({ fact: "parent(alice, bob)" });
    await toolHandlers.dbAssert({ fact: "ancestor(X, Y) :- parent(X, Y)" });

    // Test that the safe recursive rule works
    const result = await toolHandlers.queryStart({ query: "ancestor(alice, bob)" });
    expect(result.isError).toBeFalsy();

    const solution = await toolHandlers.queryNext();
    expect(solution.isError).toBeFalsy();
    expect(solution.content[0].text).toMatch(/Solution:\s*true/);

    await toolHandlers.queryClose();
  });
});