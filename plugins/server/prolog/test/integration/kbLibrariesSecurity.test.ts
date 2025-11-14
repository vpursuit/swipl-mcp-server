/**
 * KB_LIBRARIES Environment Variable Security Tests
 * Tests that KB_LIBRARIES whitelist enforcement works correctly
 * Ensures dangerous libraries cannot be loaded via environment configuration
 */
import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("KB_LIBRARIES Security: Safe Library Loading", () => {
  let originalLibs: string | undefined;

  beforeEach(async () => {
    await prologInterface.stop();
    originalLibs = process.env.KB_LIBRARIES;
  });

  afterEach(async () => {
    await prologInterface.stop();
    if (originalLibs !== undefined) {
      process.env.KB_LIBRARIES = originalLibs;
    } else {
      delete process.env.KB_LIBRARIES;
    }
  });

  test("should load library(aggregate) when specified in KB_LIBRARIES", async () => {
    process.env.KB_LIBRARIES = "aggregate";
    await prologInterface.start();

    // Try to use aggregate_all/3 which requires library(aggregate)
    const result = await toolHandlers.clauses({
      operation: "assert",
      clauses: ["fact(a, 1).", "fact(b, 2).", "fact(c, 3).", "test_sum(Sum) :- aggregate_all(sum(X), fact(_, X), Sum)."]
    });

    expect(result.isError).toBeFalsy();

    // Verify the aggregate predicate works
    const queryResult = await toolHandlers.query({ operation: "start", query: "test_sum(S)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/S\s*=\s*6/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should load library(assoc) when specified in KB_LIBRARIES", async () => {
    process.env.KB_LIBRARIES = "assoc";
    await prologInterface.start();

    // Try to use assoc predicates
    const result = await toolHandlers.clauses({
      operation: "assert",
      clauses: "test_assoc(Value) :- empty_assoc(A0), put_assoc(key1, A0, value1, A1), get_assoc(key1, A1, Value)."
    });

    expect(result.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({ operation: "start", query: "test_assoc(V)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/V\s*=\s*value1/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should load multiple libraries when comma-separated in KB_LIBRARIES", async () => {
    process.env.KB_LIBRARIES = "aggregate,assoc,pairs";
    await prologInterface.start();

    // All three libraries should be available
    const result = await toolHandlers.clauses({
      operation: "assert",
      clauses: ["num(1).", "num(2).", "test_all(Sum) :- aggregate_all(sum(X), num(X), Sum)."]
    });

    expect(result.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({ operation: "start", query: "test_all(S)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/S\s*=\s*3/);

    await toolHandlers.query({ operation: "close" });
  });
});

maybeDescribe("KB_LIBRARIES Security: Whitelist Enforcement", () => {
  let originalLibs: string | undefined;

  beforeEach(async () => {
    await prologInterface.stop();
    originalLibs = process.env.KB_LIBRARIES;
  });

  afterEach(async () => {
    await prologInterface.stop();
    if (originalLibs !== undefined) {
      process.env.KB_LIBRARIES = originalLibs;
    } else {
      delete process.env.KB_LIBRARIES;
    }
  });

  test("should block library(process) even when specified in KB_LIBRARIES", async () => {
    // Attempt to load dangerous library via env var
    process.env.KB_LIBRARIES = "process";
    await prologInterface.start();

    // process_create should not be available (library not loaded)
    const result = await toolHandlers.query({
      operation: "start",
      query: "current_predicate(process_create/3)"
    });

    // Should fail (no solutions) - process library should not be loaded
    expect(result.isError).toBeFalsy(); // Query succeeds
    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.content[0].text).toMatch(/No more solutions|false/i); // But predicate doesn't exist

    await toolHandlers.query({ operation: "close" });
  });

  test("should block library(filesex) even when specified in KB_LIBRARIES", async () => {
    process.env.KB_LIBRARIES = "filesex";
    await prologInterface.start();

    // make_directory_path should not be available
    const result = await toolHandlers.query({
      operation: "start",
      query: "current_predicate(make_directory_path/1)"
    });

    expect(result.isError).toBeFalsy();
    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.content[0].text).toMatch(/No more solutions|false/i);

    await toolHandlers.query({ operation: "close" });
  });

  test("should block library(http/http_open) even when specified in KB_LIBRARIES", async () => {
    process.env.KB_LIBRARIES = "http/http_open";
    await prologInterface.start();

    // http_open should not be available
    const result = await toolHandlers.query({
      operation: "start",
      query: "current_predicate(http_open/3)"
    });

    expect(result.isError).toBeFalsy();
    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.content[0].text).toMatch(/No more solutions|false/i);

    await toolHandlers.query({ operation: "close" });
  });

  test("should block library(shell) even when specified in KB_LIBRARIES", async () => {
    process.env.KB_LIBRARIES = "shell";
    await prologInterface.start();

    // shell should not be available
    const result = await toolHandlers.query({
      operation: "start",
      query: "shell('echo test')"
    });

    // Should be blocked by dangerous predicate validation
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toMatch(/unsafe_goal|dangerous predicate|Security Error/i);
  });
});

maybeDescribe("KB_LIBRARIES Security: Edge Cases", () => {
  let originalLibs: string | undefined;

  beforeEach(async () => {
    await prologInterface.stop();
    originalLibs = process.env.KB_LIBRARIES;
  });

  afterEach(async () => {
    await prologInterface.stop();
    if (originalLibs !== undefined) {
      process.env.KB_LIBRARIES = originalLibs;
    } else {
      delete process.env.KB_LIBRARIES;
    }
  });

  test("should handle non-existent library gracefully", async () => {
    process.env.KB_LIBRARIES = "nonexistent_library_xyz";

    // Server should start successfully (just skip invalid library)
    await expect(prologInterface.start()).resolves.not.toThrow();

    // Server should still be functional
    const result = await toolHandlers.query({ operation: "start", query: "member(X, [1,2,3])" });
    expect(result.isError).toBeFalsy();

    await toolHandlers.query({ operation: "close" });
  });

  test("should load safe libraries and skip unsafe ones in mixed list", async () => {
    process.env.KB_LIBRARIES = "aggregate,process,assoc,filesex";
    await prologInterface.start();

    // aggregate should be loaded (safe)
    const result1 = await toolHandlers.clauses({
      operation: "assert",
      clauses: ["val(5).", "test_agg(Sum) :- aggregate_all(sum(X), val(X), Sum)."]
    });
    expect(result1.isError).toBeFalsy();

    // process should NOT be loaded (unsafe)
    const result2 = await toolHandlers.query({
      operation: "start",
      query: "current_predicate(process_create/3)"
    });
    expect(result2.isError).toBeFalsy();
    const nextResult2 = await toolHandlers.query({ operation: "next" });
    expect(nextResult2.content[0].text).toMatch(/No more solutions|false/i);

    await toolHandlers.query({ operation: "close" });
  });

  test("should verify whitelist cannot be bypassed", async () => {
    // Try various bypass attempts
    const bypassAttempts = [
      "process",
      "../process",
      "library(process)",
      "process/create"
    ];

    for (const attempt of bypassAttempts) {
      process.env.KB_LIBRARIES = attempt;
      await prologInterface.stop();
      await prologInterface.start();

      // process_create should never be available
      const result = await toolHandlers.query({
        operation: "start",
        query: "current_predicate(process_create/3)"
      });
      expect(result.isError).toBeFalsy();
      const nextResult = await toolHandlers.query({ operation: "next" });
      expect(nextResult.content[0].text).toMatch(/No more solutions|false/i);
      await toolHandlers.query({ operation: "close" });
    }
  });
});
