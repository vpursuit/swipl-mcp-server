/**
 * Integration tests for Safe Library Module Loading
 * Tests library(clpfd) and other sandbox-safe libraries
 * Updated for Step 4: Uses unified files tool
 */
import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";
import os from "os";
import path from "path";
import { mkdirSync, existsSync, writeFileSync, unlinkSync } from "fs";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Safe Library Loading: library(clpfd)", () => {
  const homeDir = os.homedir();
  const allowedDir = path.join(homeDir, '.model-context-lab');
  const testFile = path.join(allowedDir, "test_clpfd.pl");
  let originalRoots: string | undefined;

  beforeEach(async () => {
    await prologInterface.stop();

    // Configure roots to allow file loading
    originalRoots = process.env.SWI_MCP_ALLOWED_ROOTS;
    process.env.SWI_MCP_ALLOWED_ROOTS = allowedDir;

    // Ensure directory exists
    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    // Restore original roots configuration
    if (originalRoots !== undefined) {
      process.env.SWI_MCP_ALLOWED_ROOTS = originalRoots;
    } else {
      delete process.env.SWI_MCP_ALLOWED_ROOTS;
    }

    // Clean up test file
    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should support clpfd constraints with pre-loaded library", async () => {
    // clpfd is pre-loaded by default - no directive needed
    const fileContent = `% Simple N-Queens constraint for N=4
queens_4(Qs) :-
    Qs = [Q1, Q2, Q3, Q4],
    Qs ins 1..4,
    all_distinct(Qs),
    safe_queens(Qs),
    label(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :- safe_queens(Qs, Q, 1), safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
    Q0 #\\= Q,
    abs(Q0 - Q) #\\= D0,
    D1 #= D0 + 1,
    safe_queens(Qs, Q0, D1).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });

    expect(loadResult.isError).toBeFalsy();
    expect(loadResult.content[0].text).toContain("Successfully imported file");

    // Now test that we can use clpfd constraints
    const queryResult = await toolHandlers.query({ operation: "start", query: "queens_4(Qs)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/Qs\s*=/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should support clpfd N-Queens constraint solving", async () => {
    // clpfd is pre-loaded by default - no directive needed
    const fileContent = `n_queens(N, Qs) :-
    length(Qs, N),
    Qs ins 1..N,
    all_distinct(Qs),
    safe_diagonal(Qs),
    label(Qs).

safe_diagonal([]).
safe_diagonal([Q|Qs]) :- safe_diagonal(Qs, Q, 1), safe_diagonal(Qs).

safe_diagonal([], _, _).
safe_diagonal([Q|Qs], Q0, D0) :-
    abs(Q0 - Q) #\\= D0,
    D1 #= D0 + 1,
    safe_diagonal(Qs, Q0, D1).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    // Test 4-Queens problem
    const queryResult = await toolHandlers.query({ operation: "start", query: "n_queens(4, Qs)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    // Should find a solution like [2,4,1,3] or [3,1,4,2]
    expect(nextResult.content[0].text).toMatch(/Qs\s*=\s*\[/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should support clpfd arithmetic constraints", async () => {
    // clpfd is pre-loaded by default - no directive needed
    const fileContent = `solve_puzzle(X, Y, Z) :-
    [X, Y, Z] ins 1..10,
    X + Y #= Z,
    X #< Y,
    label([X, Y, Z]).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({ operation: "start", query: "solve_puzzle(X, Y, Z)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    // Should find solution like X=1, Y=2, Z=3
    expect(nextResult.content[0].text).toMatch(/X\s*=\s*\d+/);
    expect(nextResult.content[0].text).toMatch(/Y\s*=\s*\d+/);
    expect(nextResult.content[0].text).toMatch(/Z\s*=\s*\d+/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should support clpfd all_distinct constraint", async () => {
    // clpfd is pre-loaded by default - no directive needed
    const fileContent = `unique_values(List) :-
    List ins 1..5,
    all_distinct(List),
    label(List).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({
      operation: "start",
      query: "unique_values([A,B,C]), A #= 1"
    });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    // Should find A=1, B and C different values from 2..5
    expect(nextResult.content[0].text).toMatch(/A\s*=\s*1/);

    await toolHandlers.query({ operation: "close" });
  });
});

maybeDescribe("Safe Library Loading: Other Safe Libraries", () => {
  const homeDir = os.homedir();
  const allowedDir = path.join(homeDir, '.model-context-lab');
  const testFile = path.join(allowedDir, "test_safe_libs.pl");
  let originalRoots: string | undefined;

  beforeEach(async () => {
    await prologInterface.stop();

    // Configure roots to allow file loading
    originalRoots = process.env.SWI_MCP_ALLOWED_ROOTS;
    process.env.SWI_MCP_ALLOWED_ROOTS = allowedDir;

    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    // Restore original roots configuration
    if (originalRoots !== undefined) {
      process.env.SWI_MCP_ALLOWED_ROOTS = originalRoots;
    } else {
      delete process.env.SWI_MCP_ALLOWED_ROOTS;
    }

    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should support library(lists) predicates - pre-loaded", async () => {
    // library(lists) is pre-loaded by default - no directive needed
    const fileContent = `test_lists(Result) :-
    append([1,2], [3,4], Result).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({ operation: "start", query: "test_lists(R)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/R\s*=\s*\[1,\s*2,\s*3,\s*4\]/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should support library(apply) maplist - pre-loaded", async () => {
    // library(apply) is pre-loaded by default - no directive needed
    const fileContent = `double(X, Y) :- Y is X * 2.

test_maplist(Input, Output) :-
    maplist(double, Input, Output).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({ operation: "start", query: "test_maplist([1,2,3], R)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/R\s*=\s*\[2,\s*4,\s*6\]/);

    await toolHandlers.query({ operation: "close" });
  });
});

maybeDescribe("Edge Cases: Library Loading", () => {
  const homeDir = os.homedir();
  const allowedDir = path.join(homeDir, '.model-context-lab');
  const testFile = path.join(allowedDir, "test_edge_cases.pl");
  let originalRoots: string | undefined;

  beforeEach(async () => {
    await prologInterface.stop();

    // Configure roots to allow file loading
    originalRoots = process.env.SWI_MCP_ALLOWED_ROOTS;
    process.env.SWI_MCP_ALLOWED_ROOTS = allowedDir;

    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    // Restore original roots configuration
    if (originalRoots !== undefined) {
      process.env.SWI_MCP_ALLOWED_ROOTS = originalRoots;
    } else {
      delete process.env.SWI_MCP_ALLOWED_ROOTS;
    }

    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should support combined use of pre-loaded libraries", async () => {
    // libraries (lists, apply, clpfd) are all pre-loaded by default - no directives needed
    const fileContent = `% Helper predicate to add 1
add_one(X, Y) :- Y is X + 1.

test_combined(Result) :-
    maplist(add_one, [1,2,3], Tmp),
    append(Tmp, [10], Result).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.query({ operation: "start", query: "test_combined(R)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.query({ operation: "next" });
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/R\s*=\s*\[2,\s*3,\s*4,\s*10\]/);

    await toolHandlers.query({ operation: "close" });
  });

  test("should maintain security with pre-loaded libraries", async () => {
    // clpfd is pre-loaded by default - no directive needed
    const fileContent = `test_safe :- X #> 0.
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.files({ operation: "import", filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    // Verify dangerous operations still blocked
    const dangerousQuery = await toolHandlers.query({ operation: "start", query: "shell('ls')" });
    expect(dangerousQuery.isError).toBeTruthy();
    expect(dangerousQuery.content[0].text).toMatch(/unsafe_goal|permission_error|dangerous predicate|Security Error/i);
  });
});
