/**
 * Integration tests for Safe Library Module Loading
 * Tests library(clpfd) and other sandbox-safe libraries
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

  beforeEach(async () => {
    await prologInterface.stop();

    // Ensure directory exists
    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    // Clean up test file
    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should allow loading library(clpfd) via use_module directive", async () => {
    // Create a file with use_module(library(clpfd)) directive
    const fileContent = `:- use_module(library(clpfd)).

% Simple N-Queens constraint for N=4
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
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    expect(loadResult.isError).toBeFalsy();
    expect(loadResult.content[0].text).toContain("Successfully consulted file");

    // Now test that we can use clpfd constraints
    const queryResult = await toolHandlers.queryStart({ query: "queens_4(Qs)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/Qs\s*=/);

    await toolHandlers.queryClose();
  });

  test("should allow library(clpfd) constraint solving - N-Queens example", async () => {
    const fileContent = `:- use_module(library(clpfd)).

n_queens(N, Qs) :-
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
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    // Test 4-Queens problem
    const queryResult = await toolHandlers.queryStart({ query: "n_queens(4, Qs)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    // Should find a solution like [2,4,1,3] or [3,1,4,2]
    expect(nextResult.content[0].text).toMatch(/Qs\s*=\s*\[/);

    await toolHandlers.queryClose();
  });

  test("should allow library(clpfd) arithmetic constraints", async () => {
    const fileContent = `:- use_module(library(clpfd)).

solve_puzzle(X, Y, Z) :-
    [X, Y, Z] ins 1..10,
    X + Y #= Z,
    X #< Y,
    label([X, Y, Z]).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({ query: "solve_puzzle(X, Y, Z)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    // Should find solution like X=1, Y=2, Z=3
    expect(nextResult.content[0].text).toMatch(/X\s*=\s*\d+/);
    expect(nextResult.content[0].text).toMatch(/Y\s*=\s*\d+/);
    expect(nextResult.content[0].text).toMatch(/Z\s*=\s*\d+/);

    await toolHandlers.queryClose();
  });

  test("should allow library(clpfd) all_distinct constraint", async () => {
    const fileContent = `:- use_module(library(clpfd)).

unique_values(List) :-
    List ins 1..5,
    all_distinct(List),
    label(List).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({
      query: "unique_values([A,B,C]), A #= 1"
    });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    // Should find A=1, B and C different values from 2..5
    expect(nextResult.content[0].text).toMatch(/A\s*=\s*1/);

    await toolHandlers.queryClose();
  });
});

maybeDescribe("Safe Library Loading: Other Safe Libraries", () => {
  const homeDir = os.homedir();
  const allowedDir = path.join(homeDir, '.model-context-lab');
  const testFile = path.join(allowedDir, "test_safe_libs.pl");

  beforeEach(async () => {
    await prologInterface.stop();

    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should allow library(lists) - common list predicates", async () => {
    const fileContent = `:- use_module(library(lists)).

test_lists(Result) :-
    append([1,2], [3,4], Result).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({ query: "test_lists(R)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/R\s*=\s*\[1,\s*2,\s*3,\s*4\]/);

    await toolHandlers.queryClose();
  });

  test("should allow library(apply) - maplist and friends", async () => {
    const fileContent = `:- use_module(library(apply)).

double(X, Y) :- Y is X * 2.

test_maplist(Input, Output) :-
    maplist(double, Input, Output).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({ query: "test_maplist([1,2,3], R)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/R\s*=\s*\[2,\s*4,\s*6\]/);

    await toolHandlers.queryClose();
  });

  test("should allow library(aggregate) - aggregation operations", async () => {
    const fileContent = `:- use_module(library(aggregate)).

fact(a, 1).
fact(b, 2).
fact(c, 3).

test_sum(Sum) :-
    aggregate_all(sum(X), fact(_, X), Sum).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({ query: "test_sum(S)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/S\s*=\s*6/);

    await toolHandlers.queryClose();
  });

  test("should allow library(assoc) - association lists", async () => {
    const fileContent = `:- use_module(library(assoc)).

test_assoc(Value) :-
    empty_assoc(A0),
    put_assoc(key1, A0, value1, A1),
    put_assoc(key2, A1, value2, A2),
    get_assoc(key1, A2, Value).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({ query: "test_assoc(V)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/V\s*=\s*value1/);

    await toolHandlers.queryClose();
  });
});

maybeDescribe("Unsafe Library Blocking", () => {
  const homeDir = os.homedir();
  const allowedDir = path.join(homeDir, '.model-context-lab');
  const testFile = path.join(allowedDir, "test_unsafe_libs.pl");

  beforeEach(async () => {
    await prologInterface.stop();

    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should block library(process) - system process execution", async () => {
    const fileContent = `:- use_module(library(process)).

test_process :- process_create(path(ls), [], []).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    expect(loadResult.isError).toBeTruthy();
    expect(loadResult.content[0].text).toMatch(/permission_error|not approved|unsafe/i);
  });

  test("should block library(filesex) - extended file operations", async () => {
    const fileContent = `:- use_module(library(filesex)).

test_filesex :- make_directory_path('/tmp/test').
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    expect(loadResult.isError).toBeTruthy();
    expect(loadResult.content[0].text).toMatch(/permission_error|not approved|unsafe/i);
  });

  test("should block library(http/http_open) - network operations", async () => {
    const fileContent = `:- use_module(library(http/http_open)).

test_http :- http_open('http://example.com', _, []).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    expect(loadResult.isError).toBeTruthy();
    expect(loadResult.content[0].text).toMatch(/permission_error|not approved|unsafe/i);
  });

  test("should block use_module with non-library paths", async () => {
    const fileContent = `:- use_module('/tmp/malicious.pl').

test_rule :- true.
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    expect(loadResult.isError).toBeTruthy();
    expect(loadResult.content[0].text).toMatch(/permission_error|Only use_module\(library\(\.\.\.\)\)/i);
  });

  test("should block use_module with relative paths", async () => {
    const fileContent = `:- use_module('../other_module.pl').

test_rule :- true.
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    expect(loadResult.isError).toBeTruthy();
    expect(loadResult.content[0].text).toMatch(/permission_error|Only use_module\(library\(\.\.\.\)\)/i);
  });
});

maybeDescribe("Edge Cases: Library Loading", () => {
  const homeDir = os.homedir();
  const allowedDir = path.join(homeDir, '.model-context-lab');
  const testFile = path.join(allowedDir, "test_edge_cases.pl");

  beforeEach(async () => {
    await prologInterface.stop();

    if (!existsSync(allowedDir)) {
      mkdirSync(allowedDir, { recursive: true });
    }
  });

  afterEach(async () => {
    await prologInterface.stop();

    try {
      if (existsSync(testFile)) {
        unlinkSync(testFile);
      }
    } catch (e) {
      // Ignore cleanup errors
    }
  });

  test("should handle non-existent library gracefully", async () => {
    const fileContent = `:- use_module(library(nonexistent_library_xyz)).

test_rule :- true.
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });

    // Should fail with existence error or permission error
    expect(loadResult.isError).toBeTruthy();
    expect(loadResult.content[0].text).toMatch(/permission_error|existence_error|not approved/i);
  });

  test("should allow multiple safe library imports", async () => {
    const fileContent = `:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).

% Helper predicate to add 1
add_one(X, Y) :- Y is X + 1.

test_combined(Result) :-
    maplist(add_one, [1,2,3], Tmp),
    append(Tmp, [10], Result).
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    const queryResult = await toolHandlers.queryStart({ query: "test_combined(R)" });
    expect(queryResult.isError).toBeFalsy();

    const nextResult = await toolHandlers.queryNext();
    expect(nextResult.isError).toBeFalsy();
    expect(nextResult.content[0].text).toMatch(/R\s*=\s*\[2,\s*3,\s*4,\s*10\]/);

    await toolHandlers.queryClose();
  });

  test("should maintain security after loading safe libraries", async () => {
    const fileContent = `:- use_module(library(clpfd)).

test_safe :- X #> 0.
`;

    writeFileSync(testFile, fileContent);

    await prologInterface.start();
    const loadResult = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
    expect(loadResult.isError).toBeFalsy();

    // Verify dangerous operations still blocked
    const dangerousQuery = await toolHandlers.queryStart({ query: "shell('ls')" });
    expect(dangerousQuery.isError).toBeTruthy();
    expect(dangerousQuery.content[0].text).toMatch(/unsafe_goal|permission_error|dangerous predicate|Security Error/i);
  });
});
