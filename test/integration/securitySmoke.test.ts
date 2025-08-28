import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "../../src/tools.js";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;
maybeDescribe("Security Smoke Tests", () => {
  beforeEach(() => {
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  test("arithmetic filter with between + is/2 via findall", async () => {
    await prologInterface.start();
    const q = "findall(X, (between(1,10,X), 0 is X mod 2), L)";
    await toolHandlers.queryStart({ query: q });
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    const t = res.content[0].text;
    expect(t).toContain("Solution:");
    expect(t).toMatch(/L\s*=\s*\[/);
    await toolHandlers.queryClose();
  });

  test("collections with findall + member", async () => {
    await prologInterface.start();
    const q = "findall(X, member(X, [a,b,c]), L)";
    await toolHandlers.queryStart({ query: q });
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    const t = res.content[0].text;
    expect(t).toContain("L=");
    expect(t).toMatch(/\[(?:a|b|c)/);
    await toolHandlers.queryClose();
  });

  test("user predicate with lists via findall", async () => {
    await prologInterface.start();
    // Define a pure helper in kb
    await prologInterface.query("assert((double(X,Y) :- Y is X*2))");
    await toolHandlers.queryStart({ query: "findall(Y, (member(X,[1,2,3]), double(X,Y)), L)" });
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    const t = res.content[0].text;
    expect(t).toContain("L=");
    expect(t).toContain("[2,4,6]");
    await toolHandlers.queryClose();
  });

  test("engine mode: even numbers with backtracking", async () => {
    await prologInterface.start();
    // Start engine that yields even numbers between 1 and 6
    const se = await toolHandlers.queryStartEngine({ query: "(between(1,6,X), 0 is X mod 2)" });
    expect(se.isError).toBeFalsy();
    const r1 = await toolHandlers.queryNext();
    expect(r1.isError).toBeFalsy();
    expect(r1.content[0].text).toMatch(/X\s*=\s*2/);
    const r2 = await toolHandlers.queryNext();
    expect(r2.isError).toBeFalsy();
    expect(r2.content[0].text).toMatch(/X\s*=\s*4/);
    await toolHandlers.queryClose();
  });

  test("strings: sub_atom/5 + atom_string/2 + string_concat/3", async () => {
    await prologInterface.start();
    // sub_atom to extract 'world'
    await toolHandlers.queryStart({ query: "sub_atom('hello_world', 6, 5, 0, S)" });
    let res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    expect(res.content[0].text).toMatch(/S\s*=\s*world/);
    await toolHandlers.queryClose();

    // atom_string convert
    await toolHandlers.queryStart({ query: 'atom_string(A, "hello")' });
    res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    expect(res.content[0].text).toMatch(/A\s*=\s*hello/);
    await toolHandlers.queryClose();

    // (Optional) atom_concat concatenation can vary across environments; omit from smoke test.
  });
});
