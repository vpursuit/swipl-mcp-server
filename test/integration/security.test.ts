import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "../../src/tools.js";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Security: block non-whitelisted system predicates", () => {
  beforeEach(() => {
    prologInterface.stop();
  });
  afterEach(() => {
    prologInterface.stop();
  });

  test("directory_files/2 is rejected as unsafe", async () => {
    await prologInterface.start();
    const started = await toolHandlers.queryStart({ query: "directory_files('.', L)" });
    // Expect the start to fail or the next to report an unsafe goal
    if (started.isError) {
      expect(started.content[0].text).toMatch(/unsafe_goal|Error:/i);
      return;
    }
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBe(true);
    expect(res.content[0].text).toMatch(/unsafe_goal|permission_error|Error:/i);
  });

  test("member/2 remains allowed (whitelisted)", async () => {
    await prologInterface.start();
    const started = await toolHandlers.queryStart({ query: "member(X, [a,b])" });
    expect(started.isError).toBeFalsy();
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    expect(res.content[0].text).toMatch(/X\s*=\s*(a|b)/);
    await toolHandlers.queryClose();
  });
});
