import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "../../src/tools.js";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Unsafe goal consistency", () => {
  beforeEach(() => {
    prologInterface.stop();
  });
  afterEach(() => {
    prologInterface.stop();
  });

  test("call/1 should be rejected with unsafe_goal (not timeout)", async () => {
    await prologInterface.start();
    const res = await toolHandlers.queryStart({ query: "call(true)" });
    expect(res.isError).toBe(true);
    expect(res.content[0].text).toMatch(/unsafe_goal|Error:/i);
  });
});
