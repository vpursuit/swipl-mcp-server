/**
 * Unsafe Goal Consistency Tests
 * Tests security validation for unsafe predicates
 *
 * TODO: Check if any API changes needed - appears to only use queryStart which remains
 * This file tests valid functionality and should be kept
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Unsafe goal consistency", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });
  afterEach(async () => {
    await prologInterface.stop();
  });

  test("call/1 should be rejected with unsafe_goal (not timeout)", async () => {
    await prologInterface.start();
    const res = await toolHandlers.query({ operation: "start", query: "call(true)" });
    expect(res.isError).toBe(true);
    expect(res.content[0].text).toMatch(/unsafe_goal|Error:/i);
  });
});
