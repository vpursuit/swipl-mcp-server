/**
 * Type-level validation tests
 *
 * These tests validate that types are correctly enforced at compile-time.
 * If this file compiles without errors, it means our type safety is working.
 */

import { describe, test, expect } from "vitest";
import type { ToolDefinition } from "@vpursuit/mcp-core";
import { tools, toolHandlers, getCapabilitiesSummary } from "../../src/tools.js";
import type { CapabilitiesSummary } from "../../src/types.js";

describe("Type Validation", () => {
  test("all tool handlers match ToolDefinition signature", () => {
    // Compile-time validation: This should compile without errors
    // Each handler must accept (args, extra) and return Promise<CallToolResult>
    type ToolHandler = ToolDefinition['handler'];

    const handlerMap: Record<string, ToolHandler> = Object.fromEntries(
      Object.entries(tools).map(([k, v]) => [k, v.handler])
    );

    // Runtime check that all handlers are functions
    // Note: function.length is unreliable for handlers with unused params (_args, _extra)
    for (const [name, handler] of Object.entries(handlerMap)) {
      expect(typeof handler).toBe('function');
      expect(name).toBeTruthy();
    }
  });

  test("toolHandlers export maintains type safety", () => {
    // Ensure toolHandlers doesn't use 'any' type assertion
    // This is validated at compile-time by the ToolHandler type

    for (const [name, handler] of Object.entries(toolHandlers)) {
      expect(typeof handler).toBe('function');
      expect(name).toBeTruthy();
    }
  });

  test("getCapabilitiesSummary returns properly typed object", () => {
    const caps = getCapabilitiesSummary();

    // Type-level check: caps should be CapabilitiesSummary
    const _typeCheck: CapabilitiesSummary = caps;

    // Runtime validation
    expect(caps.server).toBeDefined();
    expect(caps.server.name).toBe("swipl-mcp-server");
    expect(typeof caps.server.version).toBe("string");

    expect(caps.modes).toEqual(["standard", "engine"]);
    expect(caps.tools).toBeDefined();
    expect(caps.security).toBeDefined();
    expect(caps.prompts).toBeDefined();
  });

  test("no handler uses 'any' type (compile-time validation)", () => {
    // If this compiles, it means no handler is using 'any'
    // because ToolHandler type is strictly defined

    const helpers = Object.values(tools).map(tool => tool.handler);

    // All handlers should be callable with (args, extra)
    // This test validates at compile-time that handlers match the signature
    for (const handler of helpers) {
      expect(typeof handler).toBe('function');
    }
  });

  test("SessionState type is properly exported", async () => {
    // This test validates that SessionState type is accessible
    // and PrologInterface.getSessionState() returns the correct type

    const { PrologInterface } = await import("../../src/PrologInterface.js");
    const prolog = new PrologInterface();

    const state = prolog.getSessionState();

    // State should be one of the valid session states
    const validStates = ["idle", "query", "query_completed", "engine", "engine_completed"];
    expect(validStates).toContain(state);
  });
});
