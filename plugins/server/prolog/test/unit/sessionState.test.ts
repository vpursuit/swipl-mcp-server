import { describe, test, expect, vi, beforeEach, afterEach } from "vitest";
import { PrologInterface } from "../../src/PrologInterface.js";
import { logger } from "../../src/logger.js";

describe("Session state behavior", () => {
  const prevTrace = process.env.SWI_MCP_TRACE;
  let warnSpy: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    process.env.SWI_MCP_TRACE = "1";
    warnSpy = vi.spyOn(logger, "warn").mockImplementation(() => {});
  });

  afterEach(() => {
    warnSpy.mockRestore();
    if (prevTrace === undefined) delete process.env.SWI_MCP_TRACE; else process.env.SWI_MCP_TRACE = prevTrace;
  });

  test("invalid transition logs a warning", () => {
    const iface = new PrologInterface();
    // idle -> query_completed is invalid; should log a warning
    (iface as any).setSessionState("query_completed");
    expect(warnSpy).toHaveBeenCalled();
  });

  test("query_completed yields no more solutions without process", async () => {
    const iface = new PrologInterface();
    // Simulate completed query state without an active process
    (iface as any).queryActive = false;
    (iface as any).sessionState = "query_completed";
    const res = await iface.nextSolution();
    expect(res.status).toBe("done");
    expect(res.error).toBeUndefined();
  });

  test("engine_completed yields no more solutions without process", async () => {
    const iface = new PrologInterface();
    // Simulate completed engine state without an active process
    (iface as any).engineActive = false;
    (iface as any).engineReachedEOF = true;
    (iface as any).sessionState = "engine_completed";
    const res = await iface.nextEngine();
    expect(res.status).toBe("done");
    expect(res.error).toBeUndefined();
  });
});

