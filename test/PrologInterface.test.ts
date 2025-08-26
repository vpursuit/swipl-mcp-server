import fs from "fs";
import { spawn } from "child_process";
import { PrologInterface } from "../src/PrologInterface.js";
import { logger } from "../src/logger.js";

jest.mock("child_process");
const mockSpawn = spawn as jest.MockedFunction<typeof spawn>;

describe("PrologInterface (unit)", () => {
  let iface: PrologInterface;
  let dataHandler: ((buf: Buffer) => void) | null = null;
  let mockProc: any;
  let stderrHandler: ((buf: Buffer) => void) | null = null;
  const handlers: Record<string, Function[]> = {};

  const makeMockProc = () => {
    dataHandler = null;
    const stdout = {
      on: jest.fn((event: string, cb: any) => {
        if (event === "data") dataHandler = cb;
      }),
    };
    const stdin = { write: jest.fn() };
    const on = jest.fn((event: string, cb: any) => {
      (handlers[event] ||= []).push(cb);
      return mockProc;
    });
    const stderr = {
      on: jest.fn((event: string, cb: any) => {
        if (event === "data") stderrHandler = cb;
      }),
    };
    mockProc = { stdout, stdin, stderr, on, kill: jest.fn() };
    mockSpawn.mockReturnValue(mockProc as any);
  };

  beforeEach(() => {
    jest.clearAllMocks();
    process.env.SWI_MCP_PROLOG_PATH = undefined;
    process.env.SWI_MCP_TRACE = undefined;
    process.env.SWI_MCP_READY_TIMEOUT_MS = undefined;
    iface = new PrologInterface();
    makeMockProc();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  it("start() uses env override and resolves on READY", async () => {
    const envPath = "/fake/prolog_server.pl";
    process.env.SWI_MCP_PROLOG_PATH = envPath;

    const accessSpy = jest.spyOn(fs, "accessSync").mockImplementation((p: any) => {
      if (String(p) !== envPath) throw Object.assign(new Error("not found"), { code: "ENOENT" });
    });

    const startPromise = iface.start();
    // emit READY
    expect(typeof dataHandler).toBe("function");
    dataHandler && dataHandler(Buffer.from("@@READY@@\n"));
    await startPromise;

    // spawn called with -s envPath
    expect(mockSpawn).toHaveBeenCalled();
    const args = (mockSpawn.mock.calls[0] || [])[1] as string[];
    expect(args).toContain("-s");
    expect(args).toContain(envPath);

    accessSpy.mockRestore();
  });

  it("start() rejects if swipl missing (ENOENT)", async () => {
    mockSpawn.mockImplementation(() => {
      const err: any = new Error("missing");
      err.code = "ENOENT";
      throw err;
    });
    await expect(new PrologInterface().start()).rejects.toThrow(
      /SWI-Prolog not found in PATH/i,
    );
  });

  it("start() times out without READY and cleans up", async () => {
    process.env.SWI_MCP_READY_TIMEOUT_MS = "5"; // tiny timeout
    await expect(iface.start()).rejects.toThrow(/ready timeout/i);
  });

  it("start() rejects on process error before READY", async () => {
    const envPath = "/fake/prolog_server.pl";
    process.env.SWI_MCP_PROLOG_PATH = envPath;
    jest.spyOn(fs, "accessSync").mockImplementation(() => {});
    const p = iface.start();
    // trigger process error
    handlers["error"]?.forEach((cb) => cb(Object.assign(new Error("boom"), { code: "EFAIL" })));
    await expect(p).rejects.toThrow(/prolog process/i);
  });

  it("start() rejects on process exit before READY", async () => {
    const envPath = "/fake/prolog_server.pl";
    process.env.SWI_MCP_PROLOG_PATH = envPath;
    jest.spyOn(fs, "accessSync").mockImplementation(() => {});
    const p = iface.start();
    // trigger process exit
    handlers["exit"]?.forEach((cb) => cb(0, null));
    await expect(p).rejects.toThrow(/exited before ready/i);
  });

  it("logs stderr at trace level during startup", async () => {
    const envPath = "/fake/prolog_server.pl";
    process.env.SWI_MCP_PROLOG_PATH = envPath;
    process.env.SWI_MCP_TRACE = "1";
    jest.spyOn(fs, "accessSync").mockImplementation(() => {});
    const dbg = jest.spyOn(logger, "debug");
    const p = iface.start();
    // emit some stderr
    stderrHandler && stderrHandler(Buffer.from("warning: something"));
    // now emit READY to resolve
    dataHandler && dataHandler(Buffer.from("@@READY@@\n"));
    await p;
    expect(dbg).toHaveBeenCalled();
    const hadStderrLog = dbg.mock.calls.some((call) => String(call[0]).includes("Prolog stderr:"));
    expect(hadStderrLog).toBe(true);
    dbg.mockRestore();
  });

  it("routes responses by id(...) and FIFO fallback", async () => {
    // Prepare
    const envPath = "/fake/prolog_server.pl";
    process.env.SWI_MCP_PROLOG_PATH = envPath;
    jest.spyOn(fs, "accessSync").mockImplementation(() => {});
    const startPromise = iface.start();
    dataHandler && dataHandler(Buffer.from("@@READY@@\n"));
    await startPromise;

    // Two queries
    const p1 = iface.query("first");
    const p2 = iface.query("second");

    // Allow queue to register promises/writes
    await Promise.resolve();

    // id-route first
    dataHandler && dataHandler(Buffer.from("id(0, ok_first)\n"));
    await expect(p1).resolves.toBe("ok_first");

    // FIFO for second (no id(...))
    dataHandler && dataHandler(Buffer.from("ok_second\n"));
    await expect(p2).resolves.toBe("ok_second");
  });
});
