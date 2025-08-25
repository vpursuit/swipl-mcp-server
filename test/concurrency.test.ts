import { spawn } from "child_process";
import { PrologInterface } from "../src/PrologInterface.js";

jest.mock("child_process");
const mockSpawn = spawn as jest.MockedFunction<typeof spawn>;

describe("PrologInterface command queue", () => {
  let mockProcess: any;
  let mockStdin: any;
  let mockStdout: any;
  let dataHandler: ((buf: Buffer | string) => void) | null = null;

  beforeEach(() => {
    jest.clearAllMocks();
    dataHandler = null;

    mockStdin = { write: jest.fn() };
    mockStdout = {
      on: jest.fn((event: string, cb: any) => {
        if (event === "data") dataHandler = cb;
      }),
    };
    mockProcess = {
      stdin: mockStdin,
      stdout: mockStdout,
      stderr: { on: jest.fn() },
      on: jest.fn(),
      kill: jest.fn(),
    };
    mockSpawn.mockReturnValue(mockProcess as any);
  });

  test("serializes concurrent sendCommand calls", async () => {
    const iface = new PrologInterface();

    // Start and simulate READY
    const startPromise = iface.start();
    expect(typeof dataHandler).toBe("function");
    dataHandler && dataHandler(Buffer.from("@@READY@@\n"));
    await startPromise;

    // Fire two queries concurrently
    const p1 = iface.query("first");
    const p2 = iface.query("second");

    // Allow the queue to schedule the first write
    await Promise.resolve();

    // Only the first write should have been performed until first resolves
    expect(mockStdin.write).toHaveBeenCalledTimes(1);
    const firstCallArg = (mockStdin.write as jest.Mock).mock.calls[0][0] as string;
    expect(firstCallArg.startsWith("cmd(")).toBe(true);
    expect(firstCallArg).toContain("first");

    // Respond to first
    dataHandler && dataHandler(Buffer.from("resp1\n"));
    const r1 = await p1;
    expect(r1).toBe("resp1");

    // Now the second should be written
    await Promise.resolve();
    expect(mockStdin.write).toHaveBeenCalledTimes(2);
    const secondCallArg = (mockStdin.write as jest.Mock).mock.calls[1][0] as string;
    expect(secondCallArg.startsWith("cmd(")).toBe(true);
    expect(secondCallArg).toContain("second");

    // Respond to second
    dataHandler && dataHandler(Buffer.from("resp2\n"));
    const r2 = await p2;
    expect(r2).toBe("resp2");
  });
});
