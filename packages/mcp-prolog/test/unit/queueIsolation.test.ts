import { describe, test, expect } from "vitest";
import { PrologInterface } from "../../src/PrologInterface.js";

describe("Command queue isolation", () => {
  test("continues after a failed command and resolves the next", async () => {
    const iface = new PrologInterface();

    // Stub a minimal process with a stdin.write we can control
    let writeCall = 0;
    const proc: any = {
      stdin: {
        write: (_data: string) => {
          writeCall++;
          if (writeCall === 1) {
            // Simulate EPIPE by rejecting the first pending query asynchronously
            setTimeout(() => {
              const entry = (iface as any).queryPromises.get("0");
              entry?.reject(new Error("Prolog process connection lost (EPIPE)"));
            }, 0);
          }
          // succeed on subsequent writes
          return true;
        }
      }
    };
    (iface as any).process = proc;

    // First command should reject due to simulated EPIPE
    const first = iface.query("first_command");
    await expect(first).rejects.toThrow(/EPIPE|connection lost|not available/i);

    // Second command should still run and be resolvable via response routing
    const secondPromise = iface.query("second_command");
    // Wait a tick to allow registration of the pending promise for id=1
    await new Promise((r) => setTimeout(r, 0));
    // Resolve the second by emitting a matching id response (id=1 after first failure)
    (iface as any).processResponseLine("id(1, ok)");

    await expect(secondPromise).resolves.toBe("ok");
  });
});
