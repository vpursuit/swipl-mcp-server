import { describe, test, expect } from "vitest";
import { PrologInterface } from "../../src/PrologInterface.js";

describe("Correlation ID routing", () => {
  test("routes tagged responses by id even if not FIFO", async () => {
    const iface = new PrologInterface();
    // Monkeypatch internal pending map with two promises
    const p1 = new Promise<string>((resolve) => {
      (iface as any).queryPromises.set("1", { resolve, reject: () => { } });
    });
    const p2 = new Promise<string>((resolve) => {
      (iface as any).queryPromises.set("2", { resolve, reject: () => { } });
    });

    // Deliver responses out of order
    (iface as any).processResponseLine("id(2, respB)");
    (iface as any).processResponseLine("id(1, respA)");

    await expect(p1).resolves.toBe("respA");
    await expect(p2).resolves.toBe("respB");
  });

  test("drops late or unknown id responses without FIFO fallback", async () => {
    const iface = new PrologInterface();
    // Seed a different pending id
    let resolved = false;
    (iface as any).queryPromises.set("1", { resolve: () => { resolved = true; }, reject: () => { } });

    // Deliver a response for an unknown id - should be dropped
    (iface as any).processResponseLine("id(999, stray)");

    // Ensure the pending entry is still present and not resolved
    expect((iface as any).queryPromises.has("1")).toBe(true);
    expect(resolved).toBe(false);
  });
});
