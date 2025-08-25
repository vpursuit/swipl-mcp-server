import { PrologInterface } from "../src/PrologInterface.js";

describe("Correlation ID routing", () => {
  test("routes tagged responses by id even if not FIFO", async () => {
    const iface = new PrologInterface();
    // Monkeypatch internal pending map with two promises
    const p1 = new Promise<string>((resolve) => {
      (iface as any).queryPromises.set("1", { resolve, reject: () => {} });
    });
    const p2 = new Promise<string>((resolve) => {
      (iface as any).queryPromises.set("2", { resolve, reject: () => {} });
    });

    // Deliver responses out of order
    (iface as any).processResponseLine("id(2, respB)");
    (iface as any).processResponseLine("id(1, respA)");

    await expect(p1).resolves.toBe("respA");
    await expect(p2).resolves.toBe("respB");
  });
});
