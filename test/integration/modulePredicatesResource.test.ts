/**
 * Integration test for the prolog://module/{name}/predicates resource template.
 */

import { describe, test, expect, beforeAll, afterAll } from "vitest";
import { spawn, execSync } from "child_process";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Module Predicates Resource", () => {
  let child: import("child_process").ChildProcess | null = null;
  let nextId = 1;
  const pending = new Map<number, (msg: any) => void>();

  const send = (method: string, params?: any) => {
    const id = nextId++;
    const msg = { jsonrpc: "2.0", id, method, params };
    child?.stdin?.write(JSON.stringify(msg) + "\n");
    return new Promise<any>((resolve) => pending.set(id, resolve));
  };

  beforeAll(async () => {
    execSync("npm run build", { stdio: "inherit", cwd: process.cwd() });
    child = spawn(process.execPath, ["build/index.js"], {
      cwd: process.cwd(),
      stdio: ["pipe", "pipe", "pipe"],
      env: { ...process.env },
    });
    child.stdout?.setEncoding("utf8");
    child.stdout?.on("data", (chunk: string) => {
      for (const line of chunk.split(/\r?\n/)) {
        if (!line.trim()) continue;
        try {
          const msg = JSON.parse(line);
          if (msg.id && pending.has(msg.id)) {
            const resolve = pending.get(msg.id)!;
            pending.delete(msg.id);
            resolve(msg);
          }
        } catch {}
      }
    });
  }, 60000);

  afterAll(async () => {
    try { child?.kill("SIGTERM"); } catch {}
    child = null;
  });

  test("lists predicates for kb module", async () => {
    // Add a known predicate
    await send("tools/call", { name: "db_assert", arguments: { fact: "parent(alice, bob)" } });
    const readResp = await send("resources/read", { uri: "prolog://kb/predicates" });
    const text = readResp?.result?.contents?.[0]?.text || "";
    // We expect at least parent/2 in the exported set for kb
    expect(text).toMatch(/parent\/[0-9]+/);
  }, 60000);
});
