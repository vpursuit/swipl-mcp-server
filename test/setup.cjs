/**
 * Jest test setup file (CommonJS)
 * - Keeps runtime setup in CJS so Jest can load it without ESM hassles
 * - Configures globals, env timeouts, and graceful teardown
 */

// Shorten PrologInterface timeouts under test
process.env.SWI_MCP_READY_TIMEOUT_MS = process.env.SWI_MCP_READY_TIMEOUT_MS || "2500";
process.env.SWI_MCP_QUERY_TIMEOUT_MS = process.env.SWI_MCP_QUERY_TIMEOUT_MS || "2500";

// Detect SWI-Prolog availability once
try {
  const { spawnSync } = require("child_process");
  const res = spawnSync("swipl", ["--version"], { stdio: "ignore" });
  global.HAS_SWIPL = res.status === 0;
} catch {
  global.HAS_SWIPL = false;
}

// Ensure the singleton prologInterface is stopped at the end of all tests
// Importing ESM from CJS: use dynamic import()
import("../src/tools.js").then((mod) => {
  const prologInterface = mod?.prologInterface;
  if (prologInterface) {
    afterAll(() => {
      try { prologInterface.stop(); } catch {}
    });
  }
}).catch(() => {});

// Custom Jest matcher for Prolog responses
expect.extend({
  toBeValidPrologResponse(received) {
    const isValid = typeof received === 'string' && received.length > 0;
    if (isValid) {
      return {
        message: () => `Expected ${received} not to be a valid Prolog response`,
        pass: true,
      };
    } else {
      return {
        message: () => `Expected ${received} to be a valid Prolog response`,
        pass: false,
      };
    }
  },
});
