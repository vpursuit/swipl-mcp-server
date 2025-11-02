/**
 * Vitest test setup for swipl-mcp-server
 * - E2E and integration tests for the orchestrator package
 */

import { spawnSync } from "child_process";

// Detect SWI-Prolog availability
try {
  const res = spawnSync("swipl", ["--version"], { stdio: "ignore" });
  global.HAS_SWIPL = res.status === 0;
} catch {
  global.HAS_SWIPL = false;
}
