/**
 * Capabilities Tool Tests
 * Tests the capabilities tool for machine-readable server information
 */

import { describe, afterEach, test, expect } from "vitest";
import { toolHandlers } from "../../src/tools.js";

describe("Capabilities Tool", () => {
  const origEnv = { ...process.env };

  afterEach(() => {
    // Restore environment after each test
    process.env = { ...origEnv } as any;
  });

  test("capabilities returns machine-readable structured data only", async () => {
    const res = await toolHandlers.capabilities();
    expect(res.isError).toBeFalsy();
    // Use structured content for machine-readable data
    const obj = res.structuredContent;
    expect(obj.server?.name).toBe("swipl-mcp-server");
    expect(typeof obj.tools).toBe("object");
    expect(obj.tools?.core).toContain("capabilities");
    expect(obj.tools?.knowledge_base).toContain("clauses");
    expect(obj.security?.module).toBe("knowledge_base");
    // Capabilities should return JSON in content for backwards compatibility (MCP best practice)
    expect(res.content).toHaveLength(1);
    expect(res.content[0].type).toBe("text");
    // Verify the content contains valid JSON
    expect(() => JSON.parse(res.content[0].text)).not.toThrow();
    const parsedContent = JSON.parse(res.content[0].text);
    expect(parsedContent.server?.name).toBe("swipl-mcp-server");
  });


});
