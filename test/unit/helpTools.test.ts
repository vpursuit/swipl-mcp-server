import { describe, afterEach, test, expect } from "vitest";
import { toolHandlers } from "../../src/tools.js";

describe("Agent vs Developer Help/Diagnostics", () => {
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
    expect(obj.tools?.core).toContain("help");
    expect(obj.tools?.database).toContain("db_assert");
    expect(obj.tools?.database).toContain("db_assert_many");
    expect(obj.tools?.database).toContain("db_retract_all");
    expect(obj.security?.module).toBe("kb");
    // Legacy env list should not exist in agent-facing capabilities
    expect(obj.env).toBeUndefined();
    // Capabilities should return JSON in content for backwards compatibility (MCP best practice)
    expect(res.content).toHaveLength(1);
    expect(res.content[0].type).toBe("text");
    // Verify the content contains valid JSON
    expect(() => JSON.parse(res.content[0].text)).not.toThrow();
    const parsedContent = JSON.parse(res.content[0].text);
    expect(parsedContent.server?.name).toBe("swipl-mcp-server");
  });

  test("help (agent) returns usage guidance and excludes developer env section", async () => {
    const res = await toolHandlers.help({});
    expect(res.isError).toBeFalsy();
    const text = res.content[0].text;
    expect(text).toMatch(/Standard mode/i);
    expect(text).toMatch(/Engine mode/i);
    // Should not include developer 'Environment variables' section
    expect(text).not.toMatch(/Environment variables:/i);
  });


});
