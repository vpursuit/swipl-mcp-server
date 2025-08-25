import { toolHandlers } from "../src/tools.js";

describe("Agent vs Developer Help/Diagnostics", () => {
  const origEnv = { ...process.env };

  afterEach(() => {
    // Restore environment after each test
    process.env = { ...origEnv } as any;
  });

  test("capabilities returns machine-readable agent summary without env list", async () => {
    const res = await toolHandlers.capabilities();
    expect(res.isError).toBeFalsy();
    // Use structured content for machine-readable data
    const obj = res.structuredContent;
    expect(obj.server?.name).toBe("swipl-mcp-server");
    expect(typeof obj.tools).toBe("object");
    expect(obj.tools?.core).toContain("help");
    expect(obj.tools?.database).toContain("db_assert");
    expect(obj.security?.module).toBe("kb");
    // Legacy env list should not exist in agent-facing capabilities
    expect(obj.env).toBeUndefined();
    // Verify plain text content is human-readable
    const text = (res.content[0] as any).text;
    expect(text).toMatch(/SWI-Prolog MCP Server/);
    expect(text).toMatch(/Query Modes:/);
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
