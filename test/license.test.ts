import { toolHandlers } from "../src/tools.js";

describe("License Tool", () => {
  test("license tool returns BSD-3-Clause license with Peter Trebing", async () => {
    const result = await toolHandlers.license();
    
    expect(result.isError).toBeFalsy();
    expect(result.content).toHaveLength(1);
    expect(result.content[0].type).toBe("text");
    
    const licenseText = result.content[0].text;
    expect(licenseText).toContain("BSD 3-Clause License");
    expect(licenseText).toContain("Copyright (c) 2025, Peter Trebing");
    expect(licenseText).toContain("Redistribution and use in source and binary forms");
    expect(licenseText).toContain("Neither the name of the copyright holder");
    
    // Test structured content
    expect(result.structuredContent).toBeDefined();
    expect(result.structuredContent?.type).toBe("license");
    expect(result.structuredContent?.text).toBe(licenseText);
  });
});