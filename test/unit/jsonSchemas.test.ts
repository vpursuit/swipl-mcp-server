import { describe, expect } from "vitest";
import { jsonSchemas } from "../../src/tools.js";

describe("JSON Schemas (MCP registration)", () => {
  it("should be plain serializable JSON objects", () => {
    const s = JSON.stringify(jsonSchemas);
    expect(typeof s).toBe("string");
    expect(s.length).toBeGreaterThan(10);
  });

  it("help.topic enum should include expected values and be optional", () => {
    const help = jsonSchemas.help as any;
    expect(help.type).toBe("object");
    expect(help.properties.topic.enum).toEqual(
      expect.arrayContaining([
        "overview",
        "standard_mode",
        "engine_mode",
        "safety",
        "security",
        "examples",
        "troubleshooting",
      ]),
    );
    // Not required by default
    expect(help.required).toBeUndefined();
  });

  it("queryStart.query should be required string with minLength 1", () => {
    const queryStart = jsonSchemas.queryStart as any;
    expect(queryStart.type).toBe("object");
    expect(queryStart.required).toEqual(["query"]);
    expect(queryStart.properties.query.type).toBe("string");
    expect(queryStart.properties.query.minLength).toBe(1);
  });

  it("dbAssert.fact should accept string or array of strings", () => {
    const dbAssert = jsonSchemas.dbAssert as any;
    expect(dbAssert.type).toBe("object");
    expect(dbAssert.required).toEqual(["fact"]);
    expect(Array.isArray(dbAssert.properties.fact.anyOf)).toBe(true);
    expect(dbAssert.properties.fact.anyOf.length).toBe(2);
    expect(dbAssert.properties.fact.anyOf[0].type).toBe("string");
    expect(dbAssert.properties.fact.anyOf[1].type).toBe("array");
    expect(dbAssert.properties.fact.anyOf[1].items.type).toBe("string");
  });

  it("empty-input tools should forbid additionalProperties", () => {
    const emptyish = [
      jsonSchemas.queryNext,
      jsonSchemas.queryClose,
      jsonSchemas.queryNext,
      jsonSchemas.queryClose,
      jsonSchemas.symbolsList,
    ] as any[];
    for (const sch of emptyish) {
      expect(sch).toMatchObject({ type: "object", properties: {}, additionalProperties: false });
    }
  });
});
