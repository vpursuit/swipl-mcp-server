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

  it("dbAssert.fact should accept only string", () => {
    const dbAssert = jsonSchemas.dbAssert as any;
    expect(dbAssert.type).toBe("object");
    expect(dbAssert.required).toEqual(["fact"]);
    expect(dbAssert.properties.fact.type).toBe("string");
  });

  it("dbAssertMany.facts should accept array of strings", () => {
    const dbAssertMany = jsonSchemas.dbAssertMany as any;
    expect(dbAssertMany.type).toBe("object");
    expect(dbAssertMany.required).toEqual(["facts"]);
    expect(dbAssertMany.properties.facts.type).toBe("array");
    expect(dbAssertMany.properties.facts.items.type).toBe("string");
  });

  it("dbRetract.fact should accept only string", () => {
    const dbRetract = jsonSchemas.dbRetract as any;
    expect(dbRetract.type).toBe("object");
    expect(dbRetract.required).toEqual(["fact"]);
    expect(dbRetract.properties.fact.type).toBe("string");
  });

  it("dbRetractMany.facts should accept array of strings", () => {
    const dbRetractMany = jsonSchemas.dbRetractMany as any;
    expect(dbRetractMany.type).toBe("object");
    expect(dbRetractMany.required).toEqual(["facts"]);
    expect(dbRetractMany.properties.facts.type).toBe("array");
    expect(dbRetractMany.properties.facts.items.type).toBe("string");
  });

  it("dbRetractAll should have no parameters", () => {
    const dbRetractAll = jsonSchemas.dbRetractAll as any;
    expect(dbRetractAll.type).toBe("object");
    expect(dbRetractAll.properties).toEqual({});
    expect(dbRetractAll.additionalProperties).toBe(false);
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
