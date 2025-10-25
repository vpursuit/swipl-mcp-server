import { describe, expect } from "vitest";
import { jsonSchemas } from "@vpursuit/mcp-prolog";

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

  it("knowledgeBaseAssert.fact should accept only string", () => {
    const knowledgeBaseAssert = jsonSchemas.knowledgeBaseAssert as any;
    expect(knowledgeBaseAssert.type).toBe("object");
    expect(knowledgeBaseAssert.required).toEqual(["fact"]);
    expect(knowledgeBaseAssert.properties.fact.type).toBe("string");
  });

  it("knowledgeBaseAssertMany.facts should accept array of strings", () => {
    const knowledgeBaseAssertMany = jsonSchemas.knowledgeBaseAssertMany as any;
    expect(knowledgeBaseAssertMany.type).toBe("object");
    expect(knowledgeBaseAssertMany.required).toEqual(["facts"]);
    expect(knowledgeBaseAssertMany.properties.facts.type).toBe("array");
    expect(knowledgeBaseAssertMany.properties.facts.items.type).toBe("string");
  });

  it("knowledgeBaseRetract.fact should accept only string", () => {
    const knowledgeBaseRetract = jsonSchemas.knowledgeBaseRetract as any;
    expect(knowledgeBaseRetract.type).toBe("object");
    expect(knowledgeBaseRetract.required).toEqual(["fact"]);
    expect(knowledgeBaseRetract.properties.fact.type).toBe("string");
  });

  it("knowledgeBaseRetractMany.facts should accept array of strings", () => {
    const knowledgeBaseRetractMany = jsonSchemas.knowledgeBaseRetractMany as any;
    expect(knowledgeBaseRetractMany.type).toBe("object");
    expect(knowledgeBaseRetractMany.required).toEqual(["facts"]);
    expect(knowledgeBaseRetractMany.properties.facts.type).toBe("array");
    expect(knowledgeBaseRetractMany.properties.facts.items.type).toBe("string");
  });

  it("knowledgeBaseClear should have no parameters", () => {
    const knowledgeBaseClear = jsonSchemas.knowledgeBaseClear as any;
    expect(knowledgeBaseClear.type).toBe("object");
    expect(knowledgeBaseClear.properties).toEqual({});
    expect(knowledgeBaseClear.additionalProperties).toBe(false);
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
