/**
 * JSON Schema Tests (MCP registration)
 * Updated for refactored API - maintains backward compatibility tests for legacy tools
 * Legacy tool tests will be removed in Step 8 of refactoring plan
 */

import { describe, expect, it } from "vitest";
import { jsonSchemas } from "@vpursuit/mcp-server-prolog";

describe("JSON Schemas (MCP registration)", () => {
  it("should be plain serializable JSON objects", () => {
    const s = JSON.stringify(jsonSchemas);
    expect(typeof s).toBe("string");
    expect(s.length).toBeGreaterThan(10);
  });

  describe("Core Tool Schemas", () => {
    // Help tool removed - capabilities tool is now the metadata/introspection tool

    it("query tool (unified) should support operation enum with conditional parameters", () => {
      const query = jsonSchemas.query as any;
      expect(query.type).toBe("object");
      expect(query.required).toEqual(["operation"]);
      expect(query.properties.operation.enum).toEqual(["start", "next", "close"]);
      // query parameter is optional (required for 'start' operation only)
      expect(query.properties.query).toBeDefined();
      // use_engine parameter is optional boolean
      expect(query.properties.use_engine).toBeDefined();
    });

    it("query tool query parameter should be optional string with minLength 1", () => {
      const query = jsonSchemas.query as any;
      expect(query.properties.query).toBeDefined();
      // Optional parameters are represented as anyOf with null or not required
      // Check that query has proper constraints when provided
      if (query.properties.query.anyOf) {
        const stringSchema = query.properties.query.anyOf.find((s: any) => s.type === "string");
        expect(stringSchema).toBeDefined();
        expect(stringSchema.minLength).toBe(1);
      } else {
        expect(query.properties.query.minLength).toBe(1);
      }
    });

    it("query tool use_engine parameter should be optional boolean with default false", () => {
      const query = jsonSchemas.query as any;
      expect(query.properties.use_engine).toBeDefined();
      // Check for default value
      if (query.properties.use_engine.anyOf) {
        const booleanSchema = query.properties.use_engine.anyOf.find((s: any) => s.type === "boolean");
        expect(booleanSchema).toBeDefined();
      }
      expect(query.properties.use_engine.default).toBe(false);
    });
  });

  describe("Unified Tool Schemas (Step 3)", () => {
    it("clauses tool should support operation enum and optional clauses parameter", () => {
      const clauses = jsonSchemas.clauses as any;
      expect(clauses.type).toBe("object");
      expect(clauses.required).toEqual(["operation"]);
      expect(clauses.properties.operation.enum).toEqual(["assert", "retract"]);
      expect(clauses.properties.clauses).toBeDefined();
    });
  });

  describe("Unified Tool Schemas (Step 4)", () => {
    it("files tool should support operation enum", () => {
      const files = jsonSchemas.files as any;
      expect(files.type).toBe("object");
      expect(files.required).toEqual(["operation"]);
      expect(files.properties.operation.enum).toEqual(["import", "unimport", "list"]);
      // filename parameter is optional (required based on operation)
      expect(files.properties.filename).toBeDefined();
    });

    it("files tool filename parameter should be optional string", () => {
      const files = jsonSchemas.files as any;
      expect(files.properties.filename).toBeDefined();
      // Optional string - check it's either direct type or anyOf
      const isOptionalString = files.properties.filename.type === "string" ||
        (files.properties.filename.anyOf && files.properties.filename.anyOf.some((s: any) => s.type === "string"));
      expect(isOptionalString).toBe(true);
    });
  });

  describe("Unified Tool Schemas (Step 5)", () => {
    it("workspace tool should support operation enum", () => {
      const workspace = jsonSchemas.workspace as any;
      expect(workspace.type).toBe("object");
      expect(workspace.required).toEqual(["operation"]);
      expect(workspace.properties.operation.enum).toEqual([
        "snapshot",
        "reset",
        "list_symbols"
      ]);
    });

    it("workspace tool should have all three operations in enum", () => {
      const workspace = jsonSchemas.workspace as any;
      const operations = workspace.properties.operation.enum;
      expect(operations).toHaveLength(3);
      expect(operations).toContain("snapshot");
      expect(operations).toContain("reset");
      expect(operations).toContain("list_symbols");
    });
  });
});
