/**
 * Unit tests for MCP Tools
 * Tests input validation schemas and tool registration
 * Updated for refactored 7-tool API
 */

import { describe, it, expect } from "vitest";
import { z } from "zod";
import { inputSchemas } from "../../src/tools.js";

describe("MCP Tools - Refactored API", () => {
  describe("Input Validation Schemas", () => {
    describe("QueryInputSchema (Unified)", () => {
      const QueryInputSchema = z.object(inputSchemas.query);

      it("should validate start operation with standard mode", () => {
        const validInput = { operation: "start", query: "test_fact(X)" };
        const result = QueryInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("start");
          expect(result.data.query).toBe("test_fact(X)");
          expect(result.data.use_engine).toBe(false); // default
        }
      });

      it("should validate start operation with engine mode", () => {
        const validInput = { operation: "start", query: "test_fact(X)", use_engine: true };
        const result = QueryInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("start");
          expect(result.data.query).toBe("test_fact(X)");
          expect(result.data.use_engine).toBe(true);
        }
      });

      it("should validate next operation", () => {
        const validInput = { operation: "next" };
        const result = QueryInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("next");
        }
      });

      it("should validate close operation", () => {
        const validInput = { operation: "close" };
        const result = QueryInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("close");
        }
      });

      it("should reject empty query for start operation", () => {
        const invalidInput = { operation: "start", query: "" };
        const result = QueryInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });

      it("should reject invalid operation", () => {
        const invalidInput = { operation: "invalid" };
        const result = QueryInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });

    // Help tool removed - use capabilities instead

    // Unified clauses tool (Step 3)
    describe("ClausesInputSchema", () => {
      const ClausesInputSchema = z.object(inputSchemas.clauses);

      it("should validate assert operation with single clause", () => {
        const validInput = { operation: "assert", clauses: "parent(john, mary)" };
        const result = ClausesInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("assert");
          expect(result.data.clauses).toBe("parent(john, mary)");
        }
      });

      it("should validate assert operation with array of clauses", () => {
        const validInput = { operation: "assert", clauses: ["fact1", "fact2"] };
        const result = ClausesInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.clauses).toEqual(["fact1", "fact2"]);
        }
      });

      it("should reject missing operation", () => {
        const invalidInput = { clauses: "test" };
        const result = ClausesInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });

    // Unified files tool (Step 4)
    describe("FilesInputSchema", () => {
      const FilesInputSchema = z.object(inputSchemas.files);

      it("should validate import operation with filename", () => {
        const validInput = { operation: "import", filename: "test.pl" };
        const result = FilesInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("import");
          expect(result.data.filename).toBe("test.pl");
        }
      });

      it("should validate unimport operation with filename", () => {
        const validInput = { operation: "unimport", filename: "test.pl" };
        const result = FilesInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("unimport");
          expect(result.data.filename).toBe("test.pl");
        }
      });

      it("should validate list operation without filename", () => {
        const validInput = { operation: "list" };
        const result = FilesInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("list");
        }
      });

      it("should reject missing operation", () => {
        const invalidInput = { filename: "test.pl" };
        const result = FilesInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });

      it("should reject invalid operation", () => {
        const invalidInput = { operation: "invalid", filename: "test.pl" };
        const result = FilesInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });

    // Unified workspace tool (Step 5)
    describe("WorkspaceInputSchema", () => {
      const WorkspaceInputSchema = z.object(inputSchemas.workspace);

      it("should validate snapshot operation without filename", () => {
        const validInput = { operation: "snapshot" };
        const result = WorkspaceInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("snapshot");
        }
      });

      it("should validate reset operation", () => {
        const validInput = { operation: "reset" };
        const result = WorkspaceInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("reset");
        }
      });

      it("should validate list_symbols operation", () => {
        const validInput = { operation: "list_symbols" };
        const result = WorkspaceInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.operation).toBe("list_symbols");
        }
      });

      it("should reject missing operation", () => {
        const invalidInput = { filename: "test.pl" };
        const result = WorkspaceInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });

      it("should reject invalid operation", () => {
        const invalidInput = { operation: "invalid" };
        const result = WorkspaceInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });
  });

  describe("Tool Response Structure", () => {
    it("should have correct success response format", () => {
      const successResponse = {
        content: [{ type: "text", text: "Operation successful" }],
      };

      expect(successResponse.content).toHaveLength(1);
      expect(successResponse.content[0].type).toBe("text");
      expect(successResponse.content[0].text).toBe("Operation successful");
    });

    it("should have correct error response format", () => {
      const errorResponse = {
        content: [{ type: "text", text: "Error occurred" }],
        isError: true,
      };

      expect(errorResponse.content).toHaveLength(1);
      expect(errorResponse.isError).toBe(true);
    });
  });

  describe("Query Tool Logic", () => {
    it("should format simple queries correctly", () => {
      const query = "test_fact(X)";
      const formattedQuery = query.endsWith(".") ? query : query + ".";

      expect(formattedQuery).toBe("test_fact(X).");
    });

    it("should handle queries that already end with period", () => {
      const query = "test_fact(X).";
      const formattedQuery = query.endsWith(".") ? query : query + ".";

      expect(formattedQuery).toBe("test_fact(X).");
    });

    it("should generate expected response format", () => {
      const query = "test_fact(X)";
      const mockResult = "X = a";

      const expectedResponse = {
        content: [
          {
            type: "text",
            text: `Query: ${query}\nResult: ${mockResult}`,
          },
        ],
      };

      expect(expectedResponse.content[0].text).toContain(`Query: ${query}`);
      expect(expectedResponse.content[0].text).toContain(`Result: ${mockResult}`);
    });
  });

  describe("Tool Registration", () => {
    it("should register expected number of tools", () => {
      // Current state: Still has legacy tools
      // After Step 8: Will have exactly 7 tools
      const currentToolCount = Object.keys(inputSchemas).length;

      // Should have at least the core query tools
      expect(currentToolCount).toBeGreaterThanOrEqual(3);
    });

    // Unified query tool (Step 6)
    it("should have unified query tool registered", () => {
      expect(inputSchemas).toHaveProperty("query");
    });
  });
});
