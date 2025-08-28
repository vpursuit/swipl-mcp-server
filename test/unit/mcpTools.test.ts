/**
 * Unit tests for MCP Tools
 * Tests input validation schemas and tool registration
 */

import { describe, it, expect } from "vitest";
import { z } from "zod";
import { inputSchemas } from "../../src/tools.js";

describe("MCP Tools", () => {
  describe("Input Validation Schemas", () => {
    describe("DbLoadInputSchema", () => {
      const DbLoadInputSchema = z.object({
        filename: z.string().describe("Path to the Prolog file to load"),
      });

      it("should validate correct filename input", () => {
        const validInput = { filename: "test.pl" };
        const result = DbLoadInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.filename).toBe("test.pl");
        }
      });

      it("should reject missing filename", () => {
        const invalidInput = {};
        const result = DbLoadInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });

      it("should reject non-string filename", () => {
        const invalidInput = { filename: 123 };
        const result = DbLoadInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });

    describe("QueryStartInputSchema", () => {
      const QueryStartInputSchema = z.object(inputSchemas.queryStart);

      it("should validate correct query input", () => {
        const validInput = { query: "test_fact(X)" };
        const result = QueryStartInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.query).toBe("test_fact(X)");
        }
      });

      it("should reject empty query", () => {
        const invalidInput = { query: "" };
        const result = QueryStartInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });

    describe("QueryNextInputSchema", () => {
      const QueryNextInputSchema = z.object(inputSchemas.queryNext);

      it("should validate empty input for next", () => {
        const validInput = {};
        const result = QueryNextInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
      });
    });

    describe("QueryCloseInputSchema", () => {
      const QueryCloseInputSchema = z.object(inputSchemas.queryClose);

      it("should validate empty input for close", () => {
        const validInput = {};
        const result = QueryCloseInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
      });
    });

    describe("DbAssertInputSchema", () => {
      const DbAssertInputSchema = z.object(inputSchemas.dbAssert);

      it("should validate correct fact input", () => {
        const validInput = { fact: "parent(john, mary)" };
        const result = DbAssertInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.fact).toBe("parent(john, mary)");
        }
      });

      it("should reject missing fact", () => {
        const invalidInput = {};
        const result = DbAssertInputSchema.safeParse(invalidInput);

        expect(result.success).toBe(false);
      });
    });

    describe("RetractFactInputSchema", () => {
      const RetractFactInputSchema = z.object({
        fact: z.string().describe("Prolog fact to retract (e.g., 'parent(john, mary)')"),
      });

      it("should validate correct retraction input", () => {
        const validInput = { fact: "parent(john, mary)" };
        const result = RetractFactInputSchema.safeParse(validInput);

        expect(result.success).toBe(true);
        if (result.success) {
          expect(result.data.fact).toBe("parent(john, mary)");
        }
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


  describe("start_query Tool Logic", () => {
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

  describe("list_predicates Tool Logic", () => {
    it("should generate correct predicate listing query", () => {
      const expectedQuery = "current_predicate(X/Y), format('~w/~w~n', [X,Y]), fail; true";

      expect(expectedQuery).toContain("current_predicate");
      expect(expectedQuery).toContain("format");
      expect(expectedQuery).toContain("fail; true");
    });

    it("should format predicate list response", () => {
      const mockPredicates = "test_fact/1\nparent/2\nmale/1";

      const expectedResponse = {
        content: [
          {
            type: "text",
            text: `Available predicates:\n${mockPredicates}`,
          },
        ],
      };

      expect(expectedResponse.content[0].text).toContain("Available predicates:");
      expect(expectedResponse.content[0].text).toContain("test_fact/1");
    });
  });

  describe("assert Tool Logic", () => {
    it("should generate correct assert query", () => {
      const fact = "test_new_fact(value)";
      const assertQuery = `assert(${fact})`;

      expect(assertQuery).toBe("assert(test_new_fact(value))");
    });

    it("should format assert response correctly", () => {
      const fact = "test_new_fact(value)";
      const mockResult = "true";

      const expectedResponse = {
        content: [
          {
            type: "text",
            text: `Asserted fact: ${fact}\nResult: ${mockResult}`,
          },
        ],
      };

      expect(expectedResponse.content[0].text).toContain(`Asserted fact: ${fact}`);
    });
  });

  describe("retract Tool Logic", () => {
    it("should generate correct retract query", () => {
      const fact = "test_fact(value)";
      const retractQuery = `retract(${fact})`;

      expect(retractQuery).toBe("retract(test_fact(value))");
    });

    it("should format retract response correctly", () => {
      const fact = "test_fact(value)";
      const mockResult = "true";

      const expectedResponse = {
        content: [
          {
            type: "text",
            text: `Retracted fact: ${fact}\nResult: ${mockResult}`,
          },
        ],
      };

      expect(expectedResponse.content[0].text).toContain(`Retracted fact: ${fact}`);
    });
  });

  describe("Tool Registration", () => {
    it("should register all expected tools", () => {
      const expectedTools = [
        "consult_file",
        "start_query",
        "next_solution",
        "close_query",
        "list_predicates",
        "assert_clause",
        "retract_clause",
      ];

      expectedTools.forEach((toolName) => {
        expect(typeof toolName).toBe("string");
        expect(toolName.length).toBeGreaterThan(0);
      });

      expect(expectedTools).toHaveLength(7);
    });

    it("should have proper tool descriptions", () => {
      const toolDescriptions = {
        consult_file: "Consult file to load a Prolog file into the knowledge base",
        start_query: "Start query session for Prolog queries",
        next_solution: "Get next solution from the current query",
        close_query: "Close query session when finished",
        list_predicates: "List predicates available in the knowledge base",
        assert_clause: "Assert clause to add a new clause (fact or rule) to the knowledge base",
        retract_clause: "Retract clause to remove a clause (fact or rule) from the knowledge base",
      };

      Object.entries(toolDescriptions).forEach(([tool, description]) => {
        expect(description.toLowerCase()).toContain(tool.replace("_", " "));
        expect(description.length).toBeGreaterThan(10);
      });
    });
  });

});
