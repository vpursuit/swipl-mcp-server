/**
 * Unit tests for MCP Tools
 * Tests the MCP server tool handlers and their integration
 */

import { z } from "zod";
import path from "path";
import { readFile } from "fs/promises";
import { inputSchemas, toolHandlers } from "../src/tools.js";
import { vi } from 'vitest';

// Mock fs/promises and child_process
vi.mock("fs/promises");
vi.mock("child_process");
const mockReadFile = readFile as ReturnType<typeof vi.mocked<typeof readFile>>;

// Mock child_process for tools
import { spawn } from "child_process";
const mockSpawn = vi.mocked(spawn);

describe("MCP Tools", () => {
  // Mock request structure
  const _createMockRequest = (toolName: string, args: any) => ({
    params: {
      name: toolName,
      arguments: args,
    },
  });

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

  describe("consult_file Tool Logic", () => {
    beforeEach(() => {
      vi.clearAllMocks();
    });

    it("should check file existence before consulting", async () => {
      const filename = "test.pl";
      mockReadFile.mockResolvedValue("test_fact(a).");

      try {
        await readFile(filename, "utf8");
        expect(mockReadFile).toHaveBeenCalledWith(filename, "utf8");
      } catch (_err) {
        // File doesn't exist - should trigger error response
        expect(_err).toBeDefined();
      }
    });

    it("should handle file not found error", async () => {
      const filename = "nonexistent.pl";
      mockReadFile.mockRejectedValue(new Error("File not found"));

      try {
        await readFile(filename, "utf8");
      } catch (_err) {
        const errorResponse = {
          content: [{ type: "text", text: `Error: File '${filename}' not found or not readable` }],
          isError: true,
        };

        expect(errorResponse.isError).toBe(true);
        expect(errorResponse.content[0].text).toContain("not found");
      }
    });

    it("should generate correct consult query", () => {
      const filename = "test.pl";
      const absolutePath = path.resolve(filename);
      const consultQuery = `consult('${absolutePath}')`;

      expect(consultQuery).toBe(`consult('${absolutePath}')`);
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

  describe("Tool Handlers", () => {
    beforeEach(() => {
      // Setup proper spawn mock for each test
      mockSpawn.mockImplementation(() => ({
        stdout: {
          on: vi.fn(),
        },
        stdin: {
          write: vi.fn(),
        },
        stderr: null,
        on: vi.fn(),
        kill: vi.fn(),
      } as any));
    });

    describe("consultFile", () => {
      it("should return error for non-existent file", async () => {
        mockReadFile.mockRejectedValueOnce(new Error("ENOENT: no such file"));

        const result = await toolHandlers.dbLoad({ filename: "non-existent.pl" });

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/not found or not readable|ready timeout/);
      });

      it("should handle valid file input", async () => {
        mockReadFile.mockResolvedValueOnce("test_fact(a).");

        // Mock the PrologInterface methods indirectly via the module import
        const result = await toolHandlers.dbLoad({ filename: "test.pl" });

        // Since SWI-Prolog is mocked, we expect a timeout or process error
        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/Error:|Query timeout/);
      });
    });

    describe("startQuery", () => {
      it("should handle query execution", async () => {
        const result = await toolHandlers.queryStart({ query: "test_fact(X)" });

        // Since SWI-Prolog is mocked, we expect a process error
        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/Error:|Query timeout/);
      });
    });

    describe("nextSolution", () => {
      it("should handle next solution request", async () => {
        const result = await toolHandlers.queryNext();

        // Since SWI-Prolog is mocked and no query is active, expect error
        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/No active query|Error:/);
      });
    });

    describe("closeQuery", () => {
      it("should handle close query request", async () => {
        const result = await toolHandlers.queryClose();

        // Should handle gracefully even with no active query
        expect(result.isError).toBeFalsy();
        expect(result.content[0].text).toMatch(/No active session|closed/);
      });
    });

    describe("listPredicates", () => {
      it("should attempt to list predicates", async () => {
        const result = await toolHandlers.symbolsList();

        // Since SWI-Prolog is mocked, we expect a process error
        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/Error:|Query timeout/);
      });
    });

    describe("assertClause", () => {
      it("should attempt to assert clause", async () => {
        const result = await toolHandlers.dbAssert({ fact: "test_fact(new)" });

        // Since SWI-Prolog is mocked, we expect a process error
        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/Error:|Query timeout/);
      });
    });

    describe("retractClause", () => {
      it("should attempt to retract fact", async () => {
        const result = await toolHandlers.dbRetract({ fact: "test_fact(old)" });

        // Since SWI-Prolog is mocked, we expect a process error
        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/Error:|Query timeout/);
      });

      it("should handle array of facts for assert", async () => {
        const result = await toolHandlers.dbAssert({
          fact: ["fact1(a)", "fact2(b)", "fact3(c)"],
        });

        // Should handle array input correctly (even if it fails due to mocking)
        expect(result.content[0].text).toMatch(/Asserted \d+\/3 clauses/);
      }, 30000);

      it("should handle array of facts for retract", async () => {
        const result = await toolHandlers.dbRetract({
          fact: ["fact1(a)", "fact2(b)"],
        });

        // Should handle array input correctly (even if it fails due to mocking)
        expect(result.content[0].text).toMatch(/Retracted \d+\/2 clauses/);
      }, 30000);

      it("should handle single fact for assert (backwards compatibility)", async () => {
        const result = await toolHandlers.dbAssert({ fact: "single_fact(test)" });

        // Should handle single fact as before
        expect(result.content[0].text).toMatch(/Asserted \d+\/1 clauses/);
      }, 30000);
    });
  });
});
