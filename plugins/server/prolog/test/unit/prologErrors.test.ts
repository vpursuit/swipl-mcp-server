/**
 * Unit tests for Prolog error parsing and formatting
 * Tests the parsePrologError and formatPrologError functions
 */

import { describe, test, expect } from "vitest";
import { PrologInterface, PrologErrorKind } from "../../src/PrologInterface.js";

describe("Prolog Error Parsing and Formatting", () => {
  describe("parsePrologError", () => {
    describe("standard Prolog error terms", () => {
      test("should parse unsafe_goal error", () => {
        const result = PrologInterface.parsePrologError("error(unsafe_goal(system(rm -rf /)))");
        expect(result.kind).toBe(PrologErrorKind.UNSAFE_GOAL);
        expect(result.message).toBe("Security Error: Unsafe operation blocked");
        expect(result.details?.goal).toBe("system(rm -rf /)");
        expect(result.details?.raw).toBe("error(unsafe_goal(system(rm -rf /)))");
      });

      test("should parse permission_error", () => {
        const result = PrologInterface.parsePrologError("error(permission_error(execute, directive, 'test'))");
        expect(result.kind).toBe(PrologErrorKind.PERMISSION_ERROR);
        expect(result.message).toBe("Permission denied: Cannot execute directive");
        expect(result.details?.operation).toBe("execute");
        expect(result.details?.file).toBe("'test'");
      });

      test("should parse existence_error", () => {
        const result = PrologInterface.parsePrologError("error(existence_error(procedure, foo/2))");
        expect(result.kind).toBe(PrologErrorKind.EXISTENCE_ERROR);
        expect(result.message).toBe("procedure not found: foo/2");
        expect(result.details?.file).toBe("foo/2");
      });

      test("should parse syntax_error", () => {
        const result = PrologInterface.parsePrologError("error(syntax_error(unexpected_token))");
        expect(result.kind).toBe(PrologErrorKind.SYNTAX_ERROR);
        expect(result.message).toBe("Syntax error in Prolog code");
        expect(result.details?.raw).toBe("error(syntax_error(unexpected_token))");
      });

      test("should parse future server-side timeout", () => {
        const result = PrologInterface.parsePrologError("error(timeout(goal, 5000))");
        expect(result.kind).toBe(PrologErrorKind.TIMEOUT);
        expect(result.message).toBe("Query timed out");
        expect(result.details?.raw).toBe("error(timeout(goal, 5000))");
      });
    });

    describe("client-side timeout messages", () => {
      test("should parse client timeout with duration", () => {
        const result = PrologInterface.parsePrologError("Query timeout after 5000ms");
        expect(result.kind).toBe(PrologErrorKind.TIMEOUT);
        expect(result.message).toBe("Query timed out");
        expect(result.details?.timeoutMs).toBe(5000);
        expect(result.details?.raw).toBe("Query timeout after 5000ms");
      });

      test("should parse client timeout with different duration", () => {
        const result = PrologInterface.parsePrologError("Query timeout after  30000ms");
        expect(result.kind).toBe(PrologErrorKind.TIMEOUT);
        expect(result.message).toBe("Query timed out");
        expect(result.details?.timeoutMs).toBe(30000);
      });

      test("should handle malformed timeout duration", () => {
        const result = PrologInterface.parsePrologError("Query timeout after invalidms");
        expect(result.kind).toBe(PrologErrorKind.UNKNOWN);
        expect(result.message).toBe("Query timeout after invalidms");
      });

      test("should be case insensitive for timeout detection", () => {
        const result = PrologInterface.parsePrologError("QUERY TIMEOUT AFTER 1000MS");
        expect(result.kind).toBe(PrologErrorKind.TIMEOUT);
        expect(result.details?.timeoutMs).toBe(1000);
      });
    });

    describe("custom server error patterns", () => {
      test("should parse session_conflict", () => {
        const result = PrologInterface.parsePrologError("error(session_conflict(query, engine))");
        expect(result.kind).toBe(PrologErrorKind.SESSION_CONFLICT);
        expect(result.message).toBe("Session conflict: A query session is already active, cannot start engine session");
      });

      test("should parse invalid_query_syntax", () => {
        const result = PrologInterface.parsePrologError("error(invalid_query_syntax(parse_error))");
        expect(result.kind).toBe(PrologErrorKind.SYNTAX_ERROR);
        expect(result.message).toBe("Invalid query syntax");
      });

      test("should parse invalid_query_structure", () => {
        const result = PrologInterface.parsePrologError("error(invalid_query_structure(validation_error))");
        expect(result.kind).toBe(PrologErrorKind.SYNTAX_ERROR);
        expect(result.message).toBe("Invalid query structure");
      });

      test("should parse no_active_query", () => {
        const result = PrologInterface.parsePrologError("error(no_active_query)");
        expect(result.kind).toBe(PrologErrorKind.NO_ACTIVE_SESSION);
        expect(result.message).toBe("No active query session");
      });

      test("should parse no_active_engine", () => {
        const result = PrologInterface.parsePrologError("error(no_active_engine)");
        expect(result.kind).toBe(PrologErrorKind.NO_ACTIVE_SESSION);
        expect(result.message).toBe("No active engine session");
      });

      test("should parse undefined_predicate_in_query", () => {
        const result = PrologInterface.parsePrologError("error(undefined_predicate_in_query(foo/2, foo(X, Y)))");
        expect(result.kind).toBe(PrologErrorKind.EXISTENCE_ERROR);
        expect(result.message).toBe("Undefined predicate: foo/2");
        expect(result.details?.predicate).toBe("foo/2");
        expect(result.details?.goal).toBe("foo(X, Y)");
      });

      test("should parse nothing_to_retract", () => {
        const result = PrologInterface.parsePrologError("error(nothing_to_retract)");
        expect(result.kind).toBe(PrologErrorKind.EXISTENCE_ERROR);
        expect(result.message).toBe("No matching facts to retract");
      });

      test("should parse line_too_long", () => {
        const result = PrologInterface.parsePrologError("error(line_too_long(10000))");
        expect(result.kind).toBe(PrologErrorKind.QUERY_TOO_LARGE);
        expect(result.message).toBe("Query too large: 10000 characters exceeds limit");
      });

      test("should parse state_inconsistency", () => {
        const result = PrologInterface.parsePrologError("error(state_inconsistency)");
        expect(result.kind).toBe(PrologErrorKind.UNKNOWN);
        expect(result.message).toBe("Internal state inconsistency detected");
      });
    });

    describe("malformed inputs", () => {
      test("should handle non-error format as unknown", () => {
        const result = PrologInterface.parsePrologError("some random error message");
        expect(result.kind).toBe(PrologErrorKind.UNKNOWN);
        expect(result.message).toBe("some random error message");
        expect(result.details?.raw).toBe("some random error message");
      });

      test("should handle error with unrecognized content", () => {
        const result = PrologInterface.parsePrologError("error(unknown_error_type(details))");
        expect(result.kind).toBe(PrologErrorKind.UNKNOWN);
        expect(result.message).toBe("unknown_error_type(details)");
      });

      test("should handle empty string", () => {
        const result = PrologInterface.parsePrologError("");
        expect(result.kind).toBe(PrologErrorKind.UNKNOWN);
        expect(result.message).toBe("");
      });

      test("should handle whitespace-only string", () => {
        const result = PrologInterface.parsePrologError("   \n\t  ");
        expect(result.kind).toBe(PrologErrorKind.UNKNOWN);
        expect(result.message).toBe("");
      });
    });

    describe("edge cases", () => {
      test("should handle timeout with Number.isFinite safety", () => {
        // Test with a very large number that might not be finite
        const result = PrologInterface.parsePrologError("Query timeout after 999999999999999999999ms");
        expect(result.kind).toBe(PrologErrorKind.TIMEOUT);
        // Should handle the number safely
        expect(typeof result.details?.timeoutMs).toBe("number");
      });

      test("should trim whitespace from input", () => {
        const result = PrologInterface.parsePrologError("  error(no_active_query)  \n");
        expect(result.kind).toBe(PrologErrorKind.NO_ACTIVE_SESSION);
        expect(result.details?.raw).toBe("error(no_active_query)");
      });
    });
  });

  describe("formatPrologError", () => {
    describe("UNSAFE_GOAL formatting", () => {
      test("should format unsafe_goal with simple predicate", () => {
        const error = {
          kind: PrologErrorKind.UNSAFE_GOAL,
          message: "Security Error: Unsafe operation blocked",
          details: { goal: "system('rm -rf /')" }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Security Error: Operation blocked - contains dangerous predicate 'system'");
        expect(parsed.kind).toBe("unsafe_goal");
        expect(parsed.details).toEqual({ goal: "system('rm -rf /')" });
      });

      test("should handle module:predicate patterns", () => {
        const error = {
          kind: PrologErrorKind.UNSAFE_GOAL,
          message: "Security Error: Unsafe operation blocked",
          details: { goal: "knowledge_base:dangerous_pred(X)" }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Security Error: Operation blocked - contains dangerous predicate 'dangerous_pred'");
      });

      test("should handle complex module patterns", () => {
        const error = {
          kind: PrologErrorKind.UNSAFE_GOAL,
          message: "Security Error: Unsafe operation blocked",
          details: { goal: "user:system:file:dangerous(X)" }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Security Error: Operation blocked - contains dangerous predicate 'dangerous'");
      });

      test("should handle goal without parentheses", () => {
        const error = {
          kind: PrologErrorKind.UNSAFE_GOAL,
          message: "Security Error: Unsafe operation blocked",
          details: { goal: "dangerous_atom" }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Security Error: Unsafe operation blocked");
      });

      test("should handle missing goal details", () => {
        const error = {
          kind: PrologErrorKind.UNSAFE_GOAL,
          message: "Security Error: Unsafe operation blocked",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Security Error: Unsafe operation blocked");
      });
    });

    describe("TIMEOUT formatting", () => {
      test("should format timeout with duration", () => {
        const error = {
          kind: PrologErrorKind.TIMEOUT,
          message: "Query timed out",
          details: { timeoutMs: 5000 }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Query timed out after 5000ms. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.");
      });

      test("should format timeout without duration", () => {
        const error = {
          kind: PrologErrorKind.TIMEOUT,
          message: "Query timed out",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Query timed out. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.");
      });

      test("should handle non-finite timeout values", () => {
        const error = {
          kind: PrologErrorKind.TIMEOUT,
          message: "Query timed out",
          details: { timeoutMs: Infinity }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Query timed out. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.");
      });

      test("should handle invalid timeout values", () => {
        const error = {
          kind: PrologErrorKind.TIMEOUT,
          message: "Query timed out",
          details: { timeoutMs: "invalid" as any }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Query timed out. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.");
      });
    });

    describe("other error types", () => {
      test("should format PERMISSION_ERROR", () => {
        const error = {
          kind: PrologErrorKind.PERMISSION_ERROR,
          message: "Permission denied: Cannot execute directive",
          details: { operation: "execute" }
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Security Error: Directives are not allowed in sandboxed consult");
      });

      test("should format EXISTENCE_ERROR", () => {
        const error = {
          kind: PrologErrorKind.EXISTENCE_ERROR,
          message: "Undefined predicate: foo/2",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Undefined predicate: foo/2");
      });

      test("should format SYNTAX_ERROR", () => {
        const error = {
          kind: PrologErrorKind.SYNTAX_ERROR,
          message: "Invalid syntax",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Syntax Error: Invalid Prolog syntax. If using knowledge_base_assert_many with complex rules (containing :-), try knowledge_base_assert instead.");
      });

      test("should format SESSION_CONFLICT", () => {
        const error = {
          kind: PrologErrorKind.SESSION_CONFLICT,
          message: "Session conflict detected",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Session conflict detected");
      });

      test("should format NO_ACTIVE_SESSION", () => {
        const error = {
          kind: PrologErrorKind.NO_ACTIVE_SESSION,
          message: "No active session",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("No active session");
      });

      test("should format QUERY_TOO_LARGE", () => {
        const error = {
          kind: PrologErrorKind.QUERY_TOO_LARGE,
          message: "Query too large",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed.message).toBe("Query too large");
      });

      test("should format UNKNOWN", () => {
        const error = {
          kind: PrologErrorKind.UNKNOWN,
          message: "Unknown error occurred",
          details: {}
        };
        const result = PrologInterface.formatPrologError(error);
        const parsed = JSON.parse(result);
        expect(parsed).toEqual({
          kind: "unknown",
          message: "Unknown error occurred",
          details: {}
        });
      });
    });
  });
});
