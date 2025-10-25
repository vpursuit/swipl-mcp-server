/**
 * Unit tests for knowledge_base_dump tool
 * Tests the database dump functionality with proper Prolog formatting
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-prolog";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Database Dump Tool", () => {
  beforeEach(() => {
    // Reset any existing state
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  describe("knowledge_base_dump tool", () => {
    test("should return empty message when knowledge base is empty", async () => {
      const result = await toolHandlers.knowledgeBaseDump();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("% No clauses in knowledge base");
      expect(result.structuredContent.dump).toBe("'% No clauses in knowledge base'");
    });

    test("should properly format facts using portray_clause", async () => {
      // Add some facts to the knowledge base
      await toolHandlers.knowledgeBaseAssert({ fact: "parent(john, mary)" });
      await toolHandlers.knowledgeBaseAssert({ fact: "parent(mary, alice)" });
      await toolHandlers.knowledgeBaseAssert({ fact: "age(john, 45)" });

      const result = await toolHandlers.knowledgeBaseDump();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Knowledge base dump:");
      expect(result.content[0].text).toContain("parent(john,mary)");
      expect(result.content[0].text).toContain("parent(mary,alice)");
      expect(result.content[0].text).toContain("age(john,45)");

      // Check that facts are properly formatted with periods
      expect(result.structuredContent.dump).toMatch(/parent\(john,mary\)\./);
      expect(result.structuredContent.dump).toMatch(/parent\(mary,alice\)\./);
      expect(result.structuredContent.dump).toMatch(/age\(john,45\)\./);
    });

    test("should properly format rules using portray_clause", async () => {
      // Add a rule to the knowledge base
      await toolHandlers.knowledgeBaseAssert({ fact: "parent(tom, bob)" });
      await toolHandlers.knowledgeBaseAssert({ fact: "parent(bob, charlie)" });
      await toolHandlers.knowledgeBaseAssert({ fact: "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)" });

      const result = await toolHandlers.knowledgeBaseDump();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Knowledge base dump:");
      expect(result.structuredContent.dump).toContain("parent(tom,bob).");
      expect(result.structuredContent.dump).toContain("parent(bob,charlie).");
      expect(result.structuredContent.dump).toMatch(/grandparent\(.*:-.*parent\(.*parent\(/);
    });

    test("should handle mixed facts and rules", async () => {
      // Add both facts and rules
      await toolHandlers.knowledgeBaseAssertMany({
        facts: [
          "likes(mary, food)",
          "likes(mary, wine)",
          "likes(john, wine)",
          "happy(X) :- likes(X, wine)"
        ]
      });

      const result = await toolHandlers.knowledgeBaseDump();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Knowledge base dump:");

      const dump = result.structuredContent.dump;
      expect(dump).toContain("likes(mary,food).");
      expect(dump).toContain("likes(mary,wine).");
      expect(dump).toContain("likes(john,wine).");
      expect(dump).toMatch(/happy\(.*:-.*likes\(/);
    });

    test("should include processing time in response", async () => {
      const result = await toolHandlers.knowledgeBaseDump();

      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toMatch(/Processing time: \d+ms/);
      expect(result.structuredContent.processing_time_ms).toBeGreaterThanOrEqual(0);
    });

    test("should work after asserting and retracting facts", async () => {
      // Add some facts
      await toolHandlers.knowledgeBaseAssert({ fact: "test_fact(a)" });
      await toolHandlers.knowledgeBaseAssert({ fact: "test_fact(b)" });

      // Check initial dump
      let result = await toolHandlers.knowledgeBaseDump();
      expect(result.structuredContent.dump).toContain("test_fact(a).");
      expect(result.structuredContent.dump).toContain("test_fact(b).");

      // Retract one fact
      await toolHandlers.knowledgeBaseRetract({ fact: "test_fact(a)" });

      // Check updated dump
      result = await toolHandlers.knowledgeBaseDump();
      expect(result.structuredContent.dump).not.toContain("test_fact(a).");
      expect(result.structuredContent.dump).toContain("test_fact(b).");
    });
  });
});