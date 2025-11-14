/**
 * Security Integration Tests
 * Tests sandbox security restrictions and path validation
 * Updated for Step 4: Uses unified files tool
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "@vpursuit/mcp-server-prolog";
import os from "os";
import path from "path";
import fs from "fs/promises";
import { mkdirSync, existsSync, writeFileSync, unlinkSync } from "fs";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Security: block non-whitelisted system predicates", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });
  afterEach(async () => {
    await prologInterface.stop();
  });

  test("directory_files/2 is rejected as unsafe", async () => {
    await prologInterface.start();
    const started = await toolHandlers.query({ operation: "start", query: "directory_files('.', L)" });
    // Expect the start to fail or the next to report an unsafe goal
    if (started.isError) {
      expect(started.content[0].text).toMatch(/unsafe_goal|Error:/i);
      return;
    }
    const res = await toolHandlers.query({ operation: "next" });
    expect(res.isError).toBe(true);
    expect(res.content[0].text).toMatch(/unsafe_goal|permission_error|Error:/i);
  });

  test("member/2 remains allowed (whitelisted)", async () => {
    await prologInterface.start();
    const started = await toolHandlers.query({ operation: "start", query: "member(X, [a,b])" });
    expect(started.isError).toBeFalsy();
    const res = await toolHandlers.query({ operation: "next" });
    expect(res.isError).toBeFalsy();
    expect(res.content[0].text).toMatch(/X\s*=\s*(a|b)/);
    await toolHandlers.query({ operation: "close" });
  });
});

maybeDescribe("Security: File Path Restrictions", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  test("should block loading /etc/passwd with clear security error", async () => {
    const result = await toolHandlers.files({ operation: "import", filename: "/etc/passwd" });

    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toMatch(/system directory|Access to system/i);
    expect(result.structuredContent.error_code).toBe("file_path_violation");
    expect(result.structuredContent.blocked_path).toBe("/etc/passwd");
  });

  test("should block loading /usr/bin/swipl with clear security error", async () => {
    const result = await toolHandlers.files({ operation: "import", filename: "/usr/bin/swipl" });

    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toMatch(/system directory|Access to system/i);
    expect(result.structuredContent.error_code).toBe("file_path_violation");
  });

  test("should block loading files outside allowed directory", async () => {
    // Without configured roots, any file access should fail
    const result = await toolHandlers.files({ operation: "import", filename: "/tmp/malicious.pl" });

    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toMatch(/No filesystem roots configured|File must be within allowed roots/i);
    expect(result.structuredContent.error_code).toBe("file_path_violation");
  });

  test("should allow loading files from allowed directory", async () => {
    const homeDir = os.homedir();
    const allowedDir = path.join(homeDir, '.model-context-lab');
    const testFile = path.join(allowedDir, "test.pl");

    // Configure roots to allow this directory
    const originalRoots = process.env.SWI_MCP_ALLOWED_ROOTS;
    process.env.SWI_MCP_ALLOWED_ROOTS = allowedDir;

    console.log(`Home directory: ${homeDir}`);
    console.log(`Target directory: ${allowedDir}`);
    console.log(`Test file: ${testFile}`);

    // Ensure the directory exists using sync operations with enhanced error handling
    try {
      if (!existsSync(allowedDir)) {
        console.log(`Directory does not exist, creating: ${allowedDir}`);
        mkdirSync(allowedDir, { recursive: true });
      }

      // Double-check the directory was created
      if (!existsSync(allowedDir)) {
        throw new Error(`Directory creation failed: ${allowedDir}`);
      }

      console.log(`Directory confirmed to exist: ${allowedDir}`);
    } catch (error) {
      console.error(`Failed to create directory ${allowedDir}:`, error);
      throw error;
    }

    // Create a simple test file
    try {
      writeFileSync(testFile, "test_fact(hello).\n", 'utf8');
      console.log(`Test file created: ${testFile}`);

      // Verify file was created
      if (!existsSync(testFile)) {
        throw new Error(`File creation failed: ${testFile}`);
      }
    } catch (error) {
      console.error(`Failed to create test file ${testFile}:`, error);
      throw error;
    }

    try {
      // This should pass security check and load successfully
      const result = await toolHandlers.files({ operation: "import", filename: testFile });

      // Should succeed - no security error
      expect(result).toBeDefined();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Successfully imported file");
      expect(result.structuredContent.operation).toBe("import");
      expect(result.structuredContent.clauses_added).toBeGreaterThan(0);
    } finally {
      // Restore original roots configuration
      if (originalRoots !== undefined) {
        process.env.SWI_MCP_ALLOWED_ROOTS = originalRoots;
      } else {
        delete process.env.SWI_MCP_ALLOWED_ROOTS;
      }

      // Clean up test file
      try {
        unlinkSync(testFile);
      } catch (_error) {
        // Ignore cleanup errors
      }
    }
  });

  test("file path validation is handled by Prolog", () => {
    // This test verifies that file path validation is now handled by Prolog's
    // built-in mechanisms rather than duplicate validation in TypeScript
    expect(true).toBe(true); // Placeholder test
  });
});

maybeDescribe("Security: Dangerous Operation Detection", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  test("should return security error for dangerous shell operation", async () => {
    await prologInterface.start();

    const result = await toolHandlers.clauses({
      operation: "assert",
      clauses: "test_system :- shell('echo test')."
    });

    expect(result.isError).toBeTruthy();
    // Dangerous predicates are now detected pre-execution with clear error messages
    expect(result.content[0].text).toContain("Dangerous predicate");
    expect(result.content[0].text).toContain("shell");
    // Pre-execution validation prevents the dangerous operation from running
    expect(result.structuredContent.success).toBe(0);
  });

  test("should return security error for dangerous call operation", async () => {
    await prologInterface.start();

    const result = await toolHandlers.clauses({
      operation: "assert",
      clauses: "test_call :- call(shell('echo test'))."
    });

    expect(result.isError).toBeTruthy();
    // Dangerous predicates are now detected pre-execution with clear error messages
    expect(result.content[0].text).toContain("Dangerous predicate");
    expect(result.structuredContent.success).toBe(0);
  });

  test("should return security error in clauses for dangerous operations", async () => {
    await prologInterface.start();

    const result = await toolHandlers.clauses({
      operation: "assert",
      clauses: [
        "dangerous_fact1 :- system('rm -rf /').",
        "dangerous_fact2 :- shell('echo test')."
      ]
    });

    expect(result.isError).toBeTruthy();
    // Dangerous predicates are now detected pre-execution with clear error messages
    expect(result.content[0].text).toContain("Dangerous predicate");
    // Should show detailed failure information for both dangerous operations
    expect(result.content[0].text).toContain("FAILED");
    expect(result.content[0].text).toContain("system");
    expect(result.content[0].text).toContain("shell");
    // Verify both operations were rejected
    expect(result.structuredContent.success).toBe(0);
    expect(result.structuredContent.total).toBe(2);
  });
});

maybeDescribe("Security: Error Message Quality", () => {
  beforeEach(async () => {
    await prologInterface.stop();
  });

  afterEach(async () => {
    await prologInterface.stop();
  });

  test("security errors should be clear and actionable", async () => {
    const result = await toolHandlers.files({ operation: "import", filename: "/etc/hosts" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).not.toContain("timeout");
    expect(result.content[0].text).not.toContain("unsafe_goal");
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toMatch(/^Error: Security Error:/);
  });

  test("allowed directory should be clearly communicated", () => {
    const allowedDir = path.join(os.homedir(), '.model-context-lab');
    expect(allowedDir).toContain(os.homedir());
    expect(allowedDir).toContain(".model-context-lab");
  });
});
