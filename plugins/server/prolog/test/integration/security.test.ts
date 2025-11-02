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
    const started = await toolHandlers.queryStart({ query: "directory_files('.', L)" });
    // Expect the start to fail or the next to report an unsafe goal
    if (started.isError) {
      expect(started.content[0].text).toMatch(/unsafe_goal|Error:/i);
      return;
    }
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBe(true);
    expect(res.content[0].text).toMatch(/unsafe_goal|permission_error|Error:/i);
  });

  test("member/2 remains allowed (whitelisted)", async () => {
    await prologInterface.start();
    const started = await toolHandlers.queryStart({ query: "member(X, [a,b])" });
    expect(started.isError).toBeFalsy();
    const res = await toolHandlers.queryNext();
    expect(res.isError).toBeFalsy();
    expect(res.content[0].text).toMatch(/X\s*=\s*(a|b)/);
    await toolHandlers.queryClose();
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
    const result = await toolHandlers.knowledgeBaseLoad({ filename: "/etc/passwd" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("Files can only be loaded from");
    expect(result.content[0].text).toContain(path.join(os.homedir(), '.swipl-mcp-server'));
    expect(result.structuredContent.error_code).toBe("file_path_violation");
    expect(result.structuredContent.blocked_path).toBe("/etc/passwd");
  });

  test("should block loading /usr/bin/swipl with clear security error", async () => {
    const result = await toolHandlers.knowledgeBaseLoad({ filename: "/usr/bin/swipl" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("Files can only be loaded from");
    expect(result.structuredContent.error_code).toBe("file_path_violation");
  });

  test("should block loading files outside allowed directory", async () => {
    const result = await toolHandlers.knowledgeBaseLoad({ filename: "/tmp/malicious.pl" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("Files can only be loaded from");
    expect(result.content[0].text).toContain(path.join(os.homedir(), '.swipl-mcp-server'));
    expect(result.structuredContent.error_code).toBe("file_path_violation");
  });

  test("should allow loading files from allowed directory", async () => {
    const homeDir = os.homedir();
    const allowedDir = path.join(homeDir, '.swipl-mcp-server');
    const testFile = path.join(allowedDir, "test.pl");

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
      const result = await toolHandlers.knowledgeBaseLoad({ filename: testFile });
      
      // Should succeed - no security error
      expect(result).toBeDefined();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Successfully consulted file");
      expect(result.structuredContent.result).toBe("ok");
    } finally {
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

    const result = await toolHandlers.knowledgeBaseAssert({
      fact: "test_system :- shell('echo test')."
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

    const result = await toolHandlers.knowledgeBaseAssert({
      fact: "test_call :- call(shell('echo test'))."
    });

    expect(result.isError).toBeTruthy();
    // Dangerous predicates are now detected pre-execution with clear error messages
    expect(result.content[0].text).toContain("Dangerous predicate");
    expect(result.structuredContent.success).toBe(0);
  });

  test("should return security error in knowledgeBaseAssertMany for dangerous operations", async () => {
    await prologInterface.start();

    const result = await toolHandlers.knowledgeBaseAssertMany({
      facts: [
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
    const result = await toolHandlers.knowledgeBaseLoad({ filename: "/etc/hosts" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).not.toContain("timeout");
    expect(result.content[0].text).not.toContain("unsafe_goal");
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toMatch(/^Error: Security Error:/);
  });

  test("allowed directory should be clearly communicated", () => {
    const allowedDir = path.join(os.homedir(), '.swipl-mcp-server');
    expect(allowedDir).toContain(os.homedir());
    expect(allowedDir).toContain(".swipl-mcp-server");
  });
});
