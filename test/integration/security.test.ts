import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { toolHandlers, prologInterface } from "../../src/tools.js";
import { validateFilePath, getAllowedDirectory } from "../../src/security.js";
import os from "os";
import path from "path";
import fs from "fs/promises";

const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("Security: block non-whitelisted system predicates", () => {
  beforeEach(() => {
    prologInterface.stop();
  });
  afterEach(() => {
    prologInterface.stop();
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
  beforeEach(() => {
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  test("should block loading /etc/passwd with clear security error", async () => {
    const result = await toolHandlers.dbLoad({ filename: "/etc/passwd" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("system directories is blocked");
    expect(result.content[0].text).toContain(getAllowedDirectory());
    expect(result.structuredContent.error).toBe("file_path_violation");
    expect(result.structuredContent.blocked_path).toBe("/etc/passwd");
  });

  test("should block loading /usr/bin/swipl with clear security error", async () => {
    const result = await toolHandlers.dbLoad({ filename: "/usr/bin/swipl" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("system directories is blocked");
    expect(result.structuredContent.error).toBe("file_path_violation");
  });

  test("should block loading files outside allowed directory", async () => {
    const result = await toolHandlers.dbLoad({ filename: "/tmp/malicious.pl" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("Files can only be loaded from");
    expect(result.content[0].text).toContain(getAllowedDirectory());
    expect(result.structuredContent.error).toBe("file_path_violation");
  });

  test("should allow loading files from allowed directory", async () => {
    const allowedDir = getAllowedDirectory();
    const testFile = path.join(allowedDir, "test.pl");
    
    // Create a simple test file
    await fs.writeFile(testFile, "test_fact(hello).\n", 'utf8');
    
    try {
      // This should pass security check and load successfully
      const result = await toolHandlers.dbLoad({ filename: testFile });
      
      // Should succeed - no security error
      expect(result).toBeDefined();
      expect(result.isError).toBeFalsy();
      expect(result.content[0].text).toContain("Successfully consulted file");
      expect(result.structuredContent.result).toBe("ok");
    } finally {
      // Clean up test file
      try {
        await fs.unlink(testFile);
      } catch (error) {
        // Ignore cleanup errors
      }
    }
  });

  test("validateFilePath function works correctly", () => {
    // Test system directory blocking
    const etcResult = validateFilePath("/etc/passwd");
    expect(etcResult.allowed).toBeFalsy();
    expect(etcResult.error?.type).toBe("file_path_violation");
    expect(etcResult.error?.message).toContain("system directories is blocked");

    // Test allowed directory
    const allowedFile = path.join(getAllowedDirectory(), "test.pl");
    const allowedResult = validateFilePath(allowedFile);
    expect(allowedResult.allowed).toBeTruthy();
    expect(allowedResult.error).toBeUndefined();
  });
});

maybeDescribe("Security: Dangerous Operation Detection", () => {
  beforeEach(() => {
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  test("should return security error for dangerous shell operation", async () => {
    await prologInterface.start();
    
    const result = await toolHandlers.dbAssert({ 
      fact: "test_system :- shell('echo test')." 
    });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("dangerous predicate");
    expect(result.content[0].text).toContain("shell");
    expect(result.structuredContent.error).toBe("dangerous_operation");
    expect(result.structuredContent.dangerous_predicate).toBe("shell");
  });

  test("should return security error for dangerous call operation", async () => {
    await prologInterface.start();
    
    const result = await toolHandlers.dbAssert({ 
      fact: "test_call :- call(shell('echo test'))." 
    });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("dangerous predicate");
    expect(result.structuredContent.error).toBe("dangerous_operation");
  });

  test("should return security error in dbAssertMany for dangerous operations", async () => {
    await prologInterface.start();
    
    const result = await toolHandlers.dbAssertMany({ 
      facts: [
        "dangerous_fact1 :- system('rm -rf /').",
        "dangerous_fact2 :- shell('echo test')."
      ]
    });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toContain("dangerous predicate");
    expect(result.structuredContent.success).toBe(0);
    expect(result.structuredContent.total).toBe(2);
  });
});

maybeDescribe("Security: Error Message Quality", () => {
  beforeEach(() => {
    prologInterface.stop();
  });

  afterEach(() => {
    prologInterface.stop();
  });

  test("security errors should be clear and actionable", async () => {
    const result = await toolHandlers.dbLoad({ filename: "/etc/hosts" });
    
    expect(result.isError).toBeTruthy();
    expect(result.content[0].text).not.toContain("timeout");
    expect(result.content[0].text).not.toContain("unsafe_goal");
    expect(result.content[0].text).toContain("Security Error");
    expect(result.content[0].text).toMatch(/^Security Error:/);
  });

  test("allowed directory should be clearly communicated", () => {
    const allowedDir = getAllowedDirectory();
    expect(allowedDir).toContain(os.homedir());
    expect(allowedDir).toContain(".swipl-mcp-server");
  });
});
