import { describe, test, expect, beforeEach, afterEach, vi } from "vitest";
import path from "path";
import os from "os";
import { RootsManager } from "../../src/utils/roots.js";
import type { RootDirectory, PathValidationResult } from "../../src/utils/roots.js";
import { ROOT_CACHE_TTL_MS, DEFAULT_FALLBACK_DIR, BLOCKED_SYSTEM_DIRS } from "../../src/constants.js";

describe("RootsManager", () => {
  let rootsManager: RootsManager;
  let mockServer: any;

  beforeEach(() => {
    // Reset singleton instance for each test
    (RootsManager as any).instance = null;
    rootsManager = RootsManager.getInstance();

    // Create mock server with nested server property to match actual structure
    // The actual implementation calls this.serverInstance.server.listRoots()
    const mockProtocol = {
      listRoots: vi.fn(),
      setNotificationHandler: vi.fn()
    };

    mockServer = {
      server: mockProtocol,
      getClientCapabilities: vi.fn()
    };

    // Clear environment variables
    delete process.env.SWI_MCP_ALLOWED_ROOTS;
    delete process.env.SWI_MCP_STRICT_ROOTS;
    delete process.env.SWI_MCP_USE_LEGACY_DIR;
    delete process.env.SWI_MCP_ROOTS_CACHE_TTL;
  });

  afterEach(() => {
    vi.clearAllMocks();
    // Reset environment variables
    delete process.env.SWI_MCP_ALLOWED_ROOTS;
    delete process.env.SWI_MCP_STRICT_ROOTS;
    delete process.env.SWI_MCP_USE_LEGACY_DIR;
    delete process.env.SWI_MCP_ROOTS_CACHE_TTL;
  });

  describe("Singleton Pattern", () => {
    test("should return same instance on multiple calls", () => {
      const instance1 = RootsManager.getInstance();
      const instance2 = RootsManager.getInstance();

      expect(instance1).toBe(instance2);
    });

    test("should maintain state across getInstance calls", async () => {
      const instance1 = RootsManager.getInstance();
      instance1.setServerInstance(mockServer);

      const instance2 = RootsManager.getInstance();
      // Both should reference the same internal state
      expect(instance1).toBe(instance2);
    });
  });

  describe("Server Instance Management", () => {
    test("should accept server instance", () => {
      expect(() => rootsManager.setServerInstance(mockServer)).not.toThrow();
    });

    test("should attempt notification registration when server set", () => {
      mockServer.getClientCapabilities = vi.fn().mockReturnValue({
        roots: { listChanged: true }
      });

      rootsManager.setServerInstance(mockServer);

      expect(mockServer.getClientCapabilities).toHaveBeenCalled();
    });

    test("should handle missing getClientCapabilities method gracefully", () => {
      const serverWithoutCaps = {
        server: {
          listRoots: vi.fn(),
          setNotificationHandler: vi.fn()
        }
      };

      expect(() => rootsManager.setServerInstance(serverWithoutCaps)).not.toThrow();
    });
  });

  describe("URI to Path Conversion", () => {
    test("should convert valid file:// URI to filesystem path", async () => {
      const testPath = "/tmp/test-project";
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: `file://${testPath}`, name: "Test Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const roots = await rootsManager.getRoots();
      expect(roots).toHaveLength(1);
      expect(roots[0].path).toBe(testPath);
    });

    test("should handle file:// URIs with special characters", async () => {
      const testPath = "/tmp/project with spaces";
      const encodedUri = `file://${encodeURI(testPath)}`;

      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: encodedUri, name: "Spaced Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const roots = await rootsManager.getRoots();
      expect(roots).toHaveLength(1);
      expect(roots[0].path).toBe(testPath);
    });

    test("should reject non-file:// URIs", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "http://example.com/path", name: "HTTP URI" },
          { uri: "file:///valid/path", name: "Valid" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const roots = await rootsManager.getRoots();
      // Should only include the valid file:// URI
      expect(roots).toHaveLength(1);
      expect(roots[0].name).toBe("Valid");
    });

    test("should handle malformed URIs gracefully", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "not-a-uri", name: "Malformed" },
          { uri: "file:///valid", name: "Valid" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const roots = await rootsManager.getRoots();
      expect(roots).toHaveLength(1);
      expect(roots[0].name).toBe("Valid");
    });
  });

  describe("Roots Discovery", () => {
    test("should discover roots from MCP server", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "file:///project1", name: "Project 1" },
          { uri: "file:///project2", name: "Project 2" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(true);
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(1);

      const roots = await rootsManager.getRoots();
      expect(roots).toHaveLength(2);
      expect(roots[0].path).toBe("/project1");
      expect(roots[1].path).toBe("/project2");
    });

    test("should return false when server instance not set", async () => {
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(false);
    });

    test("should return false when listRoots returns empty array", async () => {
      mockServer.server.listRoots.mockResolvedValue({ roots: [] });

      rootsManager.setServerInstance(mockServer);
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(false);
    });

    test("should return false when listRoots returns null", async () => {
      mockServer.server.listRoots.mockResolvedValue(null);

      rootsManager.setServerInstance(mockServer);
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(false);
    });

    test("should handle listRoots errors gracefully", async () => {
      mockServer.server.listRoots.mockRejectedValue(new Error("Network error"));

      rootsManager.setServerInstance(mockServer);
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(false);
    });

    test("should preserve root metadata (name)", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///workspace", name: "My Workspace" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const roots = await rootsManager.getRoots();
      expect(roots[0].name).toBe("My Workspace");
    });
  });

  describe("Cache Management", () => {
    test("should use cached roots within TTL period", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);

      // First call should fetch
      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(1);

      // Second call within TTL should use cache
      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(1);
    });

    test("should refresh cache after TTL expires", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);

      // First call
      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(1);

      // Manually expire cache by setting lastDiscoveryTime to past
      (rootsManager as any).lastDiscoveryTime = Date.now() - ROOT_CACHE_TTL_MS - 1000;

      // Second call should fetch again
      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(2);
    });

    test("should allow manual cache invalidation", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);

      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(1);

      // Invalidate cache manually
      rootsManager.invalidateCache();

      // Next call should fetch again
      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(2);
    });

    test("should not cache failed discovery attempts", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "http://invalid", name: "Invalid" }]
      });

      rootsManager.setServerInstance(mockServer);

      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(1);

      // Second call should re-fetch since no valid roots were cached
      await rootsManager.discoverRoots();
      expect(mockServer.server.listRoots).toHaveBeenCalledTimes(2);
    });
  });

  describe("Environment Variable Overrides", () => {
    test("should use SWI_MCP_ALLOWED_ROOTS when set", async () => {
      const customRoot1 = "/custom/root1";
      const customRoot2 = "/custom/root2";
      process.env.SWI_MCP_ALLOWED_ROOTS = `${customRoot1}:${customRoot2}`;

      rootsManager.setServerInstance(mockServer);
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(true);
      expect(mockServer.server.listRoots).not.toHaveBeenCalled();

      const roots = await rootsManager.getRoots();
      expect(roots).toHaveLength(2);
      expect(roots[0].path).toBe(path.resolve(customRoot1));
      expect(roots[1].path).toBe(path.resolve(customRoot2));
    });

    test("should filter empty paths from SWI_MCP_ALLOWED_ROOTS", async () => {
      process.env.SWI_MCP_ALLOWED_ROOTS = "/path1:::/path2";

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const roots = await rootsManager.getRoots();
      expect(roots).toHaveLength(2);
    });

    test("should enable legacy mode with SWI_MCP_USE_LEGACY_DIR", async () => {
      process.env.SWI_MCP_USE_LEGACY_DIR = "true";
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      const success = await rootsManager.discoverRoots();

      expect(success).toBe(false);
      expect(mockServer.server.listRoots).not.toHaveBeenCalled();
    });
  });

  describe("Path Validation - System Directories", () => {
    test("should block access to /etc directory", async () => {
      rootsManager.setServerInstance(mockServer);

      const result = await rootsManager.validatePath("/etc/passwd");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("system directory");
      expect(result.error).toContain("/etc");
    });

    test("should block access to /usr directory", async () => {
      const result = await rootsManager.validatePath("/usr/bin/bash");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("system directory");
    });

    test("should block access to /bin directory", async () => {
      const result = await rootsManager.validatePath("/bin/sh");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("system directory");
    });

    test("should block all configured system directories", async () => {
      for (const blockedDir of BLOCKED_SYSTEM_DIRS) {
        // Skip Windows paths on non-Windows platforms
        if (blockedDir.startsWith("C:") && os.platform() !== "win32") {
          continue;
        }

        const testPath = path.join(blockedDir, "test.pl");
        const result = await rootsManager.validatePath(testPath);

        expect(result.allowed).toBe(false);
        expect(result.error).toContain("system directory");
      }
    });

    test("should block subdirectories of system directories", async () => {
      const result = await rootsManager.validatePath("/etc/conf.d/test.pl");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("system directory");
    });
  });

  describe("Path Validation - Discovered Roots", () => {
    test("should allow paths within discovered roots", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///workspace/project", name: "My Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath("/workspace/project/file.pl");

      expect(result.allowed).toBe(true);
      expect(result.matchedRoot).toBeDefined();
      expect(result.matchedRoot?.path).toBe("/workspace/project");
    });

    test("should allow paths in subdirectories of roots", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath("/project/subdir/deep/file.pl");

      expect(result.allowed).toBe(true);
      expect(result.matchedRoot?.path).toBe("/project");
    });

    test("should reject paths outside discovered roots", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///allowed", name: "Allowed" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath("/not-allowed/file.pl");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("allowed roots");
      expect(result.error).toContain("/not-allowed/file.pl");
    });

    test("should check multiple roots in order", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "file:///project1", name: "Project 1" },
          { uri: "file:///project2", name: "Project 2" },
          { uri: "file:///project3", name: "Project 3" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result2 = await rootsManager.validatePath("/project2/file.pl");
      expect(result2.allowed).toBe(true);
      expect(result2.matchedRoot?.path).toBe("/project2");

      const result3 = await rootsManager.validatePath("/project3/file.pl");
      expect(result3.allowed).toBe(true);
      expect(result3.matchedRoot?.path).toBe("/project3");
    });

    test("should prevent path traversal outside roots", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///workspace/restricted", name: "Restricted" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      // Try to escape using ../
      const result = await rootsManager.validatePath("/workspace/restricted/../other/file.pl");

      expect(result.allowed).toBe(false);
    });
  });

  describe("Path Validation - Fallback Directory", () => {
    test("should allow paths in fallback directory", async () => {
      const fallbackDir = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);

      mockServer.server.listRoots.mockResolvedValue({ roots: [] });
      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath(path.join(fallbackDir, "file.pl"));

      expect(result.allowed).toBe(true);
      expect(result.matchedRoot?.name).toBe("Default Directory");
    });

    test("should include fallback in allowed paths list", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      const allowedPaths = await rootsManager.getAllowedPaths();

      const fallbackDir = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);
      expect(allowedPaths).toContain(fallbackDir);
    });

    test("should disable fallback for non-root paths with SWI_MCP_STRICT_ROOTS", async () => {
      process.env.SWI_MCP_STRICT_ROOTS = "true";

      mockServer.server.listRoots.mockResolvedValue({ roots: [] });
      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      // Fallback directory still works (checked before strict mode)
      const fallbackDir = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);
      const resultFallback = await rootsManager.validatePath(path.join(fallbackDir, "file.pl"));
      expect(resultFallback.allowed).toBe(true);

      // But other paths are blocked by strict mode
      const resultOther = await rootsManager.validatePath("/other/path/file.pl");
      expect(resultOther.allowed).toBe(false);
      expect(resultOther.error).toMatch(/allowed|configured/i); // Error message when no roots
      expect(resultOther.error).not.toContain("Default Directory"); // Strict mode excludes fallback from error message
    });
  });

  describe("Error Messages", () => {
    test("should list allowed roots in error message", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "file:///project1", name: "Project 1" },
          { uri: "file:///project2", name: "Project 2" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath("/forbidden/file.pl");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("/project1");
      expect(result.error).toContain("/project2");
      expect(result.error).toContain("Project 1");
      expect(result.error).toContain("Project 2");
      expect(result.error).toContain("Default Directory");
    });

    test("should show attempted path in error message", async () => {
      mockServer.server.listRoots.mockResolvedValue({ roots: [] });
      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const attemptedPath = "/unauthorized/access.pl";
      const result = await rootsManager.validatePath(attemptedPath);

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("Attempted:");
      expect(result.error).toContain(attemptedPath);
    });

    test("should provide helpful message when no roots configured", async () => {
      process.env.SWI_MCP_STRICT_ROOTS = "true";

      mockServer.server.listRoots.mockResolvedValue({ roots: [] });
      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath("/any/path.pl");

      expect(result.allowed).toBe(false);
      expect(result.error).toContain("No allowed directories configured");
    });
  });

  describe("Edge Cases", () => {
    test("should handle relative paths by resolving them", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: `file://${process.cwd()}`, name: "CWD" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath("./test.pl");

      expect(result.allowed).toBe(true);
    });

    test("should handle paths with .. segments correctly", async () => {
      const projectPath = "/workspace/project";
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: `file://${projectPath}`, name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      // Path with .. that resolves within root
      const result = await rootsManager.validatePath(`${projectPath}/subdir/../file.pl`);

      expect(result.allowed).toBe(true);
    });

    test("should handle root path matching exactly", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      // Exact root path match
      const result = await rootsManager.validatePath("/project/file.pl");

      expect(result.allowed).toBe(true);
    });

    test("should handle case-sensitive paths on Unix systems", async () => {
      if (os.platform() === "win32") return; // Skip on Windows

      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///Project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const resultLower = await rootsManager.validatePath("/project/file.pl");
      expect(resultLower.allowed).toBe(false);

      const resultCorrect = await rootsManager.validatePath("/Project/file.pl");
      expect(resultCorrect.allowed).toBe(true);
    });

    test("should handle empty path string", async () => {
      const result = await rootsManager.validatePath("");

      expect(result.allowed).toBe(false);
    });

    test("should handle very long paths", async () => {
      const longPath = "/project/" + "a/".repeat(100) + "file.pl";

      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      const result = await rootsManager.validatePath(longPath);

      expect(result.allowed).toBe(true);
    });
  });

  describe("Fallback Directory", () => {
    test("should return correct fallback directory path", () => {
      const fallback = rootsManager.getFallbackDir();
      const expected = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);

      expect(fallback).toBe(expected);
    });
  });

  describe("Allowed Paths API", () => {
    test("should return all allowed paths including fallback", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "file:///project1", name: "P1" },
          { uri: "file:///project2", name: "P2" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      const paths = await rootsManager.getAllowedPaths();

      expect(paths).toHaveLength(3); // 2 roots + fallback
      expect(paths).toContain("/project1");
      expect(paths).toContain("/project2");
      expect(paths).toContain(path.join(os.homedir(), DEFAULT_FALLBACK_DIR));
    });

    test("should not duplicate fallback if already in roots", async () => {
      const fallbackPath = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);

      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: `file://${fallbackPath}`, name: "Fallback" }]
      });

      rootsManager.setServerInstance(mockServer);
      const paths = await rootsManager.getAllowedPaths();

      // Count occurrences of fallback path
      const fallbackCount = paths.filter(p => p === fallbackPath).length;
      expect(fallbackCount).toBe(1);
    });
  });

  describe("Roots Metadata API", () => {
    test("should return roots with full metadata", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [
          { uri: "file:///workspace", name: "My Workspace" }
        ]
      });

      rootsManager.setServerInstance(mockServer);
      const roots = await rootsManager.getRoots();

      expect(roots).toHaveLength(1);
      expect(roots[0]).toEqual({
        uri: "file:///workspace",
        path: "/workspace",
        name: "My Workspace"
      });
    });

    test("should return copy of roots array to prevent mutation", async () => {
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project", name: "Project" }]
      });

      rootsManager.setServerInstance(mockServer);
      const roots1 = await rootsManager.getRoots();
      const roots2 = await rootsManager.getRoots();

      expect(roots1).not.toBe(roots2); // Different array instances
      expect(roots1).toEqual(roots2);  // But same content
    });
  });

  describe("Integration Scenarios", () => {
    test("should handle complete workflow: discover, validate, invalidate, re-discover", async () => {
      // Initial discovery
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project1", name: "Project 1" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      let result = await rootsManager.validatePath("/project1/file.pl");
      expect(result.allowed).toBe(true);

      // Change server response
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: "file:///project2", name: "Project 2" }]
      });

      // Invalidate and re-discover
      rootsManager.invalidateCache();
      await rootsManager.discoverRoots();

      result = await rootsManager.validatePath("/project1/file.pl");
      expect(result.allowed).toBe(false);

      result = await rootsManager.validatePath("/project2/file.pl");
      expect(result.allowed).toBe(true);
    });

    test("should handle graceful degradation when server unavailable", async () => {
      // No server set
      const fallbackDir = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);

      const result = await rootsManager.validatePath(path.join(fallbackDir, "file.pl"));

      expect(result.allowed).toBe(true);
    });

    test("should prioritize discovered roots over fallback", async () => {
      const customPath = "/custom/workspace";
      mockServer.server.listRoots.mockResolvedValue({
        roots: [{ uri: `file://${customPath}`, name: "Custom" }]
      });

      rootsManager.setServerInstance(mockServer);
      await rootsManager.discoverRoots();

      // Custom path should be allowed
      let result = await rootsManager.validatePath(`${customPath}/file.pl`);
      expect(result.allowed).toBe(true);
      expect(result.matchedRoot?.name).toBe("Custom");

      // Fallback should also work
      const fallbackPath = path.join(os.homedir(), DEFAULT_FALLBACK_DIR);
      result = await rootsManager.validatePath(path.join(fallbackPath, "file.pl"));
      expect(result.allowed).toBe(true);
      expect(result.matchedRoot?.name).toBe("Default Directory");
    });
  });
});
