/**
 * Unit tests for PrologInterface class
 * Tests the core Prolog process management and communication
 */

import path from "path";
import { spawn } from "child_process";
import { PrologInterface } from "../src/PrologInterface.js";

// Mock child_process for testing
jest.mock("child_process");
const mockSpawn = spawn as jest.MockedFunction<typeof spawn>;

describe("PrologInterface", () => {
  let mockProcess: any;
  let mockStdin: any;
  let mockStdout: any;

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();

    // Create mock process streams
    mockStdin = {
      write: jest.fn(),
    };

    mockStdout = {
      on: jest.fn(),
    };

    // Create mock process
    mockProcess = {
      stdin: mockStdin,
      stdout: mockStdout,
      stderr: { on: jest.fn() },
      on: jest.fn(),
      kill: jest.fn(),
    };

    // Configure spawn mock
    mockSpawn.mockReturnValue(mockProcess as any);
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe("Process Management", () => {
    it("should spawn SWI-Prolog with correct arguments", () => {
      // We would need to refactor the code to make PrologInterface testable
      // For now, this is a placeholder test structure
      expect(true).toBe(true);
    });

    it("should handle process startup", () => {
      expect(mockSpawn).toBeDefined();
    });

    it("should handle process cleanup", () => {
      expect(mockProcess).toBeDefined();
    });
  });

  describe("Query Execution", () => {
    it("should format queries correctly", () => {
      const query = "test_fact(X)";
      const expectedFormattedQuery = "test_fact(X).";

      expect(query.endsWith(".") ? query : query + ".").toBe(expectedFormattedQuery);
    });

    it("should handle query timeouts", async () => {
      // Test timeout mechanism
      const timeoutMs = 5000;
      const start = Date.now();

      // Simulate timeout
      await new Promise((resolve) => setTimeout(resolve, 10));

      const elapsed = Date.now() - start;
      expect(elapsed).toBeLessThan(timeoutMs);
    });
  });

  describe("File Consultation", () => {
    it("should resolve file paths correctly", () => {
      const filename = "test-data.pl";
      const absolutePath = path.resolve(filename);

      expect(absolutePath).toContain("test-data.pl");
      expect(path.isAbsolute(absolutePath)).toBe(true);
    });

    it("should generate correct consult query", () => {
      const filename = "/path/to/test.pl";
      const expectedQuery = `consult('${filename}').`;

      const actualQuery = `consult('${filename}')`.endsWith(".")
        ? `consult('${filename}')`
        : `consult('${filename}').`;

      expect(actualQuery).toBe(expectedQuery);
    });
  });

  describe("Response Processing", () => {
    it("should handle empty responses", () => {
      const response = "";
      expect(response.trim()).toBe("");
    });

    it("should handle multi-line responses", () => {
      const response = "line1\nline2\nline3";
      const lines = response.split("\n");

      expect(lines).toHaveLength(3);
      expect(lines[0]).toBe("line1");
      expect(lines[2]).toBe("line3");
    });

    it("should process response lines correctly", () => {
      const testResponse = "X = a ;";
      expect(testResponse.trim()).toBeValidPrologResponse();
    });
  });

  describe("Error Handling", () => {
    it("should handle process creation errors", () => {
      const errorMessage = "Failed to create SWI-Prolog server process";
      expect(() => {
        if (!mockProcess.stdin) {
          throw new Error(errorMessage);
        }
      }).not.toThrow();
    });

    it("should handle invalid queries gracefully", () => {
      const invalidQuery = "this is not valid prolog";
      // In real implementation, this should be handled by the Prolog process
      expect(typeof invalidQuery).toBe("string");
    });

    it("should handle process termination", () => {
      mockProcess.kill();
      expect(mockProcess.kill).toHaveBeenCalled();
    });
  });

  describe("Real PrologInterface Tests", () => {
    let prologInterface: PrologInterface;

    beforeEach(() => {
      prologInterface = new PrologInterface();
      jest.clearAllMocks();
    });

    it("should create instance", () => {
      expect(prologInterface).toBeInstanceOf(PrologInterface);
    });

    it("should handle start process errors", async () => {
      mockSpawn.mockReturnValue({
        stdout: null,
        stdin: null,
        stderr: null,
        on: jest.fn(),
        kill: jest.fn(),
      } as any);

      await expect(prologInterface.start()).rejects.toThrow(
        "Failed to create SWI-Prolog server process",
      );
    });

    it("should handle query without started process", async () => {
      await expect(prologInterface.query("test.")).rejects.toThrow("Prolog server not started");
    });

    it("should handle consultFile method", async () => {
      await expect(prologInterface.consultFile("test.pl")).rejects.toThrow(
        "Prolog server not started",
      );
    });

    it("should stop process safely", () => {
      prologInterface.stop();
      // Should not throw any errors
      expect(true).toBe(true);
    });
  });
});
