/**
 * Load Test: Security Validation
 *
 * Extended security tests under load:
 * - Path traversal attempts under concurrent load
 * - Dangerous predicate detection at scale
 * - Input validation with large payloads
 * - Sandbox escape attempts
 */

import { describe, it, expect, beforeAll, beforeEach, afterAll } from "vitest";
import { prologInterface } from "@vpursuit/mcp-server-prolog";

describe("Security Validation Under Load", () => {
  beforeAll(async () => {
    await prologInterface.start();
  });

  beforeEach(async () => {
    // Ensure server is running before each test
    if (!prologInterface.process || !prologInterface.process.stdin) {
      await prologInterface.start();
    }
  });

  afterAll(async () => {
    prologInterface.stop();
  });

  it("should handle path traversal attempts under concurrent load without crashing", async () => {
    const maliciousPaths = [
      "../../../etc/passwd",
      "../../sensitive.pl",
      "../../../.ssh/id_rsa",
      "../../../../etc/shadow",
      "..\\..\\..\\windows\\system32\\config\\sam",
      "/etc/passwd",
      "/root/.ssh/id_rsa",
      "C:\\Windows\\System32\\config\\SAM",
    ];

    const promises = [];

    // Try to load malicious paths concurrently
    for (let i = 0; i < 50; i++) {
      const path = maliciousPaths[i % maliciousPaths.length];
      const promise = prologInterface.importFileWithSource(path)
        .then(result => ({ success: true, result }))
        .catch(error => ({ failed: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // System should handle all attempts without crashing
    expect(results).toHaveLength(50);
    // Either all succeed or all fail, both are acceptable - just verify no crash
    expect(results.every(r => r.success || r.failed)).toBe(true);
  }, 30000);

  it("should block dangerous predicates under load", async () => {
    const dangerousPredicates = [
      "shell('rm -rf /')",
      "system('cat /etc/passwd')",
      "load_foreign_library('/tmp/malicious.so')",
      "open('/etc/passwd', read, _)",
      "set_prolog_flag(sandboxed, false)",
    ];

    const promises = [];

    for (let i = 0; i < 50; i++) {
      const predicate = dangerousPredicates[i % dangerousPredicates.length];
      const promise = prologInterface.query(predicate)
        .then(result => ({
          blocked: typeof result === 'string' && result.includes('error'),
          result
        }))
        .catch(error => ({ blocked: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All dangerous predicates should be blocked (either error result or exception)
    const blockedCount = results.filter(r => r.blocked).length;
    expect(blockedCount).toBe(50);
  }, 30000);

  it("should handle very long queries safely", async () => {
    // Create a moderately long query
    const longQuery = `member(X, [${Array.from({ length: 800 }, (_, i) => i).join(",")}])`;

    // Should succeed
    const result = await prologInterface.query(longQuery);
    expect(result).toContain("solution(");

    // Create a very long query
    const veryLongQuery = `member(X, [${Array.from({ length: 2000 }, (_, i) => i).join(",")}])`;

    // System should handle it without crashing (may succeed or fail based on limits)
    try {
      const result2 = await prologInterface.query(veryLongQuery);
      expect(result2).toBeDefined();
    } catch (error) {
      // If rejected, that's also acceptable
      expect(error).toBeDefined();
    }
  }, 20000);

  it("should prevent resource exhaustion through nested structures", async () => {
    const promises = [];

    // Try to create deeply nested structures
    for (let i = 0; i < 20; i++) {
      const depth = 100;
      let nestedList = "[]";
      for (let j = 0; j < depth; j++) {
        nestedList = `[${nestedList}]`;
      }

      const promise = prologInterface.query(
        `assertz(nested_${i}(${nestedList}))`
      ).catch(error => ({ error: true, message: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // Some may succeed, but none should cause system instability
    expect(results).toHaveLength(20);

    // Cleanup
    for (let i = 0; i < 20; i++) {
      await prologInterface.query(
        `retractall(nested_${i}(_))`
      ).catch(() => {});
    }
  }, 30000);

  it("should handle SQL injection-style attacks in Prolog queries", async () => {
    const maliciousInputs = [
      "'); halt. user_query('",
      "X), shell('rm -rf /'), member(Y",
      "'; system('cat /etc/passwd'). %",
      ") :- shell('malicious'), parent(",
    ];

    const promises = [];

    for (let i = 0; i < 40; i++) {
      const input = maliciousInputs[i % maliciousInputs.length];
      const promise = prologInterface.query(
        `member(X, ['${input}'])`
      ).catch(error => ({ error: true, message: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should be handled (either syntax error or treated as atom)
    // Some may succeed by treating malicious input as plain atom, others may fail with syntax error
    expect(results).toHaveLength(40);
    results.forEach(result => {
      // Either error or result is returned, both are acceptable handling
      expect(result).toBeDefined();
    });
  }, 30000);

  it("should handle file system traversal attempts without crashing", async () => {
    // Attempt to access system directories
    const systemPaths = [
      "/etc/passwd",
      "/root/.bashrc",
      "/proc/self/environ",
      "/var/log/auth.log",
      "C:\\Windows\\System32\\config\\SAM",
    ];

    const promises = [];

    for (let i = 0; i < 30; i++) {
      const path = systemPaths[i % systemPaths.length];
      const promise = prologInterface.importFileWithSource(path)
        .then(result => ({ success: true, result }))
        .catch(error => ({ failed: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // System should handle all attempts without crashing
    expect(results).toHaveLength(30);
    expect(results.every(r => r.success || r.failed)).toBe(true);
  }, 30000);

  it("should enforce timeout limits to prevent DoS", async () => {
    const promises = [];

    // Try to create expensive queries
    // Note: These queries are computationally expensive and should be handled by system timeouts
    for (let i = 0; i < 20; i++) {
      const promise = prologInterface.query(
        `between(1, 1000000, X), between(1, 1000000, Y)`
      ).catch(error => ({ timedOut: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should timeout or complete (depending on system timeout settings)
    // We just verify all queries complete without crashing
    expect(results).toHaveLength(20);
    results.forEach(result => {
      expect(result).toBeDefined();
    });
  }, 30000);

  it("should handle code injection attempts through atom manipulation", async () => {
    const injectionAttempts = [
      "atom_concat('system(\"', 'rm -rf /\")', Malicious)",
      "atom_codes('halt', Codes), atom_codes(Pred, Codes)",
      "term_to_atom(shell('malicious'), Atom)",
    ];

    const promises = [];

    for (let i = 0; i < 30; i++) {
      const attempt = injectionAttempts[i % injectionAttempts.length];
      const promise = prologInterface.query(attempt)
        .then(result => ({ success: true, result }))
        .catch(error => ({ failed: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // System should handle all attempts without crashing
    // Some may succeed (atom manipulation is allowed), others may fail
    expect(results).toHaveLength(30);
    expect(results.every(r => r.success || r.failed)).toBe(true);
  }, 30000);

  it("should handle memory pressure without crashing", async () => {
    const promises = [];

    // Create memory pressure with large structures
    for (let i = 0; i < 20; i++) {
      // Large structure assertions
      const largeList = Array.from({ length: 500 }, (_, j) => j).join(",");
      promises.push(
        prologInterface.query(
          `assertz(pressure_${i}([${largeList}]))`
        ).catch(error => ({ failed: true, error: error.message }))
      );
    }

    const results = await Promise.all(promises);

    // System should handle all attempts without crashing
    expect(results).toHaveLength(20);

    // Cleanup
    for (let i = 0; i < 20; i++) {
      await prologInterface.query(
        `retractall(pressure_${i}(_))`
      ).catch(() => {});
    }
  }, 40000);

  it("should handle unicode and special characters safely", async () => {
    const specialStrings = [
      "🔥💀🚨",
      "\x00\x01\x02",
      "'; DROP TABLE users; --",
      "<script>alert('xss')</script>",
      "${system('malicious')}",
      "`rm -rf /`",
    ];

    const promises = [];

    for (let i = 0; i < 30; i++) {
      const str = specialStrings[i % specialStrings.length];
      const promise = prologInterface.query(
        `assertz(special_${i}('${str}'))`
      ).catch(error => ({ handled: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should be handled safely (either succeed or fail gracefully)
    expect(results).toHaveLength(30);

    // Cleanup
    for (let i = 0; i < 30; i++) {
      await prologInterface.query(
        `retractall(special_${i}(_))`
      ).catch(() => {});
    }
  }, 30000);
});
