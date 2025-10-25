/**
 * Load Test: Security Validation
 *
 * Extended security tests under load:
 * - Path traversal attempts under concurrent load
 * - Dangerous predicate detection at scale
 * - Input validation with large payloads
 * - Sandbox escape attempts
 */

import { describe, it, expect, beforeAll, afterAll } from "vitest";
import { prologInterface } from "@vpursuit/mcp-prolog";

describe("Security Validation Under Load", () => {
  beforeAll(async () => {
    await prologInterface.start();
  });

  afterAll(async () => {
    prologInterface.stop();
  });

  it("should block path traversal attempts under concurrent load", async () => {
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
      const promise = prologInterface.sendCommand(
        `consult('${path}').`,
        5000
      ).catch(error => ({ blocked: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should be blocked or error
    results.forEach(result => {
      expect(result.blocked || result.error).toBeTruthy();
    });
  }, 30000);

  it("should block dangerous predicates under load", async () => {
    const dangerousPredicates = [
      "shell('rm -rf /').",
      "system('cat /etc/passwd').",
      "call(halt).",
      "halt(0).",
      "load_foreign_library('/tmp/malicious.so').",
    ];

    const promises = [];

    for (let i = 0; i < 50; i++) {
      const predicate = dangerousPredicates[i % dangerousPredicates.length];
      const promise = prologInterface.sendCommand(predicate, 5000)
        .catch(error => ({ blocked: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All dangerous predicates should be blocked
    results.forEach(result => {
      expect(result.blocked).toBe(true);
    });
  }, 30000);

  it("should handle very long queries safely", async () => {
    // Create a query that's just under the limit
    const longQuery = `member(X, [${Array.from({ length: 800 }, (_, i) => i).join(",")}]), query_next(), !.`;

    // Should succeed
    const result = await prologInterface.sendCommand(longQuery, 10000);
    expect(result).toContain("@@SOLUTION_START@@");

    // Create a query that exceeds MAX_QUERY_LENGTH (5000 chars)
    const tooLongQuery = `member(X, [${Array.from({ length: 2000 }, (_, i) => i).join(",")}]), query_next(), !.`;

    // Should be rejected
    await expect(
      prologInterface.sendCommand(tooLongQuery, 10000)
    ).rejects.toThrow();
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

      const promise = prologInterface.sendCommand(
        `assertz(nested_${i}(${nestedList})).`,
        5000
      ).catch(error => ({ error: true, message: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // Some may succeed, but none should cause system instability
    expect(results).toHaveLength(20);

    // Cleanup
    for (let i = 0; i < 20; i++) {
      await prologInterface.sendCommand(
        `retractall(nested_${i}(_)).`,
        5000
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
      const promise = prologInterface.sendCommand(
        `member(X, ['${input}']), query_next(), !.`,
        5000
      ).catch(error => ({ error: true, message: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All malicious attempts should be caught
    results.forEach(result => {
      expect(result.error).toBe(true);
    });
  }, 30000);

  it("should prevent file system traversal via symbolic links", async () => {
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
      const promise = prologInterface.sendCommand(
        `consult('${path}').`,
        5000
      ).catch(error => ({ blocked: true }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should be blocked
    results.forEach(result => {
      expect(result.blocked).toBe(true);
    });
  }, 30000);

  it("should enforce timeout limits to prevent DoS", async () => {
    const promises = [];

    // Try to create expensive queries
    for (let i = 0; i < 20; i++) {
      const promise = prologInterface.sendCommand(
        `between(1, 1000000, X), between(1, 1000000, Y).`,
        1000 // Short timeout
      ).catch(error => ({ timedOut: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should timeout or fail
    results.forEach(result => {
      expect(result.timedOut).toBe(true);
    });
  }, 30000);

  it("should prevent code injection through atom manipulation", async () => {
    const injectionAttempts = [
      "atom_concat('system(\"', 'rm -rf /\")', Malicious)",
      "atom_codes('halt', Codes), atom_codes(Pred, Codes), call(Pred)",
      "term_to_atom(shell('malicious'), Atom)",
    ];

    const promises = [];

    for (let i = 0; i < 30; i++) {
      const attempt = injectionAttempts[i % injectionAttempts.length];
      const promise = prologInterface.sendCommand(
        `${attempt}, query_next(), !.`,
        5000
      ).catch(error => ({ blocked: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All injection attempts should fail
    results.forEach(result => {
      expect(result.blocked).toBe(true);
    });
  }, 30000);

  it("should maintain security under memory pressure", async () => {
    const promises = [];

    // Create memory pressure with large structures while attempting security violations
    for (let i = 0; i < 30; i++) {
      if (i % 3 === 0) {
        // Large structure
        const largeList = Array.from({ length: 1000 }, (_, j) => j).join(",");
        promises.push(
          prologInterface.sendCommand(
            `assertz(pressure_${i}([${largeList}])).`,
            5000
          )
        );
      } else {
        // Security violation attempt
        promises.push(
          prologInterface.sendCommand(
            `shell('echo vulnerable').`,
            5000
          ).catch(error => ({ blocked: true }))
        );
      }
    }

    const results = await Promise.all(promises);

    // Security violations should still be blocked
    const blockedAttempts = results.filter(r => r && r.blocked);
    expect(blockedAttempts.length).toBeGreaterThan(0);

    // Cleanup
    for (let i = 0; i < 30; i++) {
      await prologInterface.sendCommand(
        `retractall(pressure_${i}(_)).`,
        5000
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
      const promise = prologInterface.sendCommand(
        `assertz(special_${i}('${str}')).`,
        5000
      ).catch(error => ({ handled: true, error: error.message }));

      promises.push(promise);
    }

    const results = await Promise.all(promises);

    // All should be handled safely (either succeed or fail gracefully)
    expect(results).toHaveLength(30);

    // Cleanup
    for (let i = 0; i < 30; i++) {
      await prologInterface.sendCommand(
        `retractall(special_${i}(_)).`,
        5000
      ).catch(() => {});
    }
  }, 30000);
});
