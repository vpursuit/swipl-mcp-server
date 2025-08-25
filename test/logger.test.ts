import { logger } from "../src/logger.js";

describe("logger", () => {
  test("redactPath returns relative path when inside cwd", () => {
    const fake = process.cwd() + "/subdir/file.txt";
    const redacted = logger.redactPath(fake);
    expect(redacted).toBe("subdir/file.txt");
  });

  test("redactPath returns basename when outside cwd", () => {
    const redacted = logger.redactPath("/etc/passwd");
    expect(typeof redacted).toBe("string");
    expect(redacted.endsWith("passwd")).toBe(true);
  });
});
