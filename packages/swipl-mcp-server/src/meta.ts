/**
 * Runtime metadata utilities for the orchestrator package
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const PACKAGE_JSON = "package.json";
const DEFAULT_VERSION = "0.0.0";

/**
 * Check if a path exists safely
 */
function pathExists(filePath: string): boolean {
  try {
    return fs.existsSync(filePath);
  } catch {
    return false;
  }
}

/**
 * Get current directory where this module is running from
 */
function getCurrentDir(): string {
  try {
    if (import.meta?.url) {
      return path.dirname(fileURLToPath(import.meta.url));
    }
  } catch {
    // Fall through to fallback
  }

  // Fallback to process working directory
  return process.cwd();
}

/**
 * Find a file by searching common locations
 */
function findFile(filename: string): string | null {
  const currentDir = getCurrentDir();

  // Try paths in order of likelihood
  const candidates = [
    path.join(currentDir, filename),
    path.join(currentDir, "..", filename),
    path.join(currentDir, "..", "..", filename),
    path.join(process.cwd(), filename),
  ];

  for (const candidate of candidates) {
    if (pathExists(candidate)) {
      return candidate;
    }
  }

  return null;
}

/**
 * Resolve the package version from package.json
 */
export function resolvePackageVersion(): string {
  const pkgPath = findFile(PACKAGE_JSON);

  if (pkgPath) {
    try {
      const raw = fs.readFileSync(pkgPath, "utf8");
      const pkg = JSON.parse(raw);

      if (pkg?.version && typeof pkg.version === "string") {
        return pkg.version;
      }
    } catch {
      // Fall through to default
    }
  }

  return DEFAULT_VERSION;
}
