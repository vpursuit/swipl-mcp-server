/**
 * Path and file discovery utilities for MCP plugins
 *
 * Provides generic utilities for locating files, discovering module directories,
 * and handling paths securely across different deployment contexts.
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

/**
 * Redact absolute paths to avoid leaking filesystem structure in logs.
 * Returns relative path from cwd, or basename if outside cwd.
 *
 * This is a security utility that should be used whenever logging paths
 * to prevent exposing the host's filesystem structure.
 *
 * @param p - Path to redact
 * @returns Redacted path (relative or basename)
 *
 * @example
 * ```typescript
 * redactPath('/opt/homebrew/bin/swipl')  // → 'swipl'
 * redactPath('/home/user/project/src/file.ts')  // → 'src/file.ts' (if cwd is /home/user/project)
 * ```
 */
export function redactPath(p: string): string {
  try {
    const rel = path.relative(process.cwd(), p);
    return rel && !rel.startsWith("..") ? rel : path.basename(p);
  } catch {
    return "path";
  }
}

/**
 * Check if a path exists safely.
 * Returns false instead of throwing on any error.
 *
 * @param filePath - Path to check
 * @returns true if path exists, false otherwise
 */
export function pathExists(filePath: string): boolean {
  try {
    return fs.existsSync(filePath);
  } catch {
    return false;
  }
}

/**
 * Get the directory where the current module is running from.
 *
 * This function handles multiple execution contexts:
 * - Production: Uses process.argv[1] (entry script location)
 * - Test runners: Detects vitest/jest/mocha and uses process.cwd()
 * - Falls back to process.cwd() if script path is unavailable
 *
 * @returns Directory path of current module
 */
export function getCurrentDir(): string {
  // Use process.argv[1] to find the entry script location
  const scriptPath = process.argv[1];

  if (scriptPath) {
    // Detect test runner contexts where process.argv[1] points to the test framework
    // rather than our actual code. In these cases, use cwd instead.
    const isTestRunner = /\/(vitest|jest|mocha|ava|tap|node_modules\/(vitest|jest|mocha))/.test(scriptPath);

    if (isTestRunner) {
      // Test runner detected - use working directory
      // Tests are typically run from the repository root
      return process.cwd();
    }

    // Normal execution - use script directory
    return path.dirname(scriptPath);
  }

  // Last resort fallback to current working directory
  return process.cwd();
}

/**
 * Get the process current working directory (safe).
 * Returns empty string on error instead of throwing.
 *
 * @returns Current working directory or empty string
 */
export function getCwd(): string {
  try {
    return process.cwd();
  } catch {
    return "";
  }
}

/**
 * Get the directory of the current module file.
 * Alias for getCurrentDir() for semantic clarity.
 *
 * @returns Module directory path
 */
export function getModuleDir(): string {
  return getCurrentDir();
}

/**
 * Get the directory of the Node entry script (process.argv[1]).
 * Returns empty string on error.
 *
 * @returns Entry script directory or empty string
 */
export function getEntryDir(): string {
  try {
    const entryPath = process.argv?.[1] || "";
    return path.dirname(entryPath);
  } catch {
    return "";
  }
}

/**
 * Options for file search
 */
export interface FindFileOptions {
  /**
   * Base directory to start search from (default: getCurrentDir())
   */
  baseDir?: string;

  /**
   * Custom subdirectories to check (relative to baseDir)
   * @example ['prolog', 'scripts', 'resources']
   */
  customSubdirs?: string[];

  /**
   * Enable debug logging
   */
  debug?: boolean;
}

/**
 * Generate candidate paths for a filename in order of likelihood.
 *
 * Standard search pattern:
 * 1. Base directory
 * 2. Parent directory
 * 3. Parent/src
 * 4. Custom subdirectories (if provided)
 * 5. Grandparent directory
 * 6. Current working directory
 * 7. Current working directory/src
 *
 * @param filename - File to find
 * @param options - Search options
 * @returns Array of candidate paths (no duplicates)
 */
export function getFileCandidates(filename: string, options: FindFileOptions = {}): string[] {
  const baseDir = options.baseDir || getCurrentDir();
  const customSubdirs = options.customSubdirs || [];

  const candidates: string[] = [
    // Same directory (rare but possible)
    path.join(baseDir, filename),

    // Parent directory (most common - package root)
    path.join(baseDir, '..', filename),

    // Local development: src directory
    path.join(baseDir, '..', 'src', filename),
  ];

  // Add custom subdirectories
  // Check both from parent (production) and from baseDir itself (test contexts)
  for (const subdir of customSubdirs) {
    candidates.push(path.join(baseDir, '..', subdir, filename));
    candidates.push(path.join(baseDir, subdir, filename));
  }

  // Additional fallback locations
  candidates.push(
    // Grandparent directory (fallback)
    path.join(baseDir, '..', '..', filename),

    // Working directory fallback
    path.join(process.cwd(), filename),
    path.join(process.cwd(), 'src', filename)
  );

  // Remove duplicates while preserving order
  return [...new Set(candidates)];
}

/**
 * Find a file by trying common locations where it might exist.
 *
 * Searches in a logical order based on typical Node.js project structures.
 * Returns the first matching path found.
 *
 * @param filename - File to find
 * @param options - Search options
 * @returns Absolute path to file if found, null otherwise
 *
 * @example
 * ```typescript
 * const scriptPath = findFile('server.pl', {
 *   customSubdirs: ['prolog', 'scripts']
 * });
 * ```
 */
export function findFile(filename: string, options: FindFileOptions = {}): string | null {
  const log = (msg: string) => {
    if (options.debug && process.env['DEBUG']) {
      console.error(`[path-utils] ${msg}`);
    }
  };

  log(`=== findFile(${filename}) ===`);

  const candidates = getFileCandidates(filename, options);

  for (const candidate of candidates) {
    log(`Trying: ${redactPath(candidate)}`);
    if (pathExists(candidate)) {
      log(`Found: ${redactPath(candidate)}`);
      return candidate;
    }
  }

  log(`Not found: ${filename}`);
  return null;
}

/**
 * Find a file using the "try everywhere" approach.
 * Alias for findFile() for backward compatibility.
 *
 * @param filename - File to find
 * @param options - Search options
 * @returns Absolute path to file if found, null otherwise
 */
export function findNearestFile(filename: string, options: FindFileOptions = {}): string | null {
  return findFile(filename, options);
}

/**
 * Resolve the application version from package.json.
 *
 * Searches for package.json in common locations and extracts the version field.
 *
 * @param packageJsonName - Name of package.json file (default: 'package.json')
 * @param defaultVersion - Default version if not found (default: '0.0.0')
 * @param options - Search options
 * @returns Version string from package.json or default
 *
 * @example
 * ```typescript
 * const version = resolvePackageVersion();  // → "1.2.3"
 * ```
 */
export function resolvePackageVersion(
  packageJsonName = 'package.json',
  defaultVersion = '0.0.0',
  options: FindFileOptions = {}
): string {
  const log = (msg: string) => {
    if (options.debug && process.env['DEBUG']) {
      console.error(`[path-utils] ${msg}`);
    }
  };

  const pkgPath = findFile(packageJsonName, options);

  if (pkgPath) {
    try {
      const raw = fs.readFileSync(pkgPath, "utf8");
      const pkg = JSON.parse(raw);

      if (pkg && typeof pkg.version === "string") {
        log(`Resolved version: ${pkg.version} from ${redactPath(pkgPath)}`);
        return pkg.version;
      }
    } catch (error) {
      log(`Error reading ${packageJsonName}: ${error}`);
    }
  }

  log(`Version fallback: ${defaultVersion}`);
  return defaultVersion;
}
