/**
 * Executable finder utility for MCP servers and clients
 *
 * Provides a generalized way to locate executables during plugin initialization.
 * This is a core bootstrapping service used by plugins that need to spawn external processes.
 */

import { spawnSync } from "child_process";
import fs from "fs";
import path from "path";
import { redactPath } from "./path-utils.js";

/**
 * Options for finding an executable
 */
export interface FindExecutableOptions {
  /**
   * Name of the executable to find (e.g., 'swipl', 'python3', 'node')
   */
  name: string;

  /**
   * Platform-specific common installation paths to check as fallback.
   * These are checked only if the executable is not found in system PATH.
   *
   * @example
   * commonPaths: [
   *   '/opt/homebrew/bin/swipl',           // Homebrew on Apple Silicon
   *   '/usr/local/bin/swipl',              // Homebrew on Intel Mac
   *   'C:\\Program Files\\tool\\bin\\tool.exe'  // Windows
   * ]
   */
  commonPaths?: string[];

  /**
   * Arguments to pass to the executable for testing (default: ['--version'])
   * Used with spawnSync to verify the executable is functional.
   */
  testArgs?: string[];

  /**
   * Timeout in milliseconds for the spawn test (default: 1000ms)
   */
  timeout?: number;

  /**
   * Enable debug logging (default: checks DEBUG env var)
   * If not specified, logs only when DEBUG environment variable is set
   */
  debug?: boolean;
}

/**
 * Check if a file exists and is executable
 *
 * @param filePath - Path to check
 * @returns true if file exists and is executable
 */
function isExecutable(filePath: string): boolean {
  try {
    fs.accessSync(filePath, fs.constants.F_OK | fs.constants.X_OK);
    return true;
  } catch {
    return false;
  }
}

/**
 * Find an executable in system PATH or common installation locations.
 *
 * Search strategy:
 * 1. Try spawning the executable to test if it's in system PATH (most efficient)
 * 2. Check common installation directories (if provided)
 * 3. Return null if not found
 *
 * This approach lets the system handle PATH resolution correctly (including
 * Windows .exe extension, PATHEXT environment variable, symlinks, etc.)
 *
 * @param options - Configuration for finding the executable
 * @returns Path to executable if found, or null
 *
 * @example
 * ```typescript
 * const swiplPath = findExecutable({
 *   name: 'swipl',
 *   commonPaths: [
 *     '/opt/homebrew/bin/swipl',
 *     '/usr/local/bin/swipl',
 *     '/usr/bin/swipl'
 *   ]
 * });
 *
 * if (!swiplPath) {
 *   throw new Error('SWI-Prolog not found');
 * }
 * ```
 */
export function findExecutable(options: FindExecutableOptions): string | null {
  const {
    name,
    commonPaths = [],
    testArgs = ['--version'],
    timeout = 1000,
    debug = process.env['DEBUG'] !== undefined
  } = options;

  const log = (message: string) => {
    if (debug) {
      console.error(`[executable-finder] ${message}`);
    }
  };

  log(`=== findExecutable(${name}) ===`);

  // Step 1: Try spawning the executable to test if it's in system PATH
  // This is the most efficient and handles platform differences correctly
  log(`Testing if '${name}' is available in system PATH...`);
  try {
    const result = spawnSync(name, testArgs, {
      stdio: 'pipe',
      timeout,
      windowsHide: true
    });

    if (result.status === 0) {
      log(`Found '${name}' in system PATH`);
      return name;
    }
    log(`'${name}' not found in system PATH (exit status: ${result.status})`);
  } catch (error) {
    log(`Error testing '${name}' in PATH: ${error}`);
  }

  // Step 2: Check common installation locations
  if (commonPaths.length > 0) {
    log('Checking common installation locations...');
    for (const location of commonPaths) {
      log(`Checking: ${redactPath(location)}`);
      if (isExecutable(location)) {
        log(`Found '${name}' at: ${redactPath(location)}`);
        return location;
      }
    }
    log(`'${name}' not found in common locations`);
  }

  // Step 3: Not found anywhere
  log(`'${name}' not found`);
  return null;
}
