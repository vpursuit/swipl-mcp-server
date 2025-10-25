/**
 * Runtime metadata utilities for locating files relative to the running module.
 * Ultra-simple approach: just try paths where files might be, in order.
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

// Only the essential constants we need
const PACKAGE_JSON = 'package.json';
const LICENSE_FILE = 'LICENSE';
const PROLOG_SCRIPT = 'prolog_server.pl';
const DEFAULT_VERSION = '0.0.0';

// Debug logging
function debugLog(message: string): void {
  const shouldLog = process.env.DEBUG?.includes('swipl-mcp-server') || 
                    process.env.SWI_MCP_TRACE === '1';
  
  if (shouldLog) {
    console.error(`[META DEBUG] ${message}`);
  }
}

/**
 * Check if a path exists safely.
 */
function pathExists(filePath: string): boolean {
  try {
    return fs.existsSync(filePath);
  } catch {
    return false;
  }
}

/**
 * Get our current directory (where meta.js is running from).
 */
function getCurrentDir(): string {
  // For ES modules, use import.meta.url directly
  try {
    if (import.meta && import.meta.url) {
      const moduleDir = path.dirname(fileURLToPath(import.meta.url));
      debugLog(`getCurrentDir() from import.meta.url: ${moduleDir}`);
      return moduleDir;
    }
  } catch (error) {
    debugLog(`import.meta.url failed: ${error}`);
  }

  // For CommonJS/Jest/npm (testing and some execution contexts)
  const scriptPath = process.argv[1];
  debugLog(`Process argv[1]: ${scriptPath}`);
  
  if (scriptPath) {
    // Check if we're in an npm package
    if (scriptPath.includes('node_modules/@vpursuit/swipl-mcp-server')) {
      // Extract the package lib directory from the full path
      const match = scriptPath.match(/(.*node_modules\/@vpursuit\/swipl-mcp-server)\/lib/);
      if (match) {
        const packageRoot = match[1];
        const libDir = path.join(packageRoot, 'lib');
        debugLog(`getCurrentDir() from npm package: ${libDir}`);
        return libDir;
      } else if (scriptPath.includes('/lib/')) {
        // Already in lib directory
        const libDir = path.dirname(scriptPath);
        debugLog(`getCurrentDir() from lib path: ${libDir}`);
        return libDir;
      }
    }
    
    // For local development and other cases
    const scriptDir = path.dirname(scriptPath);
    debugLog(`getCurrentDir() from script path: ${scriptDir}`);
    return scriptDir;
  }
  
  // Last resort fallback to current working directory
  const cwd = process.cwd();
  debugLog(`getCurrentDir() fallback to cwd: ${cwd}`);
  return cwd;
}

/**
 * Generate candidate paths for a filename in order of likelihood.
 */
function getFileCandidates(filename: string): string[] {
  const currentDir = getCurrentDir();
  
  // Try paths in order of likelihood:
  const candidates = [
    // Same directory (rare but possible)
    path.join(currentDir, filename),
    
    // Parent directory (most common - package root)
    path.join(currentDir, '..', filename),
    
    // Local development: src directory
    path.join(currentDir, '..', 'src', filename),
    
    // NPM package: prolog directory  
    path.join(currentDir, '..', 'prolog', filename),
    
    // Grandparent directory (fallback)
    path.join(currentDir, '..', '..', filename),
    
    // Local dev from build dir: go to root
    path.join(currentDir, '..', '..', filename),
    
    // Working directory fallback
    path.join(process.cwd(), filename),
    path.join(process.cwd(), 'src', filename),
  ];
  
  // Remove duplicates while preserving order
  return [...new Set(candidates)];
}

/**
 * Find any file by trying common locations where it might exist.
 * No context detection - just brute force search in logical order.
 */
function findFile(filename: string): string | null {
  debugLog(`=== findFile(${filename}) ===`);
  
  const currentDir = getCurrentDir();
  debugLog(`Current dir: ${currentDir}`);
  
  const uniqueCandidates = getFileCandidates(filename);
  
  for (const candidate of uniqueCandidates) {
    debugLog(`Trying: ${candidate}`);
    if (pathExists(candidate)) {
      debugLog(`Found: ${candidate}`);
      return candidate;
    }
  }
  
  debugLog(`Not found: ${filename}`);
  return null;
}

/** Get the process current working directory (safe). */
export function getCwd(): string {
  try { 
    const cwd = process.cwd();
    debugLog(`getCwd() = ${cwd}`);
    return cwd;
  } catch { 
    debugLog(`getCwd() failed, returning empty string`);
    return ""; 
  }
}

/**
 * Get the directory of the current module file.
 */
export function getModuleDir(): string {
  const currentDir = getCurrentDir();
  debugLog(`getModuleDir() = ${currentDir}`);
  return currentDir;
}

/** Get the directory of the Node entry script (process.argv[1]). */
export function getEntryDir(): string {
  try { 
    const entryPath = process.argv?.[1] || "";
    const entryDir = path.dirname(entryPath);
    debugLog(`getEntryDir() = ${entryDir}`);
    return entryDir;
  } catch (error) { 
    debugLog(`getEntryDir() error: ${error}`);
    return ""; 
  }
}

/**
 * Find a file using the simple "try everywhere" approach.
 */
export function findNearestFile(filename: string): string | null {
  return findFile(filename);
}

/**
 * Get the path to the Prolog server script.
 */
export function getPrologScriptPath(): string | null {
  return findFile(PROLOG_SCRIPT);
}

/**
 * Prolog script candidate generation (simplified).
 * Just returns the paths we would try in order.
 */
export function* prologScriptCandidates(): Generator<string> {
  debugLog(`=== prologScriptCandidates() ===`);
  
  for (const candidate of getFileCandidates(PROLOG_SCRIPT)) {
    debugLog(`Candidate: ${candidate}`);
    yield candidate;
  }
}

/** Resolve the application version from package.json. */
export function resolvePackageVersion(): string {
  const pkgPath = findFile(PACKAGE_JSON);
  
  if (pkgPath) {
    try {
      const raw = fs.readFileSync(pkgPath, "utf8");
      const pkg = JSON.parse(raw);
      
      if (pkg && typeof pkg.version === "string") {
        debugLog(`Resolved version: ${pkg.version} from ${pkgPath}`);
        return pkg.version;
      }
    } catch (error) {
      debugLog(`Error reading package.json: ${error}`);
    }
  }
  
  debugLog(`Version fallback: ${DEFAULT_VERSION}`);
  return DEFAULT_VERSION;
}

// Legacy exports for backward compatibility (UNUSED - can be removed)
export function resolvePath(fileType: 'package' | 'license' | 'prolog'): string | null {
  const fileMap = {
    'package': PACKAGE_JSON,
    'license': LICENSE_FILE,
    'prolog': PROLOG_SCRIPT,
  };
  return findFile(fileMap[fileType]);
}