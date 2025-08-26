/**
 * Runtime metadata utilities for locating files relative to the running module,
 * the entry script, and the current working directory. Centralizes path
 * resolution patterns and file discovery so behaviors are consistent across
 * ESM, CommonJS/Jest, local dev, and packaged builds.
 */

import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

/** Get the process current working directory (safe). */
export function getCwd(): string {
  try { return process.cwd(); } catch { return ""; }
}

/**
 * Get the directory of the current module file.
 * Uses a guarded access to import.meta.url to avoid parse-time errors under Jest/CJS.
 */
export function getModuleDir(): string {
  try {
    const metaUrl = Function(
      "try { return import.meta && import.meta.url } catch (_) { return '' }",
    )() as unknown as string;
    if (metaUrl && typeof metaUrl === "string") {
      return path.dirname(fileURLToPath(metaUrl));
    }
  } catch { }
  return "";
}

/** Get the directory of the Node entry script (process.argv[1]). */
export function getEntryDir(): string {
  try { return path.dirname(process.argv?.[1] || ""); } catch { return ""; }
}

/** Expand relative path patterns from a base directory. */
export function* candidatePathsFor(base: string, patterns: ReadonlyArray<ReadonlyArray<string>>): Generator<string> {
  for (const parts of patterns) {
    yield path.resolve(base, ...parts);
  }
}

/**
 * Generate candidates across multiple bases with deduplication, preserving order.
 */
export function* generateCandidates(bases: string[], patterns: ReadonlyArray<ReadonlyArray<string>>): Generator<string> {
  const seen = new Set<string>();
  for (const base of bases) {
    if (!base) continue;
    for (const p of candidatePathsFor(base, patterns)) {
      if (!seen.has(p)) { seen.add(p); yield p; }
    }
  }
}

/** Return the first path that exists from an iterable of candidate paths. */
export function findFirstExisting(paths: Iterable<string>): string | null {
  for (const p of paths) {
    try {
      fs.accessSync(p, fs.constants.F_OK);
      return p;
    } catch { }
  }
  return null;
}

/**
 * Find a file (e.g., package.json, LICENSE) by searching cwd, moduleDir, and
 * entryDir, and their parent directories up to two levels.
 */
export function findNearestFile(filename: string, bases?: string[]): string | null {
  const roots = (bases && bases.length ? bases : [getCwd(), getModuleDir(), getEntryDir()]).filter(Boolean) as string[];
  const levels: string[][] = [[], [".."], ["..", ".."]];
  const seen = new Set<string>();
  const candidates: string[] = [];
  for (const base of roots) {
    for (const lvl of levels) {
      const p = path.resolve(base, ...lvl, filename);
      if (!seen.has(p)) { seen.add(p); candidates.push(p); }
    }
  }
  return findFirstExisting(candidates);
}

/**
 * Prolog script candidate generation (no fs checks). Produces the prioritized
 * search order used by the server to locate prolog_server.pl or server.pl.
 */
export function* prologScriptCandidates(moduleDir: string, entryDir: string, mainName: string, altName: string): Generator<string> {
  const cwd = getCwd();
  const seen = new Set<string>();
  const push = function*(p: string) { if (!seen.has(p)) { seen.add(p); yield p; } };

  // cwd-based first
  yield* push(path.resolve(cwd, "src", mainName));
  yield* push(path.resolve(cwd, "..", "src", mainName));

  const patterns: ReadonlyArray<ReadonlyArray<string>> = [
    ["..", "src", mainName],
    [mainName],
    ["..", "..", "src", mainName],
    ["..", "prolog", altName],
  ];

  for (const p of generateCandidates([moduleDir, entryDir], patterns)) {
    yield* push(p);
  }
}

/** Resolve the application version from the nearest package.json. */
export function resolvePackageVersion(): string {
  const pkgPath = findNearestFile("package.json");
  if (pkgPath) {
    try {
      const raw = fs.readFileSync(pkgPath, "utf8");
      const pkg = JSON.parse(raw);
      if (pkg && typeof pkg.version === "string") return pkg.version;
    } catch { }
  }
  return "0.0.0";
}
