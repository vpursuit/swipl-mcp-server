import { spawn, ChildProcess } from "child_process";
import path from "path";
import fs from "fs";
import { promises as fsPromises } from "fs";
import { logger } from "./logger.js";
import { findExecutable, findFile, getFileCandidates } from "@vpursuit/mcp-server-core";
import {
  DEFAULT_QUERY_TIMEOUT_MS,
  DEFAULT_READY_TIMEOUT_MS,
  CLEANUP_TIMEOUT_MS,
  STOP_KILL_DELAY_MS,
  STOP_MAX_WAIT_MS,
  READY_MARK,
  TERM_SOLUTION,
  TERM_ERROR,
  NO_MORE_SOLUTIONS,
  MAX_BUFFER_SIZE,
  MAX_QUERY_PROMISES,
  MAX_SOURCE_ENTRIES
} from "./constants.js";
import { SourceEntry } from "./types.js";

// Error type system
export enum PrologErrorKind {
  UNSAFE_GOAL = 'unsafe_goal',
  PERMISSION_ERROR = 'permission_error',
  SYNTAX_ERROR = 'syntax_error',
  EXISTENCE_ERROR = 'existence_error',
  INSTANTIATION_ERROR = 'instantiation_error',
  TIMEOUT = 'timeout',
  SESSION_CONFLICT = 'session_conflict',
  NO_ACTIVE_SESSION = 'no_active_session',
  QUERY_TOO_LARGE = 'query_too_large',
  UNKNOWN = 'unknown'
}

export interface PrologError {
  kind: PrologErrorKind;
  message: string;
  details?: {
    predicate?: string;
    file?: string;
    operation?: string;
    goal?: string;
    raw?: string;
    timeoutMs?: number;
  };
}

// Helper to parse boolean-like env flags
const isOn = (v?: string) => /^(1|true|yes)$/i.test(String(v || ""));

// Prolog-specific constants
const PROLOG_SCRIPT = 'prolog_server.pl';

/**
 * Find SWI-Prolog executable in PATH or common installation locations.
 * Wrapper around core findExecutable with Prolog-specific paths.
 */
function findSwiplExecutable(): string | null {
  const shouldDebug = process.env['DEBUG']?.includes('swipl-mcp-server') ||
                      process.env['SWI_MCP_TRACE'] === '1';

  return findExecutable({
    name: 'swipl',
    commonPaths: [
      '/opt/homebrew/bin/swipl',              // Homebrew on Apple Silicon Mac
      '/usr/local/bin/swipl',                 // Homebrew on Intel Mac, or standard Linux
      '/usr/bin/swipl',                       // System package manager (Linux)
      '/opt/local/bin/swipl',                 // MacPorts on Mac
      'C:\\Program Files\\swipl\\bin\\swipl.exe',       // Windows default
      'C:\\Program Files (x86)\\swipl\\bin\\swipl.exe', // Windows 32-bit
    ],
    debug: shouldDebug
  });
}

/**
 * Get the path to the Prolog server script (prolog_server.pl).
 * Searches in common plugin locations including test contexts.
 */
function getPrologScriptPath(): string | null {
  return findFile(PROLOG_SCRIPT, {
    customSubdirs: [
      'prolog',  // Production: parent/prolog directory
      'plugins/server/prolog/prolog',  // Test context: from repo root
      'products/swipl-mcp-server/prolog',  // Product tarball context
    ],
    debug: process.env['DEBUG']?.includes('swipl-mcp-server') || process.env['SWI_MCP_TRACE'] === '1'
  });
}

/**
 * Generator that yields candidate paths for the Prolog server script.
 * Useful for fallback searching with custom logic.
 */
function* prologScriptCandidates(): Generator<string> {
  const candidates = getFileCandidates(PROLOG_SCRIPT, {
    customSubdirs: [
      'prolog',
      'plugins/server/prolog/prolog',
      'products/swipl-mcp-server/prolog',
    ]
  });

  for (const candidate of candidates) {
    yield candidate;
  }
}

function findPrologServerScript(envPath: string | undefined, traceEnabled: boolean): string {
  // Check environment override first
  if (envPath) {
    const resolved = path.resolve(envPath);
    try {
      fs.accessSync(resolved, fs.constants.F_OK);
      logger.info(`Using Prolog server script from env: ${logger.redactPath(resolved)}`);
      return resolved;
    } catch {
      logger.warn(`Environment path not found: ${logger.redactPath(resolved)}`);
    }
  }

  // Use simplified path resolution
  const scriptPath = getPrologScriptPath();
  if (scriptPath && fs.existsSync(scriptPath)) {
    logger.info(`Found Prolog server script at: ${logger.redactPath(scriptPath)}`);
    return scriptPath;
  }

  // Fallback to candidate generation for compatibility
  for (const candidate of prologScriptCandidates()) {
    if (traceEnabled) {
      logger.debug(`Trying: ${logger.redactPath(candidate)}`);
    }
    try {
      fs.accessSync(candidate, fs.constants.F_OK);
      logger.info(`Found Prolog server script at: ${logger.redactPath(candidate)}`);
      return candidate;
    } catch {
      if (traceEnabled) logger.debug(`Not found at: ${logger.redactPath(candidate)}`);
    }
  }

  throw new Error(
    `Prolog server script not found. Set SWI_MCP_PROLOG_PATH to override.`,
  );
}

/**
 * Interface for managing SWI-Prolog process communication
 */
// Session state machine
//
// States:
// - "idle": no active session
// - "query": standard query session in progress
// - "query_completed": query exhausted; may only close
// - "engine": engine session in progress
// - "engine_completed": engine exhausted; may only close
// - "closing_query": transient while sending close_query
// - "closing_engine": transient while sending close_engine
//
// Allowed transitions (happy path and error path):
//   idle -> query
//   idle -> engine
//   query -> query_completed | closing_query | idle (on error)
//   query_completed -> closing_query | idle (on error)
//   closing_query -> idle
//   engine -> engine_completed | closing_engine | idle (on error)
//   engine_completed -> closing_engine | idle (on error)
//   closing_engine -> idle
//
// Notes:
// - Only one of query/engine modes can be active at a time.
// - "*_completed" retains context for consistent no-more-solutions responses.
// - Transient "closing_*" ensures serialized shutdown before new sessions.
export type SessionState =
  | "idle"
  | "query"
  | "query_completed"
  | "engine"
  | "engine_completed"
  | "closing_query"
  | "closing_engine";

const ALLOWED_TRANSITIONS: Record<SessionState, SessionState[]> = {
  idle: ["query", "engine", "idle"],
  query: ["query_completed", "closing_query", "idle"],
  query_completed: ["closing_query", "idle"],
  closing_query: ["idle"],
  engine: ["engine_completed", "closing_engine", "idle"],
  engine_completed: ["closing_engine", "idle"],
  closing_engine: ["idle"],
};

export class PrologInterface {
  private process: ChildProcess | null = null;
  private queryPromises: Map<string, { resolve: (value: string) => void; reject: (error: Error) => void }> = new Map();
  private queryCounter = 0;
  private isReady: boolean = false;
  private currentQuery: string | null = null;
  private queryActive: boolean = false;
  private engineActive: boolean = false;
  private engineReachedEOF: boolean = false;
  // Ensure only one command is in flight at a time
  private commandQueue: Promise<void> = Promise.resolve();
  // Session state guard to avoid races across transitions
  private sessionState: SessionState = "idle";
  // Health monitoring and circuit breaker
  private lastSuccessfulResponse: number | null = null;
  private consecutiveTimeouts: number = 0;
  private circuitState: 'closed' | 'open' | 'half-open' = 'closed';
  private recoveryInProgress: Promise<void> | null = null;
  private intentionalStop: boolean = false;
  // Source storage for preserving original clause text
  private kbSourceStorage: Map<string, SourceEntry> = new Map();
  private sourceIdCounter = 0;

  // Centralized state transition helper; logs invalid transitions for diagnostics
  private setSessionState(next: SessionState): void {
    const prev = this.sessionState;
    const allowed = ALLOWED_TRANSITIONS[prev] || [];
    if (!allowed.includes(next)) {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      const msg = `Invalid session state transition: ${prev} -> ${next}`;
      if (traceEnabled) {
        logger.warn(msg);
      }
    }
    this.sessionState = next;
  }

  // Public getter for session state (read-only access)
  public getSessionState(): SessionState {
    return this.sessionState;
  }

  /**
   * Start SWI-Prolog process
   */
  async start(): Promise<void> {
    if (this.process) {
      return;
    }

    const traceOn = isOn(process.env['SWI_MCP_TRACE']);
    if (traceOn) {
      logger.debug(`cwd: ${process.cwd()}`);
    }

    // Resolve Prolog server script: env override, cwd, and paths relative to module/entry
    const envPath = process.env['SWI_MCP_PROLOG_PATH'];
    const serverScript = findPrologServerScript(envPath, traceOn);

    // Reset readiness before starting
    this.isReady = false;

    // Reset circuit breaker if this is a fresh start (not recovery)
    // Recovery will set to half-open after calling start()
    if (this.circuitState !== 'half-open') {
      this.circuitState = 'closed';
      this.consecutiveTimeouts = 0;
      this.lastSuccessfulResponse = null;
    }

    const args = [
      "-q",
      "-s",
      serverScript,
      ...(traceOn ? ["-g", "assert(swi_mcp_trace_enabled)"] : []),
      "-g",
      "server_loop",
      "-t",
      "halt",
    ];

    // Find swipl executable in common locations
    const swiplExecutable = findSwiplExecutable();

    if (!swiplExecutable) {
      throw new Error(
        `SWI-Prolog executable not found!\n\n` +
        `Searched in:\n` +
        `  - System PATH\n` +
        `  - Common installation directories (Homebrew, MacPorts, system package managers)\n\n` +
        `Please install SWI-Prolog:\n` +
        `  macOS (Homebrew):       brew install swi-prolog\n` +
        `  macOS (MacPorts):       sudo port install swi-prolog\n` +
        `  Linux (Debian/Ubuntu):  sudo apt-get install swi-prolog\n` +
        `  Linux (Fedora):         sudo dnf install pl\n` +
        `  Windows:                https://www.swi-prolog.org/download/stable\n\n` +
        `After installation, restart the MCP server.`
      );
    }

    try {
      this.process = spawn(swiplExecutable, args, { stdio: ["pipe", "pipe", "pipe"] });
    } catch (error) {
      const err = error as NodeJS.ErrnoException;
      if (err.code === "ENOENT") {
        throw new Error(
          `Failed to start SWI-Prolog process. Please install SWI-Prolog from https://www.swi-prolog.org/download/stable`,
        );
      }
      throw new Error(`Failed to start SWI-Prolog process: ${err.message}`);
    }

    logger.info(`Started Prolog server (${logger.redactPid(this.process.pid)})`);

    if (!this.process.stdout || !this.process.stdin) {
      const e = new Error("Failed to create SWI-Prolog server process streams");
      await this.stop();
      throw e;
    }

    let failed = false;
    const failStartOnce = (err: Error) => {
      if (failed) return;
      failed = true;
      if (!this.isReady && this.readyRejecter) {
        const rej = this.readyRejecter;
        this.readyRejecter = null;
        try { rej(err); } catch { }
      }
    };

    // Single error handler
    this.process.on("error", (e: Error) => {
      const err = e as NodeJS.ErrnoException;
      const msg =
        err.code === "ENOENT"
          ? "SWI-Prolog executable not found in PATH"
          : `Prolog process error: ${err.message}`;
      logger.error(msg);
      failStartOnce(new Error(msg));
    });

    // Trace stderr when requested to aid diagnostics
    if (traceOn) {
      this.process.stderr?.on("data", (b: Buffer) => {
        const s = b.toString("utf8").trim();
        if (s) logger.debug(`Prolog stderr: ${s}`);
      });
    }

    this.process.stdout.on("data", (data: Buffer) => {
      this.handleResponse(data.toString("utf8"));
    });

    this.process.on("exit", (code, signal) => {
      this.process = null;
      // Reject any pending queries
      for (const [_id, promise] of this.queryPromises) {
        try { promise.reject(new Error("Prolog server exited")); } catch { }
      }
      this.queryPromises.clear();

      // If process exited unexpectedly (not during startup, explicit stop, or recovery), attempt auto-restart
      // Only auto-restart on crashes (non-zero exit or signals), not clean exits (code 0)
      // Disable auto-restart in test environments to avoid interfering with tests
      const isCleanExit = code === 0 && !signal;
      const isTestEnv = process.env.NODE_ENV === 'test' || typeof (globalThis as any).vitest !== 'undefined';
      const shouldAutoRestart = !isTestEnv && this.isReady && !this.recoveryInProgress && !this.intentionalStop && !isCleanExit;

      if (shouldAutoRestart) {
        const exitMsg = signal ? `signal ${signal}` : `code ${code}`;
        logger.warn(`Prolog server exited unexpectedly (${exitMsg}). Triggering auto-restart...`);
        // Trigger recovery in background to restart the process
        this.attemptRecovery().catch(err => {
          logger.error(`Auto-restart after unexpected exit failed: ${err instanceof Error ? err.message : String(err)}`);
        });
      } else {
        failStartOnce(new Error("Prolog server exited before ready"));
      }
    });

    // Wait for READY signal from Prolog server, clean up on failure
    try {
      await this.waitForReady();
    } catch (e) {
      await this.stop();
      throw e;
    }
  }

  private inputBuffer: string = "";
  private readyPromise: Promise<void> | null = null;
  private readyResolver: (() => void) | null = null;
  private readyRejecter: ((err: Error) => void) | null = null;

  /**
   * Wait for Prolog server to signal readiness
   */
  private async waitForReady(): Promise<void> {
    // If we've already seen the READY signal, resolve immediately
    if (this.isReady) {
      return Promise.resolve();
    }

    if (!this.readyPromise) {
      const readyTimeoutMs = Number.parseInt(process.env['SWI_MCP_READY_TIMEOUT_MS'] || String(DEFAULT_READY_TIMEOUT_MS), 10);
      const timeout = Number.isFinite(readyTimeoutMs) && readyTimeoutMs > 0 ? readyTimeoutMs : DEFAULT_READY_TIMEOUT_MS;
      this.readyPromise = new Promise((resolve, reject) => {
        this.readyResolver = resolve;
        this.readyRejecter = (err: Error) => {
          if (this.readyResolver) {
            this.readyResolver = null;
          }
          const rej = reject;
          this.readyRejecter = null;
          try {
            rej(err);
          } catch { }
        };

        const timer: NodeJS.Timeout = setTimeout(() => {
          if (this.readyResolver) {
            this.readyResolver = null;
            this.readyRejecter = null;
            reject(new Error("Prolog server ready timeout"));
          }
        }, timeout);
        // Avoid keeping the event loop alive in tests
        timer.unref?.();
      });
    }

    return this.readyPromise;
  }

  /**
   * Handle response from Prolog server (line-based)
   */
  private handleResponse(data: string): void {
    this.inputBuffer += data;

    // Buffer overflow protection
    if (this.inputBuffer.length > MAX_BUFFER_SIZE) {
      logger.error(`Input buffer exceeded ${MAX_BUFFER_SIZE} bytes; truncating`);
      // Keep only the last MAX_BUFFER_SIZE bytes to avoid losing recent data
      this.inputBuffer = this.inputBuffer.slice(-MAX_BUFFER_SIZE);
    }

    // Process complete lines using indexOf to avoid O(n²) split/pop
    let idx = 0;
    while (true) {
      const nl = this.inputBuffer.indexOf("\n", idx);
      if (nl === -1) break;
      const line = this.inputBuffer.slice(idx, nl);
      idx = nl + 1;
      if (line.trim()) {
        this.processResponseLine(line.trim());
      }
    }
    // Preserve any remaining incomplete line
    this.inputBuffer = this.inputBuffer.slice(idx);
  }

  private assertRunning(): void {
    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }
  }

  private formatBindingsString(inner: string): string {
    const cleaned = inner.replace(/'([A-Za-z_][A-Za-z0-9_]*)'=/g, "$1=");
    return cleaned.trim() === "[]" ? "true" : cleaned;
  }

  // Normalize a server result string into a structured tag
  private parseServerResult(result: string):
    | { kind: "eof" }
    | { kind: "error"; error: string }
    | { kind: "solution"; value: string }
    | { kind: "other"; value: string } {
    if (result === NO_MORE_SOLUTIONS) return { kind: "eof" };
    if (result.startsWith(TERM_ERROR)) return { kind: "error", error: result };
    if (result.startsWith(TERM_SOLUTION)) {
      const m = result.match(/^solution\((.*)\)$/);
      if (m) return { kind: "solution", value: this.formatBindingsString(m[1]) };
      return { kind: "solution", value: result };
    }
    return { kind: "other", value: result };
  }

  /**
   * Process a complete response line from Prolog server
   */
  private processResponseLine(response: string): void {
    // Ignore internal debug markers from the Prolog server
    if (response.startsWith("@@DEBUG@@")) {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      if (traceEnabled) logger.debug(`Prolog debug: ${response.slice(9).trim()}`);
      return;
    }
    // Avoid logging sensitive response bodies in normal operation
    // Log a summary at debug level only
    {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      if (traceEnabled) {
        const tag =
          response === READY_MARK
            ? "READY"
            : response.startsWith(TERM_SOLUTION)
              ? "solution"
              : response.startsWith(TERM_ERROR)
                ? "error"
                : response === NO_MORE_SOLUTIONS
                  ? "eof"
                  : "other";
        logger.debug(`Prolog response: ${tag}`);
      }
    }

    // Check for READY signal
    if (response === READY_MARK) {
      this.isReady = true;
      if (this.readyResolver) {
        this.readyResolver();
        this.readyResolver = null;
      }
      return;
    }

    // Prefer correlation-id routing if present: id(ID, Payload)
    const idMatch = response.match(/^id\((\d+),\s*(.*)\)$/);
    if (idMatch) {
      const id = idMatch[1];
      const payload = idMatch[2];
      const entry = this.queryPromises.get(id);
      if (entry) {
        this.queryPromises.delete(id);
        entry.resolve(payload);
        return;
      } else {
        // Unknown or late response for an ID we no longer track; drop to avoid misrouting
        const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
        if (traceEnabled) logger.debug(`Dropping late or unknown id(${id}, ...) response`);
        return;
      }
    }
    // Fallback to FIFO for truly untagged responses
    const queryIds = Array.from(this.queryPromises.keys());
    if (queryIds.length > 0) {
      const queryId = queryIds[0]; // FIFO
      const queryData = this.queryPromises.get(queryId)!;
      this.queryPromises.delete(queryId);
      queryData.resolve(response);
    }
  }

  /**
   * Start a new query session
   * Automatically closes any active query or engine session before starting.
   */
  async startQuery(query: string): Promise<{ status: string }> {
    // Auto-close active query session if present
    if (
      this.queryActive ||
      this.sessionState === "query" ||
      this.sessionState === "closing_query"
    ) {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      if (traceEnabled) {
        logger.info("Auto-closing active query session before starting new query");
      }
      try {
        await this.closeQuery();
      } catch (error) {
        logger.warn(`Auto-close of query failed: ${error instanceof Error ? error.message : String(error)}`);
        // Reset state to allow new query to start
        this.queryActive = false;
        this.currentQuery = null;
        this.setSessionState("idle");
      }
    }
    // Auto-close active engine session if present
    if (
      this.engineActive ||
      this.sessionState === "engine" ||
      this.sessionState === "closing_engine"
    ) {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      if (traceEnabled) {
        logger.info("Auto-closing active engine session before starting new query");
      }
      try {
        await this.closeEngine();
      } catch (error) {
        logger.warn(`Auto-close of engine failed: ${error instanceof Error ? error.message : String(error)}`);
        // Reset state to allow new query to start
        this.engineActive = false;
        this.engineReachedEOF = false;
        this.setSessionState("idle");
      }
    }

    this.assertRunning();

    this.currentQuery = query;
    this.queryActive = true;
    this.setSessionState("query");

    // Send start_query_string command to server using proper string escaping
    const escapedQuery = this.escapeQueryString(query);
    const result = await this.sendCommand(`start_query_string("${escapedQuery}")`);
    // If Prolog responded with an error(...) term, reject
    if (typeof result === "string" && result.startsWith(TERM_ERROR)) {
      this.queryActive = false;
      this.currentQuery = null;
      this.setSessionState("idle");
      const parsedError = PrologInterface.parsePrologError(result);
      throw new Error(PrologInterface.formatPrologError(parsedError));
    }
    // The unified server returns 'ok' on success
    return {
      status: "ready",
    };
  }

  /**
   * Get the next solution from current query
   */
  async nextSolution(): Promise<{ solution: string | null; status: "success" | "done"; error?: string }> {
    if (!this.queryActive && this.sessionState !== "query_completed") {
      return { solution: null, status: "done", error: "No active query. Start a query first." };
    }

    // If query is already completed, return consistent "no more solutions" message
    if (this.sessionState === "query_completed") {
      return { solution: null, status: "done" };
    }

    this.assertRunning();

    try {
      const result = await this.sendCommand("next_solution");
      const parsed = this.parseServerResult(result);
      if (parsed.kind === "eof") {
        // Keep query info but mark as completed instead of clearing everything
        this.queryActive = false;
        this.setSessionState("query_completed");
        return { solution: null, status: "done" };
      }
      if (parsed.kind === "error") {
        this.queryActive = false;
        this.currentQuery = null;
        this.setSessionState("idle");
        const parsedError = PrologInterface.parsePrologError(parsed.error);
        return { solution: null, status: "done", error: PrologInterface.formatPrologError(parsedError) };
      }
      if (parsed.kind === "solution") {
        return { solution: parsed.value, status: "success" };
      }
      return { solution: parsed.value, status: "success" };
    } catch (error) {
      this.queryActive = false;
      this.currentQuery = null;
      this.setSessionState("idle");
      const errorMessage = error instanceof Error ? error.message : String(error);
      const parsedError = PrologInterface.parsePrologError(errorMessage);
      return {
        solution: null,
        status: "done",
        error: PrologInterface.formatPrologError(parsedError),
      };
    }
  }

  /**
   * Close the current query session
   */
  async closeQuery(): Promise<{ status: string }> {
    if (!this.queryActive && this.sessionState !== "query_completed") {
      return { status: "no_active_query" };
    }

    this.assertRunning();

    try {
      this.setSessionState("closing_query");
      // Use cleanup timeout for close operations
      await this.sendCommand("close_query", { isCleanup: true });
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      // If cleanup times out, process may be stuck - trigger recovery
      if (errorMsg.includes("Cleanup operation timeout")) {
        logger.error("Query cleanup timeout detected - process may be stuck. Triggering recovery...");
        // Don't await - let recovery happen in background
        this.attemptRecovery().catch(err => {
          logger.error(`Recovery after cleanup timeout failed: ${err instanceof Error ? err.message : String(err)}`);
        });
      }
      // Continue with state cleanup even if command failed
    }

    this.queryActive = false;
    this.currentQuery = null;
    this.setSessionState("idle");

    return { status: "closed" };
  }

  /**
   * Send a single command to Prolog server (legacy method for assert/retract/consult)
   */
  async query(query: string): Promise<string> {
    const result = await this.sendCommand(query);

    // Check if result is an error term and throw
    if (typeof result === "string" && result.startsWith(TERM_ERROR)) {
      const parsedError = PrologInterface.parsePrologError(result);
      throw new Error(PrologInterface.formatPrologError(parsedError));
    }

    return result;
  }

  /**
   * Internal method to send commands to Prolog server
   */
  private async sendCommand(command: string, options?: { isCleanup?: boolean }): Promise<string> {
    this.assertRunning();

    const run = (): Promise<string> => {
      // Wait for recovery to complete if in progress
      const recovery = this.recoveryInProgress;
      if (recovery) {
        logger.info('Waiting for recovery to complete before processing query...');
        return recovery.then(() => this.sendCommand(command, options));
      }

      // Enforce queue depth limit to prevent memory exhaustion
      if (this.queryPromises.size >= MAX_QUERY_PROMISES) {
        logger.error(`Queue overflow: ${this.queryPromises.size} pending commands`);
        return Promise.reject(new Error(`Queue is full (${MAX_QUERY_PROMISES} pending queries). Server may be overloaded or unresponsive.`));
      }

      // Circuit breaker: fast-fail if circuit is open
      if (this.circuitState === 'open') {
        logger.warn('Circuit breaker is open - rejecting query fast');
        return Promise.reject(new Error('Service temporarily unavailable. Server is recovering from errors.'));
      }

      const queryIdNum = this.queryCounter++;
      const queryId = String(queryIdNum);

      let timer: NodeJS.Timeout | null = null;

      return new Promise<string>((resolve, reject) => {
        let stdinRef: (NodeJS.WritableStream & { removeListener?: Function }) | null = null;
        let onWriteErrorRef: ((err: NodeJS.ErrnoException) => void) | null = null;
        const finish = () => {
          if (timer) {
            try { clearTimeout(timer); } catch { }
            timer = null;
          }
          // Detach error listener if still attached
          if (stdinRef && onWriteErrorRef) {
            try { (stdinRef as NodeJS.WritableStream & { removeListener?: Function })?.removeListener?.("error", onWriteErrorRef); } catch { }
          }
          stdinRef = null;
          onWriteErrorRef = null;
        };

        const resolveAndFinish = (value: string) => {
          // Track successful response for health monitoring
          this.lastSuccessfulResponse = Date.now();
          // Reset circuit breaker on successful query (unless in half-open test phase)
          if (this.circuitState === 'closed' || this.circuitState === 'half-open') {
            if (this.consecutiveTimeouts > 0) {
              logger.info(`Resetting timeout counter (was ${this.consecutiveTimeouts}) after successful query`);
            }
            this.consecutiveTimeouts = 0;
            if (this.circuitState === 'half-open') {
              this.circuitState = 'closed';
              logger.info('Circuit breaker closed - service recovered');
            }
          }
          finish();
          resolve(value);
        };
        const rejectAndFinish = (err: Error | string | unknown) => {
          finish();
          reject(err instanceof Error ? err : new Error(String(err)));
        };

        this.queryPromises.set(queryId, { resolve: resolveAndFinish, reject: rejectAndFinish });

        // Send command to server
        const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
        const envelope = `cmd(${queryIdNum}, ${command})`;
        if (traceEnabled) logger.debug(`Send command: ${envelope}`);
        try {
          if (!this.process || !this.process.stdin) {
            this.queryPromises.delete(queryId);
            return rejectAndFinish(new Error("Prolog process not available"));
          }
          const stdin = this.process.stdin as NodeJS.WritableStream & { removeListener?: Function };
          stdinRef = stdin;
          const onWriteError = (err: NodeJS.ErrnoException) => {
            try { (stdin as NodeJS.WritableStream & { removeListener?: Function })?.removeListener?.("error", onWriteError); } catch { }
            this.queryPromises.delete(queryId);
            const error = err as NodeJS.ErrnoException;
            if (error?.code === "EPIPE" || String(error?.message || "").includes("EPIPE")) {
              return rejectAndFinish(new Error("Prolog process connection lost (EPIPE)"));
            }
            return rejectAndFinish(error || new Error("Prolog process write error"));
          };
          onWriteErrorRef = onWriteError;
          stdin.once?.("error", onWriteError);
          this.process.stdin.write(envelope + "\n");
          // Keep listener attached until this command settles via finish()
        } catch (e) {
          // Fail fast if write fails - handle EPIPE specifically
          this.queryPromises.delete(queryId);
          const error = e as NodeJS.ErrnoException;
          if (error.message?.includes("EPIPE") || error.code === "EPIPE") {
            return rejectAndFinish(new Error("Prolog process connection lost (EPIPE)"));
          }
          return rejectAndFinish(e);
        }

        // Timeout after configurable duration
        // Use shorter timeout for cleanup operations, normal timeout for queries
        // Query timeout hierarchy: env SWI_MCP_QUERY_TIMEOUT_MS -> DEFAULT_QUERY_TIMEOUT_MS
        const queryTimeoutMs = Number.parseInt(
          process.env['SWI_MCP_QUERY_TIMEOUT_MS'] ?? "",
          10,
        );
        const baseTimeout =
          Number.isFinite(queryTimeoutMs) && queryTimeoutMs > 0 ? queryTimeoutMs : DEFAULT_QUERY_TIMEOUT_MS;
        const qTimeout = options?.isCleanup ? CLEANUP_TIMEOUT_MS : baseTimeout;

        timer = setTimeout(() => {
          if (this.queryPromises.has(queryId)) {
            this.queryPromises.delete(queryId);

            // Track consecutive timeouts for circuit breaker
            this.consecutiveTimeouts++;

            // Avoid logging full command payloads (may contain sensitive data)
            logger.warn(`Query timeout after ${qTimeout}ms (id:${queryId}). Consecutive timeouts: ${this.consecutiveTimeouts}`);
            logger.info(`Cleaned up timed-out promise for id:${queryId}. Active promises: ${this.queryPromises.size}`);

            // Open circuit breaker after 1 consecutive timeout for immediate recovery
            if (this.consecutiveTimeouts >= 1 && this.circuitState === 'closed') {
              this.circuitState = 'open';
              logger.error(`Circuit breaker opened after ${this.consecutiveTimeouts} consecutive timeouts. Attempting recovery...`);
              // Trigger recovery attempt in background
              this.attemptRecovery().catch(err => {
                logger.error(`Recovery attempt failed: ${err instanceof Error ? err.message : String(err)}`);
              });
            }

            const errorMsg = options?.isCleanup
              ? `Cleanup operation timeout after ${qTimeout}ms. Process may be stuck.`
              : `Query timeout after ${qTimeout}ms. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.`;
            rejectAndFinish(new Error(errorMsg));
          }
        }, qTimeout);
        timer.unref?.();
      });
    };

    // Chain into the queue to ensure serialization; isolate previous failures
    const p = this.commandQueue
      .catch(() => { /* swallow prior error to keep queue alive */ })
      .then(() => run());
    // Update queue to settle after this command completes, regardless of outcome
    this.commandQueue = p.then(
      () => { /* release */ },
      () => { /* release */ }
    );
    return p;
  }

  /**
   * Consult (load) a Prolog file - legacy implementation without source tracking
   *
   * @deprecated Use importFileWithSource() instead. This method will be removed in a future version.
   *
   * Limitations compared to importFileWithSource():
   * - Does not preserve variable names or original source formatting
   * - No provenance tracking (can't tell which file added which clauses)
   * - Cannot selectively unimport files
   * - No snapshot capability
   * - Allows duplicate file loading
   *
   * This method is kept only for backward compatibility in stress/security tests.
   */
  async consultFile(filename: string): Promise<string> {
    const absolutePath = path.resolve(filename);
    // Escape backslashes first (Windows paths) then single quotes for Prolog atom
    const escaped = absolutePath.replace(/\\/g, "\\\\").replace(/'/g, "\\'");
    const result = await this.query(`consult('${escaped}')`);

    // Check if result is an error and throw
    if (typeof result === "string" && result.startsWith(TERM_ERROR)) {
      const parsedError = PrologInterface.parsePrologError(result);
      throw new Error(PrologInterface.formatPrologError(parsedError));
    }

    return result;
  }

  /**
   * Load a safe Prolog library using use_module
   * This loads the library into both knowledge_base (for execution) and prolog_server (for parsing)
   * The library will be validated by SWI-Prolog's sandbox for safety
   */
  async loadLibrary(libraryName: string): Promise<string> {
    // Validate library name format (alphanumeric, underscore, slash for paths like http/http_client)
    if (!/^[a-z0-9_/]+$/i.test(libraryName)) {
      throw new Error(`Invalid library name: ${libraryName}. Library names must contain only letters, numbers, underscores, and slashes.`);
    }

    // Use the load_safe_library command which loads into both modules
    // This ensures operators are available for both query parsing and execution
    const result = await this.query(`load_safe_library(${libraryName})`);

    // Check if result is an error and throw
    if (typeof result === "string" && result.startsWith(TERM_ERROR)) {
      const parsedError = PrologInterface.parsePrologError(result);
      throw new Error(PrologInterface.formatPrologError(parsedError));
    }

    return result;
  }

  /**
   * Start a new engine session
   * Automatically closes any active query or engine session before starting.
   */
  async startEngine(query: string): Promise<{ status: string; engine_ready: boolean }> {
    // Auto-close active query session if present
    if (
      this.queryActive ||
      this.sessionState === "query" ||
      this.sessionState === "closing_query"
    ) {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      if (traceEnabled) {
        logger.info("Auto-closing active query session before starting new engine");
      }
      try {
        await this.closeQuery();
      } catch (error) {
        logger.warn(`Auto-close of query failed: ${error instanceof Error ? error.message : String(error)}`);
        // Reset state to allow new engine to start
        this.queryActive = false;
        this.currentQuery = null;
        this.setSessionState("idle");
      }
    }
    // Auto-close active engine session if present
    if (
      this.engineActive ||
      this.sessionState === "engine" ||
      this.sessionState === "closing_engine"
    ) {
      const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
      if (traceEnabled) {
        logger.info("Auto-closing active engine session before starting new engine");
      }
      try {
        await this.closeEngine();
      } catch (error) {
        logger.warn(`Auto-close of engine failed: ${error instanceof Error ? error.message : String(error)}`);
        // Reset state to allow new engine to start
        this.engineActive = false;
        this.engineReachedEOF = false;
        this.setSessionState("idle");
      }
    }

    this.assertRunning();

    this.engineActive = true;
    this.engineReachedEOF = false;
    this.setSessionState("engine");
    // Commands are serialized; prior closes complete before this runs
    // Send start_engine_string command to server using proper string escaping
    const escapedQuery = this.escapeQueryString(query);
    const result = await this.sendCommand(`start_engine_string("${escapedQuery}")`);
    // Reject on any non-ok response from server
    if (result !== "ok") {
      this.engineActive = false;
      this.setSessionState("idle");
      if (typeof result === "string" && result.startsWith(TERM_ERROR)) {
        const parsedError = PrologInterface.parsePrologError(result);
        throw new Error(PrologInterface.formatPrologError(parsedError));
      }
      throw new Error(typeof result === "string" ? result : "Engine start failed");
    }
    return {
      status: "ready",
      engine_ready: true,
    };
  }

  /**
   * Get the next solution from current engine
   */
  async nextEngine(): Promise<{ solution: string | null; status: "success" | "done"; error?: string }> {
    if (!this.engineActive && this.sessionState !== "engine_completed") {
      return { solution: null, status: "done", error: "No active engine. Start an engine first." };
    }

    // If engine is already completed, return consistent "no more solutions" message
    if (this.sessionState === "engine_completed" || this.engineReachedEOF) {
      return { solution: null, status: "done" };
    }

    this.assertRunning();

    try {
      const result = await this.sendCommand("next_engine");
      const parsed = this.parseServerResult(result);
      if (parsed.kind === "eof") {
        // Keep engine info but mark as completed instead of clearing everything
        this.engineActive = false;
        this.engineReachedEOF = true;
        this.setSessionState("engine_completed");
        return { solution: null, status: "done" };
      }
      if (parsed.kind === "error") {
        this.engineActive = false;
        this.engineReachedEOF = true;
        this.setSessionState("idle");
        const parsedError = PrologInterface.parsePrologError(parsed.error);
        return { solution: null, status: "done", error: PrologInterface.formatPrologError(parsedError) };
      }
      if (parsed.kind === "solution") {
        return { solution: parsed.value, status: "success" };
      }
      return { solution: parsed.value, status: "success" };
    } catch (error) {
      this.engineActive = false;
      this.engineReachedEOF = true;
      this.setSessionState("idle");
      const errorMessage = error instanceof Error ? error.message : String(error);
      const parsedError = PrologInterface.parsePrologError(errorMessage);
      return {
        solution: null,
        status: "done",
        error: PrologInterface.formatPrologError(parsedError),
      };
    }
  }

  /**
   * Close the current engine session
   */
  async closeEngine(): Promise<{ status: string }> {
    if (!this.engineActive && this.sessionState !== "engine_completed") {
      return { status: "no_active_engine" };
    }

    this.assertRunning();

    try {
      this.setSessionState("closing_engine");
      // Use cleanup timeout for close operations
      await this.sendCommand("close_engine", { isCleanup: true });
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      // If cleanup times out, process may be stuck - trigger recovery
      if (errorMsg.includes("Cleanup operation timeout")) {
        logger.error("Engine cleanup timeout detected - process may be stuck. Triggering recovery...");
        // Don't await - let recovery happen in background
        this.attemptRecovery().catch(err => {
          logger.error(`Recovery after cleanup timeout failed: ${err instanceof Error ? err.message : String(err)}`);
        });
      }
      // Continue with state cleanup even if command failed
    }

    this.engineActive = false;
    this.engineReachedEOF = false;
    this.setSessionState("idle");

    return { status: "closed" };
  }

  /**
   * Attempt to recover from stuck/unresponsive state by restarting the Prolog process
   * @private
   */
  private async attemptRecovery(): Promise<void> {
    // Prevent multiple concurrent recovery attempts
    if (this.recoveryInProgress) {
      logger.info('Recovery already in progress, waiting for it to complete...');
      return this.recoveryInProgress;
    }

    const recoveryPromise = (async () => {
      logger.warn('Attempting recovery: restarting Prolog process...');

      try {
        // Force stop the process
        await this.stop();

        // Wait a bit before restarting
        await new Promise(resolve => setTimeout(resolve, 100));

        // Start fresh process
        await this.start();

        // Enter half-open circuit state to test if recovery worked
        this.circuitState = 'half-open';
        this.consecutiveTimeouts = 0;

        logger.info('Recovery complete - process restarted. Circuit breaker in half-open state.');
      } catch (error) {
        logger.error(`Recovery failed: ${error instanceof Error ? error.message : String(error)}`);
        // Keep circuit open if recovery fails
        this.circuitState = 'open';
        throw error;
      } finally {
        // Clear recovery state
        this.recoveryInProgress = null;
      }
    })();

    this.recoveryInProgress = recoveryPromise;
    return recoveryPromise;
  }

  /**
   * Get health status information for diagnostics
   */
  getHealthStatus(): {
    circuitState: 'closed' | 'open' | 'half-open';
    consecutiveTimeouts: number;
    lastSuccessfulResponse: number | null;
    pendingQueries: number;
    isReady: boolean;
    sessionState: SessionState;
  } {
    return {
      circuitState: this.circuitState,
      consecutiveTimeouts: this.consecutiveTimeouts,
      lastSuccessfulResponse: this.lastSuccessfulResponse,
      pendingQueries: this.queryPromises.size,
      isReady: this.isReady,
      sessionState: this.sessionState,
    };
  }

  /**
   * Check if the Prolog server is healthy and responsive
   * @returns true if process is running and ready, false otherwise
   */
  isHealthy(): boolean {
    return (
      this.process !== null &&
      this.isReady &&
      this.circuitState !== 'open' &&
      this.recoveryInProgress === null
    );
  }

  /**
   * Stop the Prolog process and wait for complete cleanup
   *
   * IMPORTANT: This method is now async to ensure proper cleanup.
   * All callers must await this method to prevent race conditions.
   */
  async stop(): Promise<void> {
    // Mark as intentional stop to prevent auto-restart
    this.intentionalStop = true;
    const traceEnabled = isOn(process.env['SWI_MCP_TRACE']);
    if (traceEnabled) {
      logger.debug('stop() called - intentionalStop flag set');
    }

    const proc = this.process;
    if (!proc) {
      // Already stopped - just cleanup state
      if (traceEnabled) {
        logger.debug('stop() called but process is null - cleaning up state');
      }
      this.cleanupState();
      this.intentionalStop = false;
      return;
    }

    // Create promise that resolves when process exits
    const exitPromise = new Promise<void>((resolve) => {
      if (proc.exitCode !== null) {
        resolve();  // Already exited
        return;
      }

      // Listen for exit - DON'T remove existing listeners yet!
      proc.once('exit', () => {
        resolve();
      });
    });

    // Send graceful exit command
    if (proc.stdin && !proc.killed) {
      try {
        (proc.stdin as NodeJS.WritableStream).once?.("error", () => {});
        proc.stdin.write("__EXIT__\n");
        proc.stdin.end();
      } catch {}
    }

    // Force kill after delay if still alive
    const killTimer = setTimeout(() => {
      if (!proc.killed && proc.exitCode === null) {
        try {
          proc.kill("SIGTERM");
        } catch {}
      }
    }, STOP_KILL_DELAY_MS);

    // Hard timeout - use SIGKILL if process doesn't exit within max wait time
    const maxWaitTimer = setTimeout(() => {
      if (!proc.killed && proc.exitCode === null) {
        logger.warn(`Process did not exit after ${STOP_MAX_WAIT_MS}ms, forcing SIGKILL`);
        try {
          proc.kill("SIGKILL");
          // Give SIGKILL a moment to work, then force cleanup
          setTimeout(() => {
            if (proc.exitCode === null) {
              logger.error(`Process still alive after SIGKILL, forcing cleanup`);
              // Manually trigger cleanup if process is truly stuck
              proc.emit('exit', -1, 'SIGKILL');
            }
          }, 100);
        } catch {}
      }
    }, STOP_MAX_WAIT_MS);

    try {
      // ✅ WAIT for process to actually exit (with timeout)
      await Promise.race([
        exitPromise,
        new Promise<void>(resolve => setTimeout(resolve, STOP_MAX_WAIT_MS + 500))
      ]);
    } finally {
      clearTimeout(killTimer);
      clearTimeout(maxWaitTimer);
    }

    // ✅ NOW remove listeners (process is dead)
    try {
      proc.removeAllListeners("error");
      proc.removeAllListeners("exit");
      proc.stdout?.removeAllListeners("data");
      proc.stderr?.removeAllListeners("data");
    } catch {}

    // Clear instance state
    this.process = null;
    this.cleanupState();

    // Reset intentional stop flag
    this.intentionalStop = false;
  }

  /**
   * Clean up internal state (called by stop())
   */
  private cleanupState(): void {
    // Reject and clear pending queries
    for (const [_id, promise] of this.queryPromises) {
      try {
        promise.reject(new Error("Prolog server stopped"));
      } catch {}
    }
    this.queryPromises.clear();
    this.queryActive = false;
    this.engineActive = false;
    this.engineReachedEOF = false;
    this.currentQuery = null;
    this.readyPromise = null;
    this.readyResolver = null;
    this.readyRejecter = null;
    this.setSessionState("idle");
    // Clear source storage
    this.kbSourceStorage.clear();
    this.sourceIdCounter = 0;
    // Don't reset circuit breaker state here - let attemptRecovery() manage it
    // This allows circuit to stay open if stop() is called manually vs recovery
  }

  /**
   * Escape a query string for safe passage to Prolog as a quoted string
   * Handles quotes and backslashes that could interfere with string parsing
   */
  private escapeQueryString(query: string): string {
    return query
      .replace(/\\/g, '\\\\')  // Escape backslashes first
      .replace(/"/g, '\\"');   // Escape double quotes
  }

  /**
   * Parse a Prolog error term into a structured PrologError object
   */
  static parsePrologError(errorTerm: string): PrologError {
    const trimmed = errorTerm.trim();

    // Parse error(ErrorType) format
    const errorMatch = trimmed.match(/^error\((.*)\)$/);
    if (!errorMatch) {
      // Detect client-side timeout messages explicitly
      const clientTimeout = trimmed.match(/^Query timeout after\s+(\d+)ms/i);
      if (clientTimeout) {
        const ms = parseInt(clientTimeout[1]);
        return {
          kind: PrologErrorKind.TIMEOUT,
          message: 'Query timed out',
          details: { raw: trimmed, timeoutMs: Number.isFinite(ms) ? ms : undefined }
        };
      }
      return {
        kind: PrologErrorKind.UNKNOWN,
        message: trimmed,
        details: { raw: trimmed }
      };
    }

    const errorContent = errorMatch[1];

    // unsafe_goal(Goal)
    const unsafeGoalMatch = errorContent.match(/^unsafe_goal\((.*)\)$/);
    if (unsafeGoalMatch) {
      const goal = unsafeGoalMatch[1];
      return {
        kind: PrologErrorKind.UNSAFE_GOAL,
        message: `Security Error: Unsafe operation blocked`,
        details: { goal, raw: trimmed }
      };
    }

    // permission_error(Action, Type, Object)
    const permissionMatch = errorContent.match(/^permission_error\(([^,]+),\s*([^,]+),\s*(.*)\)$/);
    if (permissionMatch) {
      const [, action, type, object] = permissionMatch;
      return {
        kind: PrologErrorKind.PERMISSION_ERROR,
        message: `Permission denied: Cannot ${action.trim()} ${type.trim()}`,
        details: { operation: action.trim(), file: object.trim(), raw: trimmed }
      };
    }

    // existence_error(Type, Name)
    const existenceMatch = errorContent.match(/^existence_error\(([^,]+),\s*(.*)\)$/);
    if (existenceMatch) {
      const [, type, name] = existenceMatch;
      return {
        kind: PrologErrorKind.EXISTENCE_ERROR,
        message: `${type.trim()} not found: ${name.trim()}`,
        details: { file: name.trim(), raw: trimmed }
      };
    }

    // instantiation_error(Message)
    const instantiationMatch = errorContent.match(/^instantiation_error\((.*)\)$/);
    if (instantiationMatch) {
      const message = instantiationMatch[1].replace(/^'|'$/g, ''); // Remove quotes
      return {
        kind: PrologErrorKind.INSTANTIATION_ERROR,
        message: message || 'Variable not sufficiently instantiated',
        details: { raw: trimmed }
      };
    }

    // syntax_error(Details)
    const syntaxMatch = errorContent.match(/^syntax_error\((.*)\)$/);
    if (syntaxMatch) {
      return {
        kind: PrologErrorKind.SYNTAX_ERROR,
        message: `Syntax error in Prolog code`,
        details: { raw: trimmed }
      };
    }

    // timeout(...) — future-proof for Prolog-side timeouts
    const timeoutTerm = errorContent.match(/^timeout\((.*)\)$/);
    if (timeoutTerm) {
      return {
        kind: PrologErrorKind.TIMEOUT,
        message: 'Query timed out',
        details: { raw: trimmed }
      };
    }

    // Custom server-specific error patterns

    // session_conflict(CurrentType, Type)
    const sessionConflictMatch = errorContent.match(/^session_conflict\(([^,]+),\s*([^)]+)\)$/);
    if (sessionConflictMatch) {
      const [, currentType, requestedType] = sessionConflictMatch;
      return {
        kind: PrologErrorKind.SESSION_CONFLICT,
        message: `Session conflict: A ${currentType.trim()} session is already active, cannot start ${requestedType.trim()} session`,
        details: { raw: trimmed }
      };
    }

    // invalid_query_syntax(ParseError)
    const invalidSyntaxMatch = errorContent.match(/^invalid_query_syntax\((.*)\)$/);
    if (invalidSyntaxMatch) {
      return {
        kind: PrologErrorKind.SYNTAX_ERROR,
        message: `Invalid query syntax`,
        details: { raw: trimmed }
      };
    }

    // invalid_query_structure(ValidationError)
    const invalidStructureMatch = errorContent.match(/^invalid_query_structure\((.*)\)$/);
    if (invalidStructureMatch) {
      return {
        kind: PrologErrorKind.SYNTAX_ERROR,
        message: `Invalid query structure`,
        details: { raw: trimmed }
      };
    }

    // no_active_query or no_active_engine
    if (errorContent === 'no_active_query' || errorContent === 'no_active_engine') {
      return {
        kind: PrologErrorKind.NO_ACTIVE_SESSION,
        message: `No active ${errorContent === 'no_active_query' ? 'query' : 'engine'} session`,
        details: { raw: trimmed }
      };
    }

    // undefined_predicate_in_query(Pred, Query)
    const undefinedPredMatch = errorContent.match(/^undefined_predicate_in_query\(([^,]+),\s*(.*)\)$/);
    if (undefinedPredMatch) {
      const [, pred, query] = undefinedPredMatch;
      return {
        kind: PrologErrorKind.EXISTENCE_ERROR,
        message: `Undefined predicate: ${pred.trim()}`,
        details: { predicate: pred.trim(), goal: query.trim(), raw: trimmed }
      };
    }

    // nothing_to_retract
    if (errorContent === 'nothing_to_retract') {
      return {
        kind: PrologErrorKind.EXISTENCE_ERROR,
        message: `No matching facts to retract`,
        details: { raw: trimmed }
      };
    }

    // line_too_long(Len)
    const lineTooLongMatch = errorContent.match(/^line_too_long\((\d+)\)$/);
    if (lineTooLongMatch) {
      const len = lineTooLongMatch[1];
      return {
        kind: PrologErrorKind.QUERY_TOO_LARGE,
        message: `Query too large: ${len} characters exceeds limit`,
        details: { raw: trimmed }
      };
    }

    // state_inconsistency
    if (errorContent === 'state_inconsistency') {
      return {
        kind: PrologErrorKind.UNKNOWN,
        message: `Internal state inconsistency detected`,
        details: { raw: trimmed }
      };
    }

    // Default case
    return {
      kind: PrologErrorKind.UNKNOWN,
      message: errorContent,
      details: { raw: trimmed }
    };
  }

  /**
   * Format a PrologError into a user-friendly message
   */
  static formatPrologError(error: PrologError): string {
    // Get the human-readable message
    let message: string;
    switch (error.kind) {
      case PrologErrorKind.UNSAFE_GOAL:
        const goalText = error.details?.goal;
        if (goalText) {
          // Handle module:predicate patterns by taking the rightmost segment after splitting on ':'
          const segments = goalText.split(':');
          const predicatePart = segments[segments.length - 1];
          const predicate = predicatePart.match(/^(\w+)\(/)?.[1];
          if (predicate) {
            message = `Security Error: Operation blocked - contains dangerous predicate '${predicate}'`;
            break;
          }
        }
        message = error.message;
        break;

      case PrologErrorKind.PERMISSION_ERROR:
        if (error.details?.operation === 'execute' && error.message.includes('directive')) {
          message = 'Security Error: Directives are not allowed in sandboxed consult';
        } else {
          message = error.message;
        }
        break;

      case PrologErrorKind.SYNTAX_ERROR:
        message = 'Syntax Error: Invalid Prolog syntax. When using the clauses tool, ensure proper formatting of facts and rules.';
        break;

      case PrologErrorKind.TIMEOUT: {
        const ms = error.details?.timeoutMs;
        const base = 'Query timed out';
        const hint = 'Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.';
        message = typeof ms === 'number' && Number.isFinite(ms)
          ? `${base} after ${ms}ms. ${hint}`
          : `${base}. ${hint}`;
        break;
      }

      default:
        message = error.message;
    }

    // Return structured JSON for domain-specific error explanation
    // The MCP Host will detect this format and preserve it for error analyzers
    return JSON.stringify({
      kind: error.kind,
      message,
      details: error.details || {}
    });
  }

  // ========================================================================
  // Source Storage Infrastructure (Step 1 of tools refactoring)
  // ========================================================================

  /**
   * Generate a unique ID for source storage entries
   * Session-scoped, simple counter-based approach (no external dependencies)
   */
  private generateSourceId(): string {
    return `src_${++this.sourceIdCounter}`;
  }

  /**
   * Pure helper: Normalize clause by ensuring it has a trailing period
   * DRY principle - single source of truth for period normalization
   */
  private normalizeClause(clause: string): string {
    const trimmed = clause.trim();
    return trimmed.endsWith('.') ? trimmed : `${trimmed}.`;
  }

  /**
   * Pure helper: Format clause for Prolog (remove trailing period, wrap rules)
   * DRY principle - reused by assert/retract operations
   *
   * Rules (clauses with :-) must be wrapped in parentheses to prevent
   * operator precedence issues when used as arguments to assertz/retract
   */
  private formatClauseForProlog(clause: string): string {
    const normalized = this.normalizeClause(clause);
    const withoutPeriod = normalized.slice(0, -1); // Remove trailing period

    // Wrap rules in parentheses to avoid precedence issues
    // e.g., assertz(foo(X) :- bar(X), baz(X)) would parse as two arguments
    // but assertz((foo(X) :- bar(X), baz(X))) parses correctly
    if (withoutPeriod.includes(':-')) {
      return `(${withoutPeriod})`;
    }

    return withoutPeriod;
  }

  /**
   * Pure helper: Generic filter for source entries
   * Functional programming - composable predicate-based filtering
   */
  private findMatchingSourceEntries(predicate: (entry: SourceEntry) => boolean): SourceEntry[] {
    return Array.from(this.kbSourceStorage.values()).filter(predicate);
  }

  /**
   * Assert a clause to Prolog KB and store its original source text
   * Ensures atomicity: clause is stored in Map only if Prolog assertion succeeds
   *
   * @param clause - Prolog clause (fact or rule)
   * @param type - Origin: 'inline' for direct assertion, 'file' for file import
   * @param file - File path (required if type='file')
   * @returns Success status with ID or error message
   */
  async assertClauseWithSource(
    clause: string,
    type: 'inline' | 'file',
    file?: string
  ): Promise<{success: boolean; id?: string; error?: string}> {
    const id = this.generateSourceId();

    try {
      // Normalize clause (ensure period)
      const normalized = this.normalizeClause(clause);

      // Format for Prolog (remove period)
      const forProlog = this.formatClauseForProlog(clause);

      // Assert to Prolog FIRST
      // Use assertz(Fact) which goes through assert_knowledge_base_term_safe/1 validation
      // The Prolog side will add knowledge_base: qualification internally
      const result = await this.query(`assertz(${forProlog})`);

      // Check if assertion succeeded (query returns empty string or success indicator)
      // If no error was thrown, we consider it successful
      // Store to Map ONLY on success
      this.kbSourceStorage.set(id, {
        id,
        sourceText: normalized,
        type,
        file,
        timestamp: Date.now(),
        compiled: true
      });

      return {success: true, id};
    } catch (error) {
      // Prolog assertion failed - do NOT store in Map
      return {success: false, error: String(error)};
    }
  }

  /**
   * Retract a clause from Prolog KB and remove from source storage
   * Ensures atomicity: source removed from Map only if Prolog retraction succeeds
   *
   * @param clause - Prolog clause to retract
   * @returns true if retracted, false if not found or failed
   */
  async retractClauseWithSource(clause: string): Promise<boolean> {
    try {
      // Normalize for comparison
      const normalized = this.normalizeClause(clause);

      // Find matching source entries (functional filter)
      const matchingIds = this.findMatchingSourceEntries(
        entry => entry.sourceText === normalized
      ).map(entry => entry.id);

      if (matchingIds.length === 0) {
        return false; // No matching clause found
      }

      // Format for Prolog (remove period)
      const forProlog = this.formatClauseForProlog(clause);

      // Retract from Prolog FIRST
      await this.query(`retract(knowledge_base:(${forProlog}))`);

      // Remove first matching entry from source storage (ONLY after Prolog succeeds)
      this.kbSourceStorage.delete(matchingIds[0]);

      return true;
    } catch (error) {
      // Retraction failed - leave source storage unchanged
      return false;
    }
  }

  /**
   * Get snapshot of knowledge base (original source text)
   * Pure transformation - filters, sorts, and maps entries
   * Synced with prolog://workspace/snapshot resource
   *
   * @returns Original source text with preserved variable names
   */
  async getSnapshot(): Promise<string> {
    const sources = Array.from(this.kbSourceStorage.values())
      .filter(entry => entry.compiled) // Only successfully compiled entries
      .sort((a, b) => a.timestamp - b.timestamp) // Preserve insertion order
      .map(entry => entry.sourceText); // Extract source text

    return sources.join('\n');
  }

  /**
   * Clear entire workspace (both Prolog KB and source storage)
   * Ensures both are cleared atomically
   */
  async clearWorkspaceWithSource(): Promise<void> {
    // Clear Prolog KB using existing predicate
    await this.query('abolish_all_user_predicates');

    // Clear source storage
    this.kbSourceStorage.clear();
  }

  /**
   * Parse .pl file into array of clause strings
   * Simple text-based parser that preserves original formatting
   *
   * @param filename - Path to .pl file
   * @returns Array of clause strings with original formatting
   */
  private async parseFileToStringArray(filename: string): Promise<string[]> {
    const content = await fsPromises.readFile(filename, 'utf-8');
    const clauses: string[] = [];
    let current = '';

    for (const line of content.split('\n')) {
      const trimmed = line.trim();

      // Skip comments and empty lines
      if (!trimmed || trimmed.startsWith('%')) {
        continue;
      }

      // Skip directives (for now - may need special handling)
      if (trimmed.startsWith(':-')) {
        continue;
      }

      current += (current ? ' ' : '') + trimmed;

      // Check if clause ends
      if (current.trim().endsWith('.')) {
        clauses.push(current.trim());
        current = '';
      }
    }

    return clauses;
  }

  /**
   * Parse a Prolog list string into individual terms
   * Handles nested structures and quoted strings
   */
  private parseTermList(listStr: string): string[] {
    const terms: string[] = [];
    let current = '';
    let depth = 0;
    let inQuotes = false;
    let escape = false;

    for (let i = 0; i < listStr.length; i++) {
      const char = listStr[i];

      if (escape) {
        current += char;
        escape = false;
        continue;
      }

      if (char === '\\' && inQuotes) {
        current += char;
        escape = true;
        continue;
      }

      if (char === '"') {
        inQuotes = !inQuotes;
        current += char;
        continue;
      }

      if (!inQuotes) {
        if (char === '(' || char === '[') {
          depth++;
        } else if (char === ')' || char === ']') {
          depth--;
        } else if (char === ',' && depth === 0) {
          // End of term
          if (current.trim()) {
            terms.push(current.trim());
          }
          current = '';
          continue;
        }
      }

      current += char;
    }

    // Add last term
    if (current.trim()) {
      terms.push(current.trim());
    }

    return terms;
  }

  /**
   * Parse Prolog file using Prolog's native parser
   * Returns structured data with original variable names preserved
   *
   * @param filename - Absolute path to .pl file
   * @returns Array of clause data or error
   */
  private async parseFileWithProlog(filename: string): Promise<{
    success: boolean;
    clauses?: Array<{
      sourceText: string;
      varNames: Array<[string, any]>;
      singletons: string[];
      lineNumber: number;
    }>;
    errors?: string[];
  }> {
    const absolutePath = path.resolve(filename);

    try {
      // Send parse command to Prolog
      const result = await this.sendCommand(`parse_file("${absolutePath.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}")`);

      // Check if it's an error response
      if (result.startsWith('error(')) {
        // Parse error message
        const errorMatch = result.match(/error\(([^)]+)\)/);
        const errorMsg = errorMatch ? errorMatch[1] : 'Unknown parsing error';
        return {
          success: false,
          errors: [errorMsg]
        };
      }

      // Parse success response - expecting success([...])
      if (result.startsWith('success(')) {
        const clauses: Array<{
          sourceText: string;
          varNames: Array<[string, any]>;
          singletons: string[];
          lineNumber: number;
        }> = [];

        const errors: string[] = [];

        // Extract the list from success(List)
        const listMatch = result.match(/success\(\[(.*)\]\)$/s);
        if (!listMatch) {
          return {
            success: false,
            errors: ['Invalid response format from Prolog parser']
          };
        }

        // Parse the clause list - this is a simplified parser
        // In production, we'd want a more robust Prolog term parser
        const clauseListStr = listMatch[1];

        // Parse the clause list more carefully
        // Each clause is in format: clause(SourceText, VarNames, Singletons, LineNum)
        // We need a more robust parser that handles escaped quotes and nested structures

        const clauses_raw = this.parseTermList(clauseListStr);

        for (const term of clauses_raw) {
          // Simple parsing for clause terms
          if (term.startsWith('clause(')) {
            // Extract components manually - this is still simplified
            const parts = term.substring(7, term.length - 1); // Remove 'clause(' and ')'

            // Find the source text (first quoted string)
            const sourceMatch = parts.match(/^"([^"\\]*(\\.[^"\\]*)*)"/);
            if (!sourceMatch) continue;

            const sourceText = sourceMatch[1].replace(/\\"/g, '"').replace(/\\\\/g, '\\');

            // For MVP, we'll just use empty arrays for varNames and singletons
            // since the complex parsing is error-prone
            clauses.push({
              sourceText,
              varNames: [],
              singletons: [],
              lineNumber: 0
            });
          } else if (term.startsWith('error(')) {
            const errorMatch = term.match(/error\("([^"\\]*(\\.[^"\\]*)*)"\)/);
            if (errorMatch) {
              errors.push(errorMatch[1].replace(/\\"/g, '"').replace(/\\\\/g, '\\'));
            }
          }
        }

        // Check for error entries (using regex to catch any we might have missed)
        const errorPattern = /error\("([^"\\]*(\\.[^"\\]*)*)"\)/g;
        let errorRegexMatch;
        while ((errorRegexMatch = errorPattern.exec(clauseListStr)) !== null) {
          errors.push(errorRegexMatch[1].replace(/\\"/g, '"').replace(/\\\\/g, '\\'));
        }

        return {
          success: errors.length === 0,
          clauses,
          errors: errors.length > 0 ? errors : undefined
        };
      }

      return {
        success: false,
        errors: ['Unexpected response format from Prolog parser']
      };

    } catch (error) {
      return {
        success: false,
        errors: [String(error)]
      };
    }
  }

  /**
   * Import .pl file with provenance tracking
   * Prevents duplicate imports and tracks which file added which clauses
   *
   * Note: Directives (lines starting with ':-') are skipped during parsing.
   * Libraries like clpfd, lists, apply are pre-loaded by default.
   * Additional libraries can be configured via KB_LIBRARIES environment variable.
   *
   * @param filename - Path to .pl file
   * @returns Success status with clause count and errors
   */
  async importFileWithSource(filename: string): Promise<{
    success: boolean;
    clausesAdded: number;
    errors: string[];
  }> {
    // Check if already imported
    const alreadyImported = this.findMatchingSourceEntries(
      entry => entry.type === 'file' && entry.file === filename
    ).length > 0;

    if (alreadyImported) {
      return {
        success: false,
        clausesAdded: 0,
        errors: [`File ${filename} already imported. Use unimport first to reload.`]
      };
    }

    // Parse file using Prolog parser
    const parseResult = await this.parseFileWithProlog(filename);

    // Return early only if there are NO clauses at all
    if (!parseResult.clauses || parseResult.clauses.length === 0) {
      return {
        success: false,
        clausesAdded: 0,
        errors: parseResult.errors || ['Unknown parsing error']
      };
    }

    let clausesAdded = 0;
    const errors: string[] = [];

    // Include parse errors from the parser
    if (parseResult.errors && parseResult.errors.length > 0) {
      errors.push(...parseResult.errors);
    }

    // Assert each clause with source tracking
    for (const clauseData of parseResult.clauses) {
      const result = await this.assertClauseWithSource(
        clauseData.sourceText,
        'file',
        filename
      );

      if (result.success) {
        clausesAdded++;
        // Optional: Could store clauseData.varNames, singletons in future
        // for enhanced error messages and debugging
      } else {
        errors.push(`Failed to assert: ${clauseData.sourceText.substring(0, 50)}... - ${result.error}`);
      }
    }

    return {
      success: errors.length === 0,
      clausesAdded,
      errors
    };
  }

  /**
   * Unimport file (remove all clauses from specific file)
   * Uses functional filtering to find file-specific entries
   *
   * @param filename - Path to file to unimport
   * @returns Success status with count of clauses removed
   */
  async unimportFile(filename: string): Promise<{
    success: boolean;
    clausesRemoved: number;
  }> {
    // Find all entries from this file (functional filter)
    const toRemove = this.findMatchingSourceEntries(
      entry => entry.type === 'file' && entry.file === filename
    );

    if (toRemove.length === 0) {
      return {success: false, clausesRemoved: 0};
    }

    let clausesRemoved = 0;

    // Retract each clause
    for (const entry of toRemove) {
      const forProlog = this.formatClauseForProlog(entry.sourceText);

      try {
        await this.query(`retract(knowledge_base:(${forProlog}))`);

        // Remove from source storage
        this.kbSourceStorage.delete(entry.id);
        clausesRemoved++;
      } catch (error) {
        // Ignore errors for individual retractions
        // Some clauses may have already been retracted manually
      }
    }

    return {success: true, clausesRemoved};
  }

  /**
   * Get list of imported files with metadata
   * Pure transformation using functional reduce/map/sort
   *
   * @returns Array of file metadata (path, clause count, timestamp)
   */
  getImportedFiles(): Array<{filename: string; clauseCount: number; timestamp: number}> {
    // Functional reduce to group by filename
    const fileMap = this.findMatchingSourceEntries(
      entry => entry.type === 'file' && entry.file !== undefined
    ).reduce((acc, entry) => {
      const filename = entry.file!;
      const existing = acc.get(filename);

      if (existing) {
        existing.count++;
        // Keep earliest timestamp
        if (entry.timestamp < existing.timestamp) {
          existing.timestamp = entry.timestamp;
        }
      } else {
        acc.set(filename, {count: 1, timestamp: entry.timestamp});
      }

      return acc;
    }, new Map<string, {count: number; timestamp: number}>());

    // Transform to array and sort (functional map/sort)
    return Array.from(fileMap.entries())
      .map(([filename, data]) => ({
        filename,
        clauseCount: data.count,
        timestamp: data.timestamp
      }))
      .sort((a, b) => a.timestamp - b.timestamp);
  }
}
