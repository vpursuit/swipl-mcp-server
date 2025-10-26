import { spawn, ChildProcess } from "child_process";
import path from "path";
import fs from "fs";
import { logger } from "./logger.js";
import { getPrologScriptPath, prologScriptCandidates } from "./meta.js";
import {
  DEFAULT_QUERY_TIMEOUT_MS,
  DEFAULT_READY_TIMEOUT_MS,
  STOP_KILL_DELAY_MS,
  READY_MARK,
  TERM_SOLUTION,
  TERM_ERROR,
  NO_MORE_SOLUTIONS,
  MAX_BUFFER_SIZE
} from "./constants.js";

// Error type system
export enum PrologErrorKind {
  UNSAFE_GOAL = 'unsafe_goal',
  PERMISSION_ERROR = 'permission_error',
  SYNTAX_ERROR = 'syntax_error',
  EXISTENCE_ERROR = 'existence_error',
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

    try {
      this.process = spawn("swipl", args, { stdio: ["pipe", "pipe", "pipe"] });
    } catch (error) {
      const err = error as NodeJS.ErrnoException;
      if (err.code === "ENOENT") {
        throw new Error(
          "SWI-Prolog not found in PATH. Please install SWI-Prolog and ensure 'swipl' command is available.",
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

    this.process.on("exit", () => {
      this.process = null;
      // Reject any pending queries
      for (const [_id, promise] of this.queryPromises) {
        try { promise.reject(new Error("Prolog server exited")); } catch { }
      }
      this.queryPromises.clear();
      failStartOnce(new Error("Prolog server exited before ready"));
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
   */
  async startQuery(query: string): Promise<{ status: string; solutions_available: boolean }> {
    if (
      this.queryActive ||
      this.sessionState === "query" ||
      this.sessionState === "closing_query"
    ) {
      throw new Error("A query is already active. Close the current query first.");
    }
    if (
      this.engineActive ||
      this.sessionState === "engine" ||
      this.sessionState === "closing_engine"
    ) {
      throw new Error("An engine session is already active. Close the engine first.");
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
      solutions_available: true,
    };
  }

  /**
   * Get the next solution from current query
   */
  async nextSolution(): Promise<{ solution?: string; more_solutions: boolean; error?: string }> {
    if (!this.queryActive && this.sessionState !== "query_completed") {
      return { error: "No active query. Start a query first.", more_solutions: false };
    }

    // If query is already completed, return consistent "no more solutions" message
    if (this.sessionState === "query_completed") {
      return { more_solutions: false };
    }

    this.assertRunning();

    try {
      const result = await this.sendCommand("next_solution");
      const parsed = this.parseServerResult(result);
      if (parsed.kind === "eof") {
        // Keep query info but mark as completed instead of clearing everything
        this.queryActive = false;
        this.setSessionState("query_completed");
        return { more_solutions: false };
      }
      if (parsed.kind === "error") {
        this.queryActive = false;
        this.currentQuery = null;
        this.setSessionState("idle");
        const parsedError = PrologInterface.parsePrologError(parsed.error);
        return { error: PrologInterface.formatPrologError(parsedError), more_solutions: false };
      }
      if (parsed.kind === "solution") {
        return { solution: parsed.value, more_solutions: true };
      }
      return { solution: parsed.value, more_solutions: true };
    } catch (error) {
      this.queryActive = false;
      this.currentQuery = null;
      this.setSessionState("idle");
      const errorMessage = error instanceof Error ? error.message : String(error);
      const parsedError = PrologInterface.parsePrologError(errorMessage);
      return {
        error: PrologInterface.formatPrologError(parsedError),
        more_solutions: false,
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
      await this.sendCommand("close_query");
    } catch (_error) {
      // Ignore errors when closing
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
  private async sendCommand(command: string): Promise<string> {
    this.assertRunning();

    const run = (): Promise<string> => {
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
        // Query timeout hierarchy: env SWI_MCP_QUERY_TIMEOUT_MS -> DEFAULT_QUERY_TIMEOUT_MS
        const queryTimeoutMs = Number.parseInt(
          process.env['SWI_MCP_QUERY_TIMEOUT_MS'] ?? "",
          10,
        );
        const qTimeout =
          Number.isFinite(queryTimeoutMs) && queryTimeoutMs > 0 ? queryTimeoutMs : DEFAULT_QUERY_TIMEOUT_MS;
        timer = setTimeout(() => {
          if (this.queryPromises.has(queryId)) {
            this.queryPromises.delete(queryId);
            // Avoid logging full command payloads (may contain sensitive data)
            logger.warn(`Query timeout after ${qTimeout}ms (id:${queryId}).`);
            logger.info(`Cleaned up timed-out promise for id:${queryId}. Active promises: ${this.queryPromises.size}`);
            rejectAndFinish(new Error(`Query timeout after ${qTimeout}ms. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.`));
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
   * Consult (load) a Prolog file
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
   */
  async startEngine(query: string): Promise<{ status: string; engine_ready: boolean }> {
    if (
      this.queryActive ||
      this.sessionState === "query" ||
      this.sessionState === "closing_query"
    ) {
      throw new Error("A query session is already active. Close the query first.");
    }
    if (
      this.engineActive ||
      this.sessionState === "engine" ||
      this.sessionState === "closing_engine"
    ) {
      throw new Error("An engine is already active. Close the current engine first.");
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
  async nextEngine(): Promise<{ solution?: string; more_solutions: boolean; error?: string }> {
    if (!this.engineActive && this.sessionState !== "engine_completed") {
      return { error: "No active engine. Start an engine first.", more_solutions: false };
    }

    // If engine is already completed, return consistent "no more solutions" message
    if (this.sessionState === "engine_completed" || this.engineReachedEOF) {
      return { more_solutions: false };
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
        return { more_solutions: false };
      }
      if (parsed.kind === "error") {
        this.engineActive = false;
        this.engineReachedEOF = true;
        this.setSessionState("idle");
        const parsedError = PrologInterface.parsePrologError(parsed.error);
        return { error: PrologInterface.formatPrologError(parsedError), more_solutions: false };
      }
      if (parsed.kind === "solution") {
        return { solution: parsed.value, more_solutions: true };
      }
      return { solution: parsed.value, more_solutions: true };
    } catch (error) {
      this.engineActive = false;
      this.engineReachedEOF = true;
      this.setSessionState("idle");
      const errorMessage = error instanceof Error ? error.message : String(error);
      const parsedError = PrologInterface.parsePrologError(errorMessage);
      return {
        error: PrologInterface.formatPrologError(parsedError),
        more_solutions: false,
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
      await this.sendCommand("close_engine");
    } catch (_error) {
      // Ignore errors when closing
    }

    this.engineActive = false;
    this.engineReachedEOF = false;
    this.setSessionState("idle");

    return { status: "closed" };
  }

  /**
   * Stop the Prolog process and wait for complete cleanup
   *
   * IMPORTANT: This method is now async to ensure proper cleanup.
   * All callers must await this method to prevent race conditions.
   */
  async stop(): Promise<void> {
    const proc = this.process;
    if (!proc) {
      // Already stopped - just cleanup state
      this.cleanupState();
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

    try {
      // ✅ WAIT for process to actually exit
      await exitPromise;
    } finally {
      clearTimeout(killTimer);
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
    switch (error.kind) {
      case PrologErrorKind.UNSAFE_GOAL:
        const goalText = error.details?.goal;
        if (goalText) {
          // Handle module:predicate patterns by taking the rightmost segment after splitting on ':'
          const segments = goalText.split(':');
          const predicatePart = segments[segments.length - 1];
          const predicate = predicatePart.match(/^(\w+)\(/)?.[1];
          if (predicate) {
            return `Security Error: Operation blocked - contains dangerous predicate '${predicate}'`;
          }
        }
        return error.message;

      case PrologErrorKind.PERMISSION_ERROR:
        if (error.details?.operation === 'execute' && error.message.includes('directive')) {
          return 'Security Error: Directives are not allowed in sandboxed consult';
        }
        return error.message;

      case PrologErrorKind.EXISTENCE_ERROR:
        return error.message;

      case PrologErrorKind.SYNTAX_ERROR:
        return 'Syntax Error: Invalid Prolog syntax';

      case PrologErrorKind.TIMEOUT: {
        const ms = error.details?.timeoutMs;
        const base = 'Query timed out';
        const hint = 'Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.';
        return typeof ms === 'number' && Number.isFinite(ms)
          ? `${base} after ${ms}ms. ${hint}`
          : `${base}. ${hint}`;
      }

      case PrologErrorKind.SESSION_CONFLICT:
        return error.message;

      case PrologErrorKind.NO_ACTIVE_SESSION:
        return error.message;

      case PrologErrorKind.QUERY_TOO_LARGE:
        return error.message;

      default:
        return error.message;
    }
  }
}
