import { spawn, ChildProcess } from "child_process";
import path from "path";
import fs from "fs";
import { logger } from "./logger.js";
import { getPrologScriptPath, prologScriptCandidates } from "./meta.js";

// Constants for configuration
const DEFAULT_READY_TIMEOUT_MS = 5000;
const STOP_KILL_DELAY_MS = 50;
const READY_MARK = "@@READY@@";
const TERM_SOLUTION = "solution(";
const TERM_ERROR = "error(";
const NO_MORE_SOLUTIONS = "no_more_solutions";

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
export class PrologInterface {
  private process: ChildProcess | null = null;
  private queryPromises: Map<string, { resolve: Function; reject: Function }> = new Map();
  private queryCounter = 0;
  private responseBuffer: string[] = [];
  private isReady: boolean = false;
  private currentQuery: string | null = null;
  private queryActive: boolean = false;
  private engineActive: boolean = false;
  // Ensure only one command is in flight at a time
  private commandQueue: Promise<void> = Promise.resolve();
  // Session state guard to avoid races across transitions
  private sessionState: "idle" | "query" | "engine" | "closing_query" | "closing_engine" = "idle";

  /**
   * Start SWI-Prolog process
   */
  async start(): Promise<void> {
    if (this.process) {
      return;
    }

    const traceOn = isOn(process.env.SWI_MCP_TRACE);
    if (traceOn) {
      logger.debug(`cwd: ${process.cwd()}`);
    }

    // Resolve Prolog server script: env override, cwd, and paths relative to module/entry
    const envPath = process.env.SWI_MCP_PROLOG_PATH;
    const serverScript = findPrologServerScript(envPath, traceOn);

    // Reset readiness before starting
    this.isReady = false;

    const args = [
      "-q",
      "-s",
      serverScript,
      ...(isOn(process.env.SWI_MCP_EXTENDED_SAFE) ? ["-g", "assert(safe_extended_enabled)"] : []),
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
      this.stop();
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
      this.stop();
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
      const readyTimeoutMs = Number.parseInt(process.env.SWI_MCP_READY_TIMEOUT_MS || String(DEFAULT_READY_TIMEOUT_MS), 10);
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

    // Process complete lines
    const lines = this.inputBuffer.split("\n");
    this.inputBuffer = lines.pop() || ""; // Keep incomplete line in buffer

    // Process each complete line
    for (const line of lines) {
      if (line.trim()) {
        this.processResponseLine(line.trim());
      }
    }
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
      const traceEnabled = isOn(process.env.SWI_MCP_TRACE);
      if (traceEnabled) logger.debug(`Prolog debug: ${response.slice(9).trim()}`);
      return;
    }
    // Avoid logging sensitive response bodies in normal operation
    // Log a summary at debug level only
    {
      const traceEnabled = isOn(process.env.SWI_MCP_TRACE);
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
      }
    }
    // Fallback to FIFO for untagged responses
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
    this.sessionState = "query";

    // Send start_query command to server
    const result = await this.sendCommand(`start_query(${query})`);
    // If Prolog responded with an error(...) term, reject
    if (typeof result === "string" && result.startsWith(TERM_ERROR)) {
      this.queryActive = false;
      this.currentQuery = null;
      this.sessionState = "idle";
      throw new Error(result);
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
    if (!this.queryActive || this.sessionState !== "query") {
      return { error: "No active query. Start a query first.", more_solutions: false };
    }

    this.assertRunning();

    try {
      const result = await this.sendCommand("next_solution");
      const parsed = this.parseServerResult(result);
      if (parsed.kind === "eof") {
        this.queryActive = false;
        this.currentQuery = null;
        this.sessionState = "idle";
        return { more_solutions: false };
      }
      if (parsed.kind === "error") {
        this.queryActive = false;
        this.currentQuery = null;
        this.sessionState = "idle";
        return { error: parsed.error, more_solutions: false };
      }
      if (parsed.kind === "solution") {
        return { solution: parsed.value, more_solutions: true };
      }
      return { solution: parsed.value, more_solutions: true };
    } catch (error) {
      this.queryActive = false;
      this.currentQuery = null;
      this.sessionState = "idle";
      return {
        error: error instanceof Error ? error.message : String(error),
        more_solutions: false,
      };
    }
  }

  /**
   * Close the current query session
   */
  async closeQuery(): Promise<{ status: string }> {
    if (!this.queryActive || this.sessionState !== "query") {
      return { status: "no_active_query" };
    }

    this.assertRunning();

    try {
      this.sessionState = "closing_query";
      await this.sendCommand("close_query");
    } catch (_error) {
      // Ignore errors when closing
    }

    this.queryActive = false;
    this.currentQuery = null;
    this.sessionState = "idle";

    return { status: "closed" };
  }

  /**
   * Send a single command to Prolog server (legacy method for assert/retract/consult)
   */
  async query(query: string): Promise<string> {
    return this.sendCommand(query);
  }

  /**
   * Internal method to send commands to Prolog server
   */
  private async sendCommand(command: string): Promise<string> {
    this.assertRunning();

    return new Promise<string>((outerResolve, outerReject) => {
      const run = () =>
        new Promise<void>((done) => {
          const queryIdNum = this.queryCounter++;
          const queryId = String(queryIdNum);

          const wrappedResolve = (value: any) => {
            try {
              outerResolve(value);
            } finally {
              try {
                done();
              } catch { }
            }
          };
          const wrappedReject = (err: any) => {
            try {
              outerReject(err instanceof Error ? err : new Error(String(err)));
            } finally {
              try {
                done();
              } catch { }
            }
          };

          this.queryPromises.set(queryId, { resolve: wrappedResolve, reject: wrappedReject });

          // Send command to server
          const traceEnabled = isOn(process.env.SWI_MCP_TRACE);
          const envelope = `cmd(${queryIdNum}, ${command})`;
          if (traceEnabled) logger.debug(`Send command: ${envelope}`);
          try {
            if (!this.process || !this.process.stdin) {
              this.queryPromises.delete(queryId);
              return wrappedReject(new Error("Prolog process not available"));
            }
            this.process.stdin.write(envelope + "\n");
          } catch (e) {
            // Fail fast if write fails - handle EPIPE specifically
            this.queryPromises.delete(queryId);
            const error = e as NodeJS.ErrnoException;
            if (error.message?.includes("EPIPE") || error.code === "EPIPE") {
              return wrappedReject(new Error("Prolog process connection lost (EPIPE)"));
            }
            return wrappedReject(e);
          }

          // Timeout after configurable duration
          const queryTimeoutMs = Number.parseInt(
            process.env.SWI_MCP_QUERY_TIMEOUT_MS || "30000",
            10,
          );
          const qTimeout =
            Number.isFinite(queryTimeoutMs) && queryTimeoutMs > 0 ? queryTimeoutMs : DEFAULT_READY_TIMEOUT_MS;
          const timer: NodeJS.Timeout = setTimeout(() => {
            if (this.queryPromises.has(queryId)) {
              this.queryPromises.delete(queryId);
              logger.warn(`Query timeout after ${qTimeout}ms for command: ${command.substring(0, 100)}${command.length > 100 ? '...' : ''}`);
              wrappedReject(new Error(`Query timeout after ${qTimeout}ms. Try increasing SWI_MCP_QUERY_TIMEOUT_MS environment variable.`));
            }
          }, qTimeout);
          timer.unref?.();
        });

      // Chain into the queue to ensure serialization
      this.commandQueue = this.commandQueue
        .then(() => run())
        .catch((err) => {
          // If the previous task failed unexpectedly, still propagate error for this command
          try {
            outerReject(err);
          } catch { }
        });
    });
  }

  /**
   * Consult (load) a Prolog file
   */
  async consultFile(filename: string): Promise<string> {
    const absolutePath = path.resolve(filename);
    const escaped = absolutePath.replace(/'/g, "\\'");
    return this.query(`consult('${escaped}')`);
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
    this.sessionState = "engine";
    // Commands are serialized; prior closes complete before this runs
    // Send start_engine command to server
    const result = await this.sendCommand(`start_engine(${query})`);
    // Reject on any non-ok response from server
    if (result !== "ok") {
      this.engineActive = false;
      this.sessionState = "idle";
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
    if (!this.engineActive || this.sessionState !== "engine") {
      return { error: "No active engine. Start an engine first.", more_solutions: false };
    }

    this.assertRunning();

    try {
      const result = await this.sendCommand("next_engine");
      const parsed = this.parseServerResult(result);
      if (parsed.kind === "eof") {
        this.engineActive = false;
        this.sessionState = "idle";
        return { more_solutions: false };
      }
      if (parsed.kind === "error") {
        this.engineActive = false;
        this.sessionState = "idle";
        return { error: parsed.error, more_solutions: false };
      }
      if (parsed.kind === "solution") {
        return { solution: parsed.value, more_solutions: true };
      }
      return { solution: parsed.value, more_solutions: true };
    } catch (error) {
      this.engineActive = false;
      this.sessionState = "idle";
      return {
        error: error instanceof Error ? error.message : String(error),
        more_solutions: false,
      };
    }
  }

  /**
   * Close the current engine session
   */
  async closeEngine(): Promise<{ status: string }> {
    if (!this.engineActive || this.sessionState !== "engine") {
      return { status: "no_active_engine" };
    }

    this.assertRunning();

    try {
      this.sessionState = "closing_engine";
      await this.sendCommand("close_engine");
    } catch (_error) {
      // Ignore errors when closing
    }

    this.engineActive = false;
    this.sessionState = "idle";

    return { status: "closed" };
  }

  /**
   * Stop the Prolog process
   */
  stop(): void {
    const proc = this.process;
    if (proc) {
      try {
        // Ask server to exit cleanly
        if (proc.stdin && !proc.killed) {
          try {
            // Swallow potential EPIPE on closed pipe
            (proc.stdin as NodeJS.WritableStream).once?.("error", () => { });
            proc.stdin.write("__EXIT__\n");
          } catch { }
          try {
            (proc.stdin as NodeJS.WritableStream).once?.("error", () => { });
            proc.stdin.end();
          } catch { }
        }
        // Remove listeners to avoid leaks
        try {
          proc.removeAllListeners("error");
        } catch { }
        try {
          proc.removeAllListeners("exit");
        } catch { }
        try {
          proc.stdout?.removeAllListeners("data");
        } catch { }
      } catch { }
      // Best-effort terminate after a short delay
      const timer: NodeJS.Timeout = setTimeout(() => {
        try {
          if (!proc.killed) proc.kill("SIGTERM");
        } catch { }
      }, STOP_KILL_DELAY_MS);
      timer.unref?.();
    }
    this.process = null;
    // Reject and clear any pending queries to prevent leaks
    for (const [_id, promise] of this.queryPromises) {
      try {
        promise.reject(new Error("Prolog server stopped"));
      } catch { }
    }
    this.queryPromises.clear();
    this.queryActive = false;
    this.engineActive = false;
    this.currentQuery = null;
    this.readyPromise = null;
    this.readyResolver = null;
    this.sessionState = "idle";
  }
}
