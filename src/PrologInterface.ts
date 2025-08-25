import { spawn, ChildProcess } from "child_process";
import path from "path";
import fs from "fs";
import { fileURLToPath } from "url";
import { logger } from "./logger.js";

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

    // Trace: show cwd only when explicitly enabled
    const traceFlag = (process.env.SWI_MCP_TRACE || "").toLowerCase();
    const traceEnabledStart = traceFlag === "1" || traceFlag === "true" || traceFlag === "yes";
    if (traceEnabledStart) {
      logger.debug(`cwd: ${process.cwd()}`);
    }

    // Resolve Prolog server script robustly: env override, cwd, and paths relative to this module
    const envPath = process.env.SWI_MCP_PROLOG_PATH;
    // Directory of the compiled module (e.g., build/)
    let moduleDir = "";
    try {
      const metaUrl = Function(
        "try { return import.meta && import.meta.url } catch (_) { return '' }",
      )() as unknown as string;
      if (metaUrl && typeof metaUrl === "string") {
        moduleDir = path.dirname(fileURLToPath(metaUrl));
      }
    } catch {}
    // Directory of the entry script (e.g., build/ when running `node build/index.js`)
    const entryDir = (() => {
      try {
        return path.dirname(process.argv?.[1] || "");
      } catch {
        return "";
      }
    })();
    const candidates: string[] = [];
    if (envPath) candidates.push(path.resolve(envPath));
    // src folder relative to current working directory
    candidates.push(
      path.resolve(process.cwd(), "src", "prolog_server.pl"),
      path.resolve(process.cwd(), "..", "src", "prolog_server.pl"),
    );
    // Relative to compiled module (build folder) - look for src folder
    if (moduleDir) {
      candidates.push(
        path.resolve(moduleDir, "..", "src", "prolog_server.pl"),
        path.resolve(moduleDir, "prolog_server.pl"), // Same folder as compiled JS
        path.resolve(moduleDir, "..", "..", "src", "prolog_server.pl"),
        path.resolve(moduleDir, "..", "prolog", "server.pl"), // NPM package structure: lib/../prolog/server.pl
      );
    }
    if (entryDir) {
      candidates.push(
        path.resolve(entryDir, "..", "src", "prolog_server.pl"),
        path.resolve(entryDir, "prolog_server.pl"), // Same folder as entry script
        path.resolve(entryDir, "..", "..", "src", "prolog_server.pl"),
        path.resolve(entryDir, "..", "prolog", "server.pl"), // NPM package structure: lib/../prolog/server.pl
      );
    }
    // De-duplicate while preserving order
    const seen = new Set<string>();
    const possiblePaths = candidates.filter((p) => {
      if (!p) return false;
      if (seen.has(p)) return false;
      seen.add(p);
      return true;
    });

    let serverScript: string | null = null;

    for (const scriptPath of possiblePaths) {
      if (traceEnabledStart) {
        logger.debug(`Trying: ${logger.redactPath(scriptPath)}`);
      }
      try {
        fs.accessSync(scriptPath, fs.constants.F_OK);
        logger.info(`Found Prolog server script at: ${logger.redactPath(scriptPath)}`);
        serverScript = scriptPath;
        break;
      } catch {
        if (traceEnabledStart) logger.debug(`Not found at: ${logger.redactPath(scriptPath)}`);
      }
    }

    if (!serverScript) {
      const hint = `cwd=${process.cwd()} moduleDir=${moduleDir || "n/a"} entryDir=${entryDir || "n/a"}`;
      throw new Error(
        `Prolog server script not found. Tried: ${possiblePaths.join(", ")} (${hint}). Set SWI_MCP_PROLOG_PATH to override.`,
      );
    }

    // Ensure we reset readiness state before starting
    this.isReady = false;

    const extendedOn = /^(1|true|yes)$/i.test(String(process.env.SWI_MCP_EXTENDED_SAFE || ""));
    const traceOn = /^(1|true|yes)$/i.test(String(process.env.SWI_MCP_TRACE || ""));
    const args = ["-q", "-s", serverScript] as string[];
    if (extendedOn) {
      args.push("-g", "assert(safe_extended_enabled)");
    }
    if (traceOn) {
      args.push("-g", "assert(swi_mcp_trace_enabled)");
    }
    args.push("-g", "server_loop", "-t", "halt");
    
    try {
      this.process = spawn("swipl", args, {
        stdio: ["pipe", "pipe", "pipe"],
      });
    } catch (error) {
      const err = error as NodeJS.ErrnoException;
      if (err.code === "ENOENT") {
        throw new Error("SWI-Prolog not found in PATH. Please install SWI-Prolog and ensure 'swipl' command is available.");
      }
      throw new Error(`Failed to start SWI-Prolog process: ${err.message}`);
    }

    logger.info(`Started Prolog server (${logger.redactPid(this.process.pid)})`);

    if (!this.process.stdout || !this.process.stdin) {
      throw new Error("Failed to create SWI-Prolog server process streams");
    }
    
    // Handle spawn errors that occur after process creation
    this.process.on("error", (error: Error) => {
      const err = error as NodeJS.ErrnoException;
      if (err.code === "ENOENT") {
        logger.error("SWI-Prolog executable not found in PATH");
      } else {
        logger.error(`Prolog process error: ${err.message}`);
      }
      if (!this.isReady && this.readyRejecter) {
        this.readyRejecter(new Error(`Prolog process failed to start: ${err.message}`));
      }
    });

    this.process.stdout.on("data", (data: Buffer) => {
      const output = data.toString("utf8");
      this.handleResponse(output);
    });

    this.process.on("error", (error) => {
      // Log, and if not ready yet, fail the wait immediately
      logger.error(`Prolog server error: ${error.message}`);
      if (!this.isReady && this.readyRejecter) {
        const rej = this.readyRejecter;
        this.readyRejecter = null;
        try {
          rej(error instanceof Error ? error : new Error(String(error)));
        } catch {}
      }
    });

    this.process.on("exit", (_code, _signal) => {
      this.process = null;
      // Reject any pending queries
      for (const [_id, promise] of this.queryPromises) {
        promise.reject(new Error("Prolog server exited"));
      }
      this.queryPromises.clear();
      if (!this.isReady && this.readyRejecter) {
        const rej = this.readyRejecter;
        this.readyRejecter = null;
        try {
          rej(new Error("Prolog server exited before ready"));
        } catch {}
      }
    });

    // Wait for READY signal from Prolog server
    await this.waitForReady();
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
      const readyTimeoutMs = Number.parseInt(process.env.SWI_MCP_READY_TIMEOUT_MS || "5000", 10);
      const timeout = Number.isFinite(readyTimeoutMs) && readyTimeoutMs > 0 ? readyTimeoutMs : 5000;
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
          } catch {}
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

  /**
   * Process a complete response line from Prolog server
   */
  private processResponseLine(response: string): void {
    // Ignore internal debug markers from the Prolog server
    if (response.startsWith("@@DEBUG@@")) {
      const traceFlag = (process.env.SWI_MCP_TRACE || "").toLowerCase();
      const traceEnabled = traceFlag === "1" || traceFlag === "true" || traceFlag === "yes";
      if (traceEnabled) logger.debug(`Prolog debug: ${response.slice(9).trim()}`);
      return;
    }
    // Avoid logging sensitive response bodies in normal operation
    // Log a summary at debug level only
    {
      const traceFlag = (process.env.SWI_MCP_TRACE || "").toLowerCase();
      const traceEnabled = traceFlag === "1" || traceFlag === "true" || traceFlag === "yes";
      if (traceEnabled) {
        const tag =
          response === "@@READY@@"
            ? "READY"
            : response.startsWith("solution(")
              ? "solution"
              : response.startsWith("error(")
                ? "error"
                : response === "no_more_solutions"
                  ? "eof"
                  : "other";
        logger.debug(`Prolog response: ${tag}`);
      }
    }

    // Check for READY signal
    if (response === "@@READY@@") {
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

    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

    this.currentQuery = query;
    this.queryActive = true;
    this.sessionState = "query";

    // Send start_query command to server
    const result = await this.sendCommand(`start_query(${query})`);
    // If Prolog responded with an error(...) term, reject
    if (typeof result === "string" && result.startsWith("error(")) {
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

    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

    try {
      const result = await this.sendCommand("next_solution");

      if (result === "no_more_solutions") {
        this.queryActive = false;
        this.currentQuery = null;
        this.sessionState = "idle";
        return { more_solutions: false };
      }

      // Handle error(...) terms from unified server
      if (result.startsWith("error(")) {
        this.queryActive = false;
        this.currentQuery = null;
        this.sessionState = "idle";
        return { error: result, more_solutions: false };
      }

      // Handle solution(...) terms from unified server
      if (result.startsWith("solution(")) {
        // Extract the inside of solution(...)
        const innerMatch = result.match(/^solution\((.*)\)$/);
        if (innerMatch) {
          const inner = innerMatch[1]; // typically like: [ 'X'=val, 'Y'=val ] or []
          // Remove quotes around variable names only (keep term formatting intact)
          const cleaned = inner.replace(/'([A-Za-z_][A-Za-z0-9_]*)'=/g, "$1=");
          const display = cleaned.trim() === "[]" ? "true" : cleaned;
          return { solution: display, more_solutions: true };
        }
        // Fallback for malformed solution terms
        return { solution: result, more_solutions: true };
      }

      // Fallback for other response formats
      return {
        solution: result,
        more_solutions: true,
      };
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

    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

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
    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

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
              } catch {}
            }
          };
          const wrappedReject = (err: any) => {
            try {
              outerReject(err instanceof Error ? err : new Error(String(err)));
            } finally {
              try {
                done();
              } catch {}
            }
          };

          this.queryPromises.set(queryId, { resolve: wrappedResolve, reject: wrappedReject });

          // Send command to server
          const traceFlag = (process.env.SWI_MCP_TRACE || "").toLowerCase();
          const traceEnabled = traceFlag === "1" || traceFlag === "true" || traceFlag === "yes";
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
            Number.isFinite(queryTimeoutMs) && queryTimeoutMs > 0 ? queryTimeoutMs : 5000;
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
          } catch {}
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

    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

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

    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

    try {
      const result = await this.sendCommand("next_engine");

      if (result === "no_more_solutions") {
        this.engineActive = false;
        this.sessionState = "idle";
        return { more_solutions: false };
      }

      if (result.startsWith("error(")) {
        this.engineActive = false;
        this.sessionState = "idle";
        return { error: result, more_solutions: false };
      }
      // Use the same robust formatting as nextSolution
      if (result.startsWith("solution(")) {
        const innerMatch = result.match(/^solution\((.*)\)$/);
        if (innerMatch) {
          const inner = innerMatch[1];
          const cleaned = inner.replace(/'([A-Za-z_][A-Za-z0-9_]*)'=/g, "$1=");
          const display = cleaned.trim() === "[]" ? "true" : cleaned;
          return { solution: display, more_solutions: true };
        }
      }
      // Fallback: return raw
      return { solution: result, more_solutions: true };
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

    if (!this.process || !this.process.stdin) {
      throw new Error("Prolog server not started");
    }

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
            (proc.stdin as NodeJS.WritableStream).once?.("error", () => {});
            proc.stdin.write("__EXIT__\n");
          } catch {}
          try {
            (proc.stdin as NodeJS.WritableStream).once?.("error", () => {});
            proc.stdin.end();
          } catch {}
        }
        // Remove listeners to avoid leaks
        try {
          proc.removeAllListeners("error");
        } catch {}
        try {
          proc.removeAllListeners("exit");
        } catch {}
        try {
          proc.stdout?.removeAllListeners("data");
        } catch {}
      } catch {}
      // Best-effort terminate after a short delay
      const timer: NodeJS.Timeout = setTimeout(() => {
        try {
          if (!proc.killed) proc.kill("SIGTERM");
        } catch {}
      }, 50);
      timer.unref?.();
    }
    this.process = null;
    // Reject and clear any pending queries to prevent leaks
    for (const [_id, promise] of this.queryPromises) {
      try {
        promise.reject(new Error("Prolog server stopped"));
      } catch {}
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
