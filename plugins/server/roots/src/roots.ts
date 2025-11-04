import path from "path";
import os from "os";
import { fileURLToPath } from "url";
import { logger } from "./logger.js";
import { ROOT_CACHE_TTL_MS, LIST_ROOTS_TIMEOUT_MS, DEFAULT_FALLBACK_DIR, BLOCKED_SYSTEM_DIRS } from "./constants.js";
import { RootsListChangedNotificationSchema } from "@modelcontextprotocol/sdk/types.js";

/**
 * Represents a root directory discovered from the MCP client
 */
export interface RootDirectory {
  uri: string;           // file:// URI
  path: string;          // Absolute filesystem path
  name?: string;         // Optional display name
}

/**
 * Result of path validation
 */
export interface PathValidationResult {
  allowed: boolean;
  error?: string;
  matchedRoot?: RootDirectory;
}

/**
 * Manages MCP roots for filesystem access control
 *
 * This class handles:
 * - Discovery of roots from MCP client via listRoots()
 * - Caching with 5-minute TTL for performance
 * - Notification-based cache invalidation (when client supports roots/list_changed)
 * - Environment variable configuration (SWI_MCP_ALLOWED_ROOTS)
 * - Path validation against allowed roots
 * - System directory blocking for security
 * - Secure by default (explicit configuration required)
 *
 * Cache Strategy:
 * - Always uses 5-minute cache TTL (protects against excessive MCP calls)
 * - Listens for roots/list_changed notifications (if client supports)
 * - On notification: invalidates cache immediately, triggers fresh discovery
 * - Without notifications: falls back to periodic 5-minute refresh
 */
export class RootsManager {
  private static instance: RootsManager | null = null;
  private roots: RootDirectory[] = [];
  private lastDiscoveryTime: number = 0;
  private serverInstance: any = null; // Will be McpServer instance
  private notificationsRegistered: boolean = false;
  private hasLoggedNoRootsWarning: boolean = false;

  private constructor() {
    // No fallback directory - explicit configuration required
  }

  /**
   * Get the singleton instance
   */
  static getInstance(): RootsManager {
    if (!RootsManager.instance) {
      RootsManager.instance = new RootsManager();
    }
    return RootsManager.instance;
  }

  /**
   * Set the MCP server instance for roots discovery
   */
  setServerInstance(server: any): void {
    logger.debug("setServerInstance called");
    this.serverInstance = server;
    logger.debug(`Server instance set, has .server property: ${!!server?.server}`);
    this.registerNotificationHandler();
  }

  /**
   * Register handler for roots/list_changed notifications
   *
   * This method uses the official MCP SDK API to register a notification handler
   * via server.server.setNotificationHandler(). The McpServer.server property
   * provides access to the underlying Protocol instance for advanced operations.
   *
   * The handler is registered regardless of advertised client capabilities because
   * some clients (e.g., MCP Inspector) send notifications without advertising support.
   *
   * When a notification is received, the roots cache is invalidated and roots are
   * immediately re-discovered from the client.
   *
   * @see https://modelcontextprotocol.io - MCP Specification
   * @see Protocol.setNotificationHandler in @modelcontextprotocol/sdk
   */
  private registerNotificationHandler(): void {
    logger.debug("registerNotificationHandler called");
    logger.debug(`Already registered: ${this.notificationsRegistered}, Has server: ${!!this.serverInstance}`);

    if (this.notificationsRegistered || !this.serverInstance) {
      logger.debug("Skipping registration - already registered or no server instance");
      return;
    }

    try {
      // Check if client advertises roots notification support
      const clientCaps = this.serverInstance.getClientCapabilities?.();
      logger.debug(`Client capabilities: ${JSON.stringify(clientCaps)}`);
      const supportsNotifications = clientCaps?.roots?.listChanged ?? false;
      logger.info(`Client advertises roots/list_changed support: ${supportsNotifications}`);

      // Defensive check: ensure server instance provides Protocol access
      if (!this.serverInstance.server) {
        logger.warn("Server instance does not expose underlying protocol - notification handler not registered");
        return;
      }

      const protocol = this.serverInstance.server;
      logger.debug(`Protocol object type: ${typeof protocol}, has setNotificationHandler: ${typeof protocol.setNotificationHandler}`);

      // Defensive check: ensure Protocol supports notification handlers
      if (typeof protocol.setNotificationHandler !== 'function') {
        logger.warn("Server protocol does not support setNotificationHandler - notification handler not registered");
        return;
      }

      logger.debug("About to call protocol.setNotificationHandler...");

      // Register the notification handler (official MCP SDK API)
      // Always register regardless of advertised capabilities for maximum compatibility
      protocol.setNotificationHandler(
        RootsListChangedNotificationSchema,
        async (notification: any) => {
          logger.info(`🔔 NOTIFICATION RECEIVED! Method: ${notification.method}`);
          logger.debug(`Notification handler called! Notification object: ${JSON.stringify(notification)}`);
          await this.handleRootsChanged();
        }
      );

      logger.info("✅ Successfully registered roots/list_changed notification handler");
      logger.debug(`Handler registered for method: ${(RootsListChangedNotificationSchema.shape.method as any)._def.value}`);
      this.notificationsRegistered = true;
    } catch (error) {
      logger.error(`❌ Could not register notification handler`, error instanceof Error ? error : new Error(String(error)));
    }
  }

  /**
   * Handle roots/list_changed notification from client
   * Called when client notifies that roots have changed
   */
  private async handleRootsChanged(): Promise<void> {
    logger.info("Received roots/list_changed notification - refreshing roots");
    logger.debug(`Before invalidation - Cache age: ${Date.now() - this.lastDiscoveryTime}ms, Cached roots: ${this.roots.length}`);

    this.invalidateCache();
    logger.debug("Cache invalidated - lastDiscoveryTime reset to 0");

    // Small delay to allow client to update its roots list before we query it
    // Some clients may send the notification before their internal state is fully updated
    logger.debug("Waiting 100ms for client state update...");
    await new Promise(resolve => setTimeout(resolve, 100));

    // Re-discover roots with forced refresh to bypass cache
    logger.debug("Triggering root discovery...");
    const rootsDiscovered = await this.discoverRoots(true);

    // Log the updated roots
    if (rootsDiscovered) {
      const roots = await this.getRoots();
      logger.info(`Roots refreshed: ${roots.length} root(s) now available`);
      roots.forEach(root => {
        logger.info(`  - ${root.path}${root.name ? ` (${root.name})` : ''}`);
      });
      logger.debug(`Root URIs: ${roots.map(r => r.uri).join(", ")}`);
    } else {
      const fallbackDir = this.getFallbackDir();
      logger.info(`Roots refresh: No roots from client, using fallback: ${fallbackDir}`);
      logger.debug("No client roots discovered - using fallback directory");
    }
  }

  /**
   * Convert file:// URI to filesystem path
   *
   * Handles both strict RFC 8089 format (file:///path) and lenient format (file://path)
   * for compatibility with various MCP clients.
   */
  private uriToPath(uri: string): string | null {
    try {
      // Validate URI format
      if (!uri.startsWith("file://")) {
        logger.warn(`Invalid URI format (must start with file://): ${uri}`);
        return null;
      }

      // Normalize URI: fix common client mistakes like file://path instead of file:///path
      let normalizedUri = uri;
      if (uri.startsWith("file://") && !uri.startsWith("file:///")) {
        // Missing slash for absolute path - add it
        normalizedUri = "file:///" + uri.slice(7);
        logger.debug(`Normalized URI from ${uri} to ${normalizedUri}`);
      }

      // Use Node.js built-in URL parsing
      const fsPath = fileURLToPath(normalizedUri);
      return path.resolve(fsPath);
    } catch (error) {
      logger.warn(`Failed to convert URI to path: ${uri} - ${error instanceof Error ? error.message : String(error)}`);
      return null;
    }
  }

  /**
   * Check if path is in a blocked system directory
   */
  private isBlockedSystemPath(filePath: string): boolean {
    const absolutePath = path.resolve(filePath);

    for (const blockedDir of BLOCKED_SYSTEM_DIRS) {
      const normalizedBlocked = path.normalize(blockedDir);
      const normalizedPath = path.normalize(absolutePath);

      // Check if path starts with blocked directory
      if (normalizedPath === normalizedBlocked ||
          normalizedPath.startsWith(normalizedBlocked + path.sep)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Discover roots from MCP client
   * Returns true if discovery was successful, false if using fallback
   *
   * @param forceRefresh - If true, bypass cache and force fresh discovery from client
   */
  async discoverRoots(forceRefresh: boolean = false): Promise<boolean> {
    // Check if cache is still valid (unless force refresh requested)
    const now = Date.now();
    const cacheAge = now - this.lastDiscoveryTime;

    if (!forceRefresh && cacheAge < ROOT_CACHE_TTL_MS && this.roots.length > 0) {
      logger.debug(`Using cached roots (age: ${cacheAge}ms)`);
      return true;
    }

    if (forceRefresh) {
      logger.info("Force refresh requested - bypassing cache and discovering roots from client");
    }

    // Check for environment variable override
    const envRoots = process.env.SWI_MCP_ALLOWED_ROOTS;
    if (envRoots) {
      logger.info("Using roots from SWI_MCP_ALLOWED_ROOTS environment variable");

      // Parse comma-separated absolute paths only
      const paths = envRoots.split(",").map(p => p.trim()).filter(p => p.length > 0);
      const validRoots: RootDirectory[] = [];

      for (const p of paths) {
        // Require absolute paths
        if (!path.isAbsolute(p)) {
          logger.warn(`Ignoring relative path in SWI_MCP_ALLOWED_ROOTS: ${p} (absolute paths required)`);
          continue;
        }

        // Resolve and normalize the path
        const resolvedPath = path.resolve(p);
        validRoots.push({
          uri: `file://${resolvedPath}`,
          path: resolvedPath,
          name: `Environment: ${path.basename(resolvedPath)}`
        });
        logger.info(`Added root from environment: ${resolvedPath}`);
      }

      this.roots = validRoots;
      this.lastDiscoveryTime = now;

      if (validRoots.length === 0) {
        logger.warn("SWI_MCP_ALLOWED_ROOTS set but no valid absolute paths found");
        return false;
      }

      return true;
    }

    // Try to discover from MCP server
    if (!this.serverInstance) {
      logger.debug("No server instance available - no roots will be configured");
      this.logNoRootsWarning();
      return false;
    }

    try {
      logger.debug("Requesting roots from MCP client via listRoots()...");

      // Wrap listRoots() with timeout to prevent indefinite hanging
      // McpServer wraps the underlying Server, so we need to access .server.listRoots()
      const listRootsPromise = this.serverInstance.server.listRoots();
      const timeoutPromise = new Promise<never>((_, reject) => {
        setTimeout(() => {
          reject(new Error(`MCP protocol error: Client did not respond to listRoots() within ${LIST_ROOTS_TIMEOUT_MS}ms. This indicates a communication failure, not missing roots configuration.`));
        }, LIST_ROOTS_TIMEOUT_MS);
      });

      const response = await Promise.race([listRootsPromise, timeoutPromise]);

      logger.debug(`listRoots() response received - roots count: ${response?.roots?.length ?? "N/A"}`);
      logger.debug(`Full response: ${JSON.stringify(response)}`);

      if (!response || !response.roots || !Array.isArray(response.roots)) {
        logger.info("Client did not provide roots array in response");
        logger.debug(`Response structure: ${JSON.stringify(response)}`);
        // Clear cached roots since client provided invalid response
        this.roots = [];
        this.lastDiscoveryTime = now;
        logger.debug("Cleared cached roots due to invalid response");
        this.logNoRootsWarning();
        return false;
      }

      if (response.roots.length === 0) {
        logger.info("Client provided empty roots list");
        logger.debug("Empty roots array received from client");
        // Clear cached roots since client has no roots configured
        this.roots = [];
        this.lastDiscoveryTime = now;
        logger.debug("Cleared cached roots - client has no roots");
        this.logNoRootsWarning();
        return false;
      }

      logger.debug(`Processing ${response.roots.length} root(s) from client...`);

      // Convert URIs to paths
      const discoveredRoots: RootDirectory[] = [];
      for (const root of response.roots) {
        logger.debug(`Converting root URI: ${root.uri} (name: ${root.name ?? "none"})`);
        const fsPath = this.uriToPath(root.uri);
        if (fsPath) {
          discoveredRoots.push({
            uri: root.uri,
            path: fsPath,
            name: root.name
          });
          logger.info(`Discovered root: ${fsPath}${root.name ? ` (${root.name})` : ''}`);
          logger.debug(`  → Converted to path: ${fsPath}`);
        } else {
          logger.warn(`Failed to convert root URI to path: ${root.uri}`);
        }
      }

      if (discoveredRoots.length === 0) {
        logger.warn("No valid file:// URIs found in client roots");
        logger.debug("All URI conversions failed");
        // Clear cached roots since no valid URIs could be converted
        this.roots = [];
        this.lastDiscoveryTime = now;
        logger.debug("Cleared cached roots - no valid URIs");
        this.logNoRootsWarning();
        return false;
      }

      logger.debug(`Setting ${discoveredRoots.length} discovered roots and updating cache`);
      this.roots = discoveredRoots;
      this.lastDiscoveryTime = now;
      logger.info(`Successfully discovered ${this.roots.length} root(s)`);
      logger.debug(`Cache updated - lastDiscoveryTime: ${this.lastDiscoveryTime}`);
      return true;

    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      const errorStack = error instanceof Error ? error.stack : '';

      // Timeout errors indicate MCP protocol failure - must propagate as error
      if (errorMsg.includes('MCP protocol error')) {
        logger.error(`MCP protocol failure during roots discovery: ${errorMsg}`);
        throw error; // Re-throw timeout errors - these are FAILURES, not "no roots"
      }

      // Other errors (e.g., client doesn't implement listRoots) are acceptable
      logger.info(`Unable to discover roots from client: ${errorMsg}`);
      logger.debug(`Root discovery error details: ${errorStack || errorMsg}`);
      this.logNoRootsWarning();
      return false;
    }
  }

  /**
   * Log informational warning when no roots are configured (once per instance)
   */
  private logNoRootsWarning(): void {
    if (this.hasLoggedNoRootsWarning) {
      return;
    }
    this.hasLoggedNoRootsWarning = true;

    logger.info("⚠️  No filesystem roots configured - file operations will be disabled");
    logger.info("Configure roots via:");
    logger.info("  1. MCP client roots configuration");
    logger.info("  2. Environment variable: SWI_MCP_ALLOWED_ROOTS=/path/one,/path/two");
  }

  /**
   * Invalidate the roots cache
   */
  invalidateCache(): void {
    logger.debug("Invalidating roots cache");
    this.lastDiscoveryTime = 0;
  }

  /**
   * Get all allowed paths (discovered roots only)
   */
  async getAllowedPaths(): Promise<string[]> {
    await this.discoverRoots();
    return this.roots.map(r => r.path);
  }

  /**
   * Get all roots with their metadata
   */
  async getRoots(): Promise<RootDirectory[]> {
    await this.discoverRoots();
    return [...this.roots];
  }

  /**
   * Get fallback directory path
   * @deprecated No fallback directory - explicit root configuration required
   */
  getFallbackDir(): string {
    return ""; // No fallback directory
  }

  /**
   * Get comprehensive roots information for introspection
   *
   * Returns all roots with their metadata, source information, and cache status.
   * Useful for debugging and transparency about filesystem access restrictions.
   */
  async getRootsInfo(): Promise<{
    roots: RootDirectory[];
    source: "client" | "environment" | "none";
    cacheAge: number;
    cacheTtl: number;
  }> {
    const rootsDiscovered = await this.discoverRoots();

    // Determine source of roots
    let source: "client" | "environment" | "none";
    if (process.env.SWI_MCP_ALLOWED_ROOTS) {
      source = "environment";
    } else if (rootsDiscovered && this.roots.length > 0) {
      source = "client";
    } else {
      source = "none";
    }

    // Calculate cache age, handling case where discovery never happened
    const cacheAge = this.lastDiscoveryTime === 0
      ? -1  // Indicates never discovered
      : Date.now() - this.lastDiscoveryTime;

    return {
      roots: [...this.roots],
      source,
      cacheAge,
      cacheTtl: ROOT_CACHE_TTL_MS
    };
  }

  /**
   * Validate if a file path is allowed
   */
  async validatePath(filePath: string): Promise<PathValidationResult> {
    try {
      const absolutePath = path.resolve(filePath);

      // Check for blocked system directories first
      if (this.isBlockedSystemPath(absolutePath)) {
        return {
          allowed: false,
          error: `Security Error: Access to system directory '${path.dirname(absolutePath)}' is not permitted`
        };
      }

      // Discover roots (uses cache if available)
      await this.discoverRoots();

      // If no roots configured, deny access with helpful error
      if (this.roots.length === 0) {
        return {
          allowed: false,
          error: this.formatNoRootsError()
        };
      }

      // Check against discovered roots
      for (const root of this.roots) {
        const relativeToRoot = path.relative(root.path, absolutePath);
        const isWithinRoot = !relativeToRoot.startsWith('..') && !path.isAbsolute(relativeToRoot);

        if (isWithinRoot) {
          return {
            allowed: true,
            matchedRoot: root
          };
        }
      }

      // Path not in any allowed root
      return {
        allowed: false,
        error: this.formatNotAllowedError(absolutePath)
      };

    } catch (_error) {
      return {
        allowed: false,
        error: `Security Error: Invalid file path`
      };
    }
  }

  /**
   * Format error message when no roots are configured
   */
  private formatNoRootsError(): string {
    return `Security Error: No filesystem roots configured. File access is disabled.

Configure roots via:
1. MCP Client: Configure roots in your MCP client settings
2. Environment Variable: SWI_MCP_ALLOWED_ROOTS=/path/one,/path/two

Example (Claude Desktop config):
{
  "mcpServers": {
    "swipl": {
      "env": {
        "SWI_MCP_ALLOWED_ROOTS": "/Users/you/prolog"
      }
    }
  }
}`;
  }

  /**
   * Format error message for disallowed paths
   */
  private formatNotAllowedError(attemptedPath: string): string {
    const allowedPaths = this.roots.map(r => {
      const displayPath = r.path;
      const displayName = r.name ? ` (${r.name})` : '';
      return `  - ${displayPath}${displayName}`;
    });

    if (allowedPaths.length === 0) {
      return this.formatNoRootsError();
    }

    return `Security Error: File must be within allowed roots:\n${allowedPaths.join('\n')}\nAttempted: ${attemptedPath}`;
  }
}

// Export singleton instance getter
export const rootsManager = RootsManager.getInstance();
