/**
 * @vpursuit/mcp-roots
 *
 * Dynamic filesystem root discovery for MCP servers
 */

import type { Plugin } from "@vpursuit/mcp-core";
import { z } from "zod";
import { RootsManager } from "./roots.js";

export { RootsManager } from "./roots.js";
export type { RootDirectory, PathValidationResult } from "./roots.js";
export { serverRef, logger } from "./logger.js";

/**
 * Roots plugin for MCP servers
 *
 * Provides dynamic filesystem root discovery with:
 * - Client-provided roots via MCP protocol
 * - Fallback to ~/.swipl-mcp-server
 * - Path validation and security checks
 * - Root change notifications
 */
export const plugin: Plugin = {
  name: "mcp-roots",
  version: "1.0.0",
  description: "Dynamic filesystem root discovery for MCP servers",

  tools: {
    roots_list: {
      title: "List Filesystem Roots",
      description: "List all filesystem roots currently known to the server",
      inputSchema: {},
      handler: async (_extra) => {
        const manager = RootsManager.getInstance();
        const roots = await manager.getRoots();

        const rootsList = roots.map((r) => ({
          uri: r.uri,
          path: r.path,
          name: r.name,
        }));

        return {
          content: [
            {
              type: "text" as const,
              text: JSON.stringify(
                {
                  success: true,
                  roots: rootsList,
                  count: roots.length,
                },
                null,
                2
              ),
            },
          ],
        };
      },
    },
  },

  async onInit(server) {
    const manager = RootsManager.getInstance();
    manager.setServerInstance(server as any);

    // Import and set server reference for logging
    const { serverRef } = await import("./logger.js");
    serverRef.current = server as any;

    // Perform initial discovery
    await manager.discoverRoots(true);
    const { logger } = await import("./logger.js");
    logger.info("Roots manager initialized");
  },

  async onShutdown() {
    // Cleanup if needed
    const { logger } = await import("./logger.js");
    logger.info("Shutting down");
  },
};
