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
      description: "List all filesystem roots currently known to the server",
      inputSchema: z.object({}),
      handler: async () => {
        const manager = RootsManager.getInstance();
        const roots = await manager.getRoots();

        return {
          success: true,
          data: {
            roots: roots.map((r) => ({
              uri: r.uri,
              path: r.path,
              name: r.name,
            })),
            count: roots.length,
          },
        };
      },
    },
  },

  async onInit(server) {
    const manager = RootsManager.getInstance();
    manager.setServerInstance(server as any);
    // Perform initial discovery
    await manager.discoverRoots(true);
    console.log("[mcp-roots] ✓ Roots manager initialized");
  },

  async onShutdown() {
    // Cleanup if needed
    console.log("[mcp-roots] Shutting down");
  },
};
