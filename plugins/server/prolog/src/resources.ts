/**
 * Resource definitions for the Prolog MCP plugin
 *
 * Provides static and dynamic resources for:
 * - Knowledge base introspection (predicates, dump)
 * - Documentation (help, license)
 * - Metadata (capabilities, logo)
 */

import { promises as fs } from "fs";
import type { ResourceDefinitions, ReadResourceResult } from "@vpursuit/mcp-server-core";
import { findNearestFile } from "@vpursuit/mcp-server-core";
import { prologInterface, getCapabilitiesSummary, tools } from "./tools.js";

/**
 * Helper function to convert Prolog list string to readable format
 */
function formatPrologList(s: string): string {
  const trimmed = String(s).trim();
  if (trimmed.startsWith("[") && trimmed.endsWith("]")) {
    const inner = trimmed.slice(1, -1).trim();
    if (!inner) return "(no exported predicates)";
    return inner.split(",").map((p) => p.trim()).join("\n");
  }
  return trimmed || "(no exported predicates)";
}

/**
 * MCP Resources
 *
 * Unified mcp:// URI scheme with hierarchical structure:
 * - mcp://workspace/* - Current workspace state (mutable)
 * - mcp://server/* - Server metadata (mostly static)
 *
 * Provides read-only access to:
 * 1. Workspace state (symbols, snapshot)
 * 2. Server metadata (capabilities, branding)
 *
 * Note: Software license is in LICENSE file (included in npm package).
 */
export const resources: ResourceDefinitions = {
  /**
   * List all predicates defined in the workspace
   */
  "workspace-symbols": {
    uri: "mcp://workspace/symbols",
    name: "Workspace Symbols",
    description: "List all user-defined predicates in the workspace",
    mimeType: "text/plain",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      await prologInterface.start();
      // Use same query as workspace tool's list_symbols operation
      const preds = await prologInterface.query("list_predicates");
      const text = formatPrologList(preds);

      return {
        contents: [{
          uri: uri.toString(),
          mimeType: "text/plain",
          text,
        }]
      };
    },
  },

  /**
   * Get workspace snapshot with original source text
   */
  "workspace-snapshot": {
    uri: "mcp://workspace/snapshot",
    name: "Workspace Snapshot",
    description: "Get workspace snapshot containing original source text with preserved formatting and variable names",
    mimeType: "text/prolog",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      await prologInterface.start();
      const snapshot = await prologInterface.getSnapshot();

      return {
        contents: [{
          uri: uri.toString(),
          mimeType: "text/prolog",
          text: snapshot || "(empty workspace)",
        }]
      };
    },
  },

  /**
   * Official swipl-mcp-server logo (SVG)
   */
  "logo": {
    uri: "mcp://server/branding/logo",
    name: "Server Logo",
    description: "Server logo image in SVG format",
    mimeType: "image/svg+xml",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      const logoPath = findNearestFile("images/logo.svg");

      if (!logoPath) {
        return {
          contents: [{
            uri: uri.toString(),
            mimeType: "text/plain",
            text: "Logo unavailable",
          }]
        };
      }

      const svg = await fs.readFile(logoPath, "utf8");
      return {
        contents: [{
          uri: uri.toString(),
          mimeType: "image/svg+xml",
          text: svg,
        }]
      };
    },
  },

  /**
   * Machine-readable summary of tools, modes, environment, and safety features
   */
  "capabilities": {
    uri: "mcp://server/capabilities",
    name: "Server Capabilities",
    description: "Machine-readable summary of tools, query modes, environment, and security model",
    mimeType: "application/json",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      const caps = getCapabilitiesSummary();
      const json = JSON.stringify(caps, null, 2);

      return {
        contents: [{
          uri: uri.toString(),
          mimeType: "application/json",
          text: json,
        }]
      };
    },
  },
};
