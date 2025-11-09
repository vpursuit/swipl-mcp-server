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
 * Prolog MCP Resources
 *
 * These resources provide read-only access to:
 * 1. Knowledge base state (predicates, full dump)
 * 2. Server documentation (help, license)
 * 3. Server metadata (capabilities, logo)
 */
export const resources: ResourceDefinitions = {
  /**
   * List all predicates defined in the knowledge_base module
   */
  "knowledge-base-predicates": {
    uri: "prolog://knowledge_base/predicates",
    name: "Knowledge Base Predicates",
    description: "List all user-defined predicates in knowledge_base module with arity",
    mimeType: "text/plain",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      await prologInterface.start();
      const preds = await prologInterface.query("list_module_predicates(knowledge_base)");
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
   * Export current knowledge base as Prolog clauses
   */
  "knowledge-base-dump": {
    uri: "prolog://knowledge_base/dump",
    name: "Knowledge Base Dump",
    description: "Export all user-defined facts and rules as Prolog source text",
    mimeType: "text/prolog",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      await prologInterface.start();
      const dump = await prologInterface.query("dump_knowledge_base");

      return {
        contents: [{
          uri: uri.toString(),
          mimeType: "text/prolog",
          text: dump,
        }]
      };
    },
  },

  /**
   * Usage guidelines and tips for this server
   */
  "help": {
    uri: "reference://help",
    name: "Help",
    description: "Usage guidelines and reference documentation",
    mimeType: "text/plain",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      const res = await tools.help.handler({}, _extra);
      const text = (Array.isArray(res.content) && res.content[0]?.text
        ? res.content[0].text
        : "Help unavailable") as string;

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
   * License text for this software
   */
  "license": {
    uri: "reference://license",
    name: "License",
    description: "Software license text (BSD 3-Clause)",
    mimeType: "text/plain",
    handler: async (uri, _extra): Promise<ReadResourceResult> => {
      const res = await tools.license.handler({}, _extra);
      const text = (Array.isArray(res.content) && res.content[0]?.text
        ? res.content[0].text
        : "License unavailable") as string;

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
   * Official swipl-mcp-server logo (SVG)
   */
  "logo": {
    uri: "reference://logo",
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
    uri: "reference://capabilities",
    name: "Capabilities",
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
