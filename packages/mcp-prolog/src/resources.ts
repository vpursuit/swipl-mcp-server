/**
 * Resource definitions for the Prolog MCP plugin
 *
 * Provides static and dynamic resources for:
 * - Knowledge base introspection (predicates, dump)
 * - Documentation (help, license)
 * - Metadata (capabilities, logo)
 */

import { promises as fs } from "fs";
import type { ResourceDefinitions } from "@vpursuit/mcp-core";
import { findNearestFile } from "./meta.js";
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
    description: "List predicates defined in the knowledge_base module",
    mimeType: "text/plain",
    handler: async () => {
      await prologInterface.start();
      const preds = await prologInterface.query("list_module_predicates(knowledge_base)");
      const text = formatPrologList(preds);

      return {
        uri: "prolog://knowledge_base/predicates",
        name: "Knowledge Base Predicates",
        description: "List predicates defined in the knowledge_base module",
        mimeType: "text/plain",
        text,
      };
    },
  },

  /**
   * Export current knowledge base as Prolog clauses
   */
  "knowledge-base-dump": {
    uri: "prolog://knowledge_base/dump",
    name: "Knowledge Base Dump",
    description: "Export current knowledge base as Prolog clauses",
    mimeType: "text/prolog",
    handler: async () => {
      await prologInterface.start();
      const dump = await prologInterface.query("dump_knowledge_base");

      return {
        uri: "prolog://knowledge_base/dump",
        name: "Knowledge Base Dump",
        description: "Export current knowledge base as Prolog clauses",
        mimeType: "text/prolog",
        text: dump,
      };
    },
  },

  /**
   * Usage guidelines and tips for this server
   */
  "help": {
    uri: "reference://help",
    name: "Help",
    description: "Usage guidelines and tips for this server",
    mimeType: "text/plain",
    handler: async () => {
      const res = await tools.help.handler({});
      const text = Array.isArray(res.content) && res.content[0]?.text
        ? res.content[0].text
        : "Help unavailable";

      return {
        uri: "reference://help",
        name: "Help",
        description: "Usage guidelines and tips for this server",
        mimeType: "text/plain",
        text,
      };
    },
  },

  /**
   * License text for this software
   */
  "license": {
    uri: "reference://license",
    name: "License",
    description: "License text for this software",
    mimeType: "text/plain",
    handler: async () => {
      const res = await tools.license.handler({} as any);
      const text = Array.isArray(res.content) && res.content[0]?.text
        ? res.content[0].text
        : "License unavailable";

      return {
        uri: "reference://license",
        name: "License",
        description: "License text for this software",
        mimeType: "text/plain",
        text,
      };
    },
  },

  /**
   * Official swipl-mcp-server logo (SVG)
   */
  "logo": {
    uri: "reference://logo",
    name: "Server Logo",
    description: "Official swipl-mcp-server logo (SVG)",
    mimeType: "image/svg+xml",
    handler: async () => {
      const logoPath = findNearestFile("images/logo.svg");

      if (!logoPath) {
        const fallback = "Logo unavailable";
        return {
          uri: "reference://logo",
          name: "Server Logo",
          description: "Official swipl-mcp-server logo (SVG)",
          mimeType: "image/svg+xml",
          text: fallback,
        };
      }

      const svg = await fs.readFile(logoPath, "utf8");
      return {
        uri: "reference://logo",
        name: "Server Logo",
        description: "Official swipl-mcp-server logo (SVG)",
        mimeType: "image/svg+xml",
        text: svg,
      };
    },
  },

  /**
   * Machine-readable summary of tools, modes, environment, and safety features
   */
  "capabilities": {
    uri: "reference://capabilities",
    name: "Capabilities",
    description: "Machine-readable summary of tools, modes, env, and safety",
    mimeType: "application/json",
    handler: async () => {
      const caps = getCapabilitiesSummary();
      const json = JSON.stringify(caps, null, 2);

      return {
        uri: "reference://capabilities",
        name: "Capabilities",
        description: "Machine-readable summary of tools, modes, env, and safety",
        mimeType: "application/json",
        text: json,
      };
    },
  },
};
