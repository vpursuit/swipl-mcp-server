import { z } from "zod";
import { zodToJsonSchema } from "zod-to-json-schema";

/**
 * Zod schemas for Prolog plugin tools and prompts
 *
 * NOTE: These are now defined as ZodRawShape (plain objects with Zod schemas as values)
 * to match MCP SDK expectations. This provides better type safety and eliminates the need
 * for .shape extraction during registration.
 */

// Tool schemas (raw shapes for direct use with MCP SDK)
export const capabilitiesSchema = {} as const;

export const querySchema = {
  operation: z
    .enum(["start", "next", "close"])
    .describe(
      "Query operation: 'start' begins new query, 'next' gets next solution, 'close' ends query",
    ),
  query: z
    .string()
    .min(1)
    .optional()
    .describe(
      "Prolog query string (required for 'start' operation)",
    ),
  use_engine: z
    .boolean()
    .optional()
    .default(false)
    .describe(
      "Use SWI-Prolog engine mode for true backtracking (default: false uses call_nth/2 pagination). Required for constraint solving with CLP(FD)",
    ),
} as const;

export const clausesSchema = {
  operation: z
    .enum(["assert", "retract"])
    .describe(
      "Clause operation: 'assert' adds facts/rules to KB with source preservation, 'retract' removes matching facts/rules. Use workspace tool's 'reset' operation to clear entire workspace.",
    ),
  clauses: z
    .union([z.string(), z.array(z.string())])
    .optional()
    .describe(
      "Single Prolog clause or array of clauses. Required for 'assert' and 'retract' operations. Accepts facts: 'parent(john, mary)' or rules: 'ancestor(X,Y) :- parent(X,Y)'. Periods optional. Source text preserved with original variable names.",
    ),
} as const;

export const filesSchema = {
  operation: z
    .enum(["import", "unimport", "list"])
    .describe(
      "File operation: 'import' loads .pl file with provenance tracking, 'unimport' removes file's clauses, 'list' shows imported files",
    ),
  filename: z
    .string()
    .optional()
    .describe(
      "Path to Prolog file (required for 'import' and 'unimport' operations). Must be within configured roots.",
    ),
} as const;

export const workspaceSchema = {
  operation: z
    .enum(["snapshot", "reset", "list_symbols"])
    .describe(
      "Workspace operation: 'snapshot' gets original source text with preserved formatting, 'reset' performs FULL workspace reset (removes all facts/rules, clears source storage, clears file import history), 'list_symbols' lists all user-defined predicates",
    ),
} as const;

export const explainErrorSchema = {
  error: z.object({
    kind: z.string().describe("Error kind/type (e.g., 'instantiation_error', 'syntax_error')"),
    message: z.string().describe("Error message"),
    details: z.object({
      predicate: z.string().optional(),
      file: z.string().optional(),
      operation: z.string().optional(),
      goal: z.string().optional(),
      raw: z.string().optional(),
      timeoutMs: z.number().optional(),
    }).optional().describe("Additional error details"),
  }).describe("Structured error object from a Prolog tool"),
  query: z.string().optional().describe("The query that caused the error"),
  include_kb: z.boolean().optional().default(true).describe("Include current knowledge base state for context (default: true)"),
} as const;

// Output schemas for structured tool responses
export const capabilitiesOutputSchema = {
  server: z.object({
    name: z.string(),
    version: z.string(),
  }),
  branding: z.object({
    logo: z.object({
      uri: z.string(),
      format: z.string(),
      description: z.string(),
    }),
  }),
  modes: z.array(z.string()),
  predicates: z.object({
    standard_prolog: z.string(),
    clpfd_available: z.boolean(),
    clpfd_note: z.string(),
  }),
  tools: z.object({
    core: z.array(z.string()),
    knowledge_base: z.array(z.string()),
    query: z.array(z.string()),
    analysis: z.array(z.string()),
  }),
  prompts: z.object({
    domain_examples: z.array(z.string()),
  }),
  security: z.object({
    module: z.string(),
    file_restrictions: z.object({
      allowed_directory: z.string(),
      blocked_directories: z.array(z.string()),
      validation: z.string(),
    }),
    consult: z.string(),
    model: z.string(),
    dangerous_predicate_blocking: z.object({
      detection: z.string(),
      blocked_predicates: z.array(z.string()),
      error_format: z.string(),
    }),
    sandbox_validation: z.string(),
    user_predicates: z.string(),
    safe_categories: z.array(z.string()),
    blocked_categories: z.array(z.string()),
  }),
  available_libraries: z.object({
    note: z.string(),
    safe_libraries: z.array(z.string()),
    description: z.string(),
  }),
  state_model: z.object({
    knowledge_base: z.string(),
    libraries: z.string(),
    queries: z.string(),
    best_practices: z.array(z.string()),
  }),
} as const;

export const symbolsListOutputSchema = {
  predicates: z.array(z.string()).describe("List of available predicates"),
  raw: z.string().describe("Raw Prolog list string"),
  processing_time_ms: z.number(),
  error: z.string().optional().describe("Error message if operation failed"),
} as const;

export const queryNextOutputSchema = {
  solution: z.string().nullable().describe("The solution or null if no more"),
  status: z.enum(["success", "done"]).describe("Iterator status: 'success' for solution available, 'done' when exhausted"),
  processing_time_ms: z.number(),
  error: z.string().optional().describe("Error message if operation failed"),
} as const;

export const explainErrorOutputSchema = {
  explanation: z.string().describe("What went wrong"),
  cause: z.string().describe("Root cause of the error"),
  suggestions: z.array(z.string()).describe("Concrete suggestions to fix the error"),
  examples: z.array(z.string()).optional().describe("Code examples"),
  tool_guidance: z.string().optional().describe("Guidance on correct tool usage"),
  processing_time_ms: z.number(),
  sampling_used: z.boolean().describe("Whether MCP sampling was used"),
} as const;

// Prompt schemas (raw shapes for direct use with MCP SDK)
export const genealogySchema = {
  family_info: z.string().describe("Family members and relationships to model. Provide names and relationships (e.g., 'John is Mary's father, Mary has two children: Alice and Bob')"),
} as const;

export const schedulingSchema = {
  tasks: z.string().describe("Tasks to schedule with durations and dependencies. Format: 'Task1 (duration X), Task2 (duration Y) depends on Task1, ...'"),
} as const;

export const puzzleSchema = {
  puzzle: z.string().optional().describe("The logic puzzle to solve with numbered clues. If not provided, suggest 3 interesting puzzles."),
} as const;

/**
 * Aggregate Zod schemas object (for backward compatibility with tests)
 * These are now the raw shapes themselves since that's what we export
 */
export const zodSchemas = {
  capabilities: capabilitiesSchema,
  query: querySchema,
  clauses: clausesSchema,
  files: filesSchema,
  workspace: workspaceSchema,
  explainError: explainErrorSchema,
  genealogy: genealogySchema,
  scheduling: schedulingSchema,
  puzzle: puzzleSchema,
} as const;

/**
 * JSON schemas for MCP tool registration
 * Converted from Zod schemas for backward compatibility
 * We wrap raw shapes in z.object() for JSON schema conversion
 */
export const jsonSchemas = {
  capabilities: zodToJsonSchema(z.object(capabilitiesSchema)),
  query: zodToJsonSchema(z.object(querySchema)),
  clauses: zodToJsonSchema(z.object(clausesSchema)),
  files: zodToJsonSchema(z.object(filesSchema)),
  workspace: zodToJsonSchema(z.object(workspaceSchema)),
  explainError: zodToJsonSchema(z.object(explainErrorSchema)),
  genealogy: zodToJsonSchema(z.object(genealogySchema)),
  scheduling: zodToJsonSchema(z.object(schedulingSchema)),
  puzzle: zodToJsonSchema(z.object(puzzleSchema)),
} as const;
