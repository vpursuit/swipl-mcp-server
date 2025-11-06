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
export const helpSchema = {
  topic: z
    .enum([
      "overview",
      "standard_mode",
      "engine_mode",
      "safety",
      "security",
      "examples",
      "prompts",
      "roots",
      "troubleshooting",
    ])
    .optional()
    .describe(
      "Optional topic to focus help on (overview, standard_mode, engine_mode, safety, security, examples, prompts, roots, troubleshooting)",
    ),
} as const;

export const knowledgeBaseLoadSchema = {
  filename: z.string().describe("Path to the Prolog file to load"),
} as const;

export const queryStartSchema = {
  query: z.string().min(1).describe("Prolog query to start"),
} as const;

export const queryNextSchema = {} as const;

export const queryCloseSchema = {} as const;

export const queryStartEngineSchema = {
  query: z.string().min(1).describe("Prolog query to start with engine-based iteration"),
} as const;

export const knowledgeBaseDumpSchema = {} as const;

export const symbolsListSchema = {} as const;

export const knowledgeBaseAssertSchema = {
  fact: z
    .string()
    .describe(
      "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
    ),
} as const;

export const knowledgeBaseRetractSchema = {
  fact: z
    .string()
    .describe(
      "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
    ),
} as const;

export const knowledgeBaseAssertManySchema = {
  facts: z.array(z.string()).describe("List of Prolog clauses to assert"),
} as const;

export const knowledgeBaseRetractManySchema = {
  facts: z.array(z.string()).describe("List of Prolog clauses to retract"),
} as const;

export const knowledgeBaseClearSchema = {} as const;

export const knowledgeBaseLoadLibrarySchema = {
  library: z
    .string()
    .describe(
      "Name of the safe library to load (e.g., 'clpfd', 'lists', 'apply'). Only sandbox-approved libraries are allowed.",
    ),
} as const;

export const capabilitiesSchema = {} as const;

export const licenseSchema = {} as const;

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
    symbols: z.array(z.string()),
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
} as const;

export const symbolsListOutputSchema = {
  predicates: z.array(z.string()).describe("List of available predicates"),
  raw: z.string().describe("Raw Prolog list string"),
  processing_time_ms: z.number(),
} as const;

export const queryNextOutputSchema = {
  solution: z.string().nullable().describe("The solution or null if no more"),
  status: z.enum(["success", "done"]).describe("Iterator status: 'success' for solution available, 'done' when exhausted"),
  processing_time_ms: z.number(),
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

export const grammarSchema = {
  sentence: z.string().optional().describe("Sentence to parse (e.g., 'the cat sat on the mat'). If not provided, use a default example."),
} as const;

/**
 * Aggregate Zod schemas object (for backward compatibility with tests)
 * These are now the raw shapes themselves since that's what we export
 */
export const zodSchemas = {
  help: helpSchema,
  knowledgeBaseLoad: knowledgeBaseLoadSchema,
  queryStart: queryStartSchema,
  queryNext: queryNextSchema,
  queryClose: queryCloseSchema,
  queryStartEngine: queryStartEngineSchema,
  knowledgeBaseDump: knowledgeBaseDumpSchema,
  symbolsList: symbolsListSchema,
  knowledgeBaseAssert: knowledgeBaseAssertSchema,
  knowledgeBaseRetract: knowledgeBaseRetractSchema,
  knowledgeBaseAssertMany: knowledgeBaseAssertManySchema,
  knowledgeBaseRetractMany: knowledgeBaseRetractManySchema,
  knowledgeBaseClear: knowledgeBaseClearSchema,
  knowledgeBaseLoadLibrary: knowledgeBaseLoadLibrarySchema,
  capabilities: capabilitiesSchema,
  license: licenseSchema,
  genealogy: genealogySchema,
  scheduling: schedulingSchema,
  puzzle: puzzleSchema,
  grammar: grammarSchema,
} as const;

/**
 * JSON schemas for MCP tool registration
 * Converted from Zod schemas for backward compatibility
 * We wrap raw shapes in z.object() for JSON schema conversion
 */
export const jsonSchemas = {
  help: zodToJsonSchema(z.object(helpSchema)),
  knowledgeBaseLoad: zodToJsonSchema(z.object(knowledgeBaseLoadSchema)),
  queryStart: zodToJsonSchema(z.object(queryStartSchema)),
  queryNext: zodToJsonSchema(z.object(queryNextSchema)),
  queryClose: zodToJsonSchema(z.object(queryCloseSchema)),
  queryStartEngine: zodToJsonSchema(z.object(queryStartEngineSchema)),
  knowledgeBaseDump: zodToJsonSchema(z.object(knowledgeBaseDumpSchema)),
  symbolsList: zodToJsonSchema(z.object(symbolsListSchema)),
  knowledgeBaseAssert: zodToJsonSchema(z.object(knowledgeBaseAssertSchema)),
  knowledgeBaseRetract: zodToJsonSchema(z.object(knowledgeBaseRetractSchema)),
  knowledgeBaseAssertMany: zodToJsonSchema(z.object(knowledgeBaseAssertManySchema)),
  knowledgeBaseRetractMany: zodToJsonSchema(z.object(knowledgeBaseRetractManySchema)),
  knowledgeBaseClear: zodToJsonSchema(z.object(knowledgeBaseClearSchema)),
  knowledgeBaseLoadLibrary: zodToJsonSchema(z.object(knowledgeBaseLoadLibrarySchema)),
  capabilities: zodToJsonSchema(z.object(capabilitiesSchema)),
  license: zodToJsonSchema(z.object(licenseSchema)),
  genealogy: zodToJsonSchema(z.object(genealogySchema)),
  scheduling: zodToJsonSchema(z.object(schedulingSchema)),
  puzzle: zodToJsonSchema(z.object(puzzleSchema)),
  grammar: zodToJsonSchema(z.object(grammarSchema)),
} as const;
