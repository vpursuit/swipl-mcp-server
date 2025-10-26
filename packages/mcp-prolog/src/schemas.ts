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
  version: z.string().describe("Server version"),
  branding: z.object({
    name: z.string(),
    logo: z.object({
      svg_path: z.string(),
      emoji: z.string(),
    }),
  }).optional(),
  predicates: z.object({
    total: z.number(),
    categories: z.array(z.string()),
  }).optional(),
  tools: z.record(z.string(), z.string()).optional(),
  prompts: z.record(z.string(), z.string()).optional(),
  security: z.object({
    file_restrictions: z.object({
      allowed_directory: z.string(),
      validation: z.string(),
    }).optional(),
    dangerous_predicate_blocking: z.object({
      blocked_predicates: z.array(z.string()),
      validation_method: z.string(),
    }).optional(),
    allowed_categories: z.array(z.string()).optional(),
    blocked_categories: z.array(z.string()).optional(),
  }).optional(),
} as const;

export const symbolsListOutputSchema = {
  predicates: z.array(z.string()).describe("List of available predicates"),
  raw: z.string().describe("Raw Prolog list string"),
  processing_time_ms: z.number(),
} as const;

export const queryNextOutputSchema = {
  solution: z.string().nullable().describe("The solution or null if no more"),
  more_solutions: z.boolean().describe("Whether more solutions are available"),
  processing_time_ms: z.number(),
} as const;

// Prompt schemas (raw shapes for direct use with MCP SDK)
export const prologInitExpertSchema = {
  task: z.string().optional().describe("Optional task to focus expert setup and reasoning"),
} as const;

export const prologQuickReferenceSchema = {} as const;

export const prologAnalyzeKnowledgeBaseSchema = {} as const;

export const prologKnowledgeBaseBuilderSchema = {
  domain: z.string().describe("The domain to model (e.g., family relationships, expert system, planning)"),
} as const;

export const prologQueryOptimizerSchema = {
  query: z.string().describe("The Prolog query to analyze and optimize"),
} as const;

export const prologLogicPuzzleSolverSchema = {
  puzzle: z.string().optional().describe("The logic puzzle to solve (with numbered clues). If empty, agent chooses an interesting puzzle."),
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
  prologInitExpert: prologInitExpertSchema,
  prologQuickReference: prologQuickReferenceSchema,
  prologAnalyzeKnowledgeBase: prologAnalyzeKnowledgeBaseSchema,
  prologKnowledgeBaseBuilder: prologKnowledgeBaseBuilderSchema,
  prologQueryOptimizer: prologQueryOptimizerSchema,
  prologLogicPuzzleSolver: prologLogicPuzzleSolverSchema,
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
  prologInitExpert: zodToJsonSchema(z.object(prologInitExpertSchema)),
  prologQuickReference: zodToJsonSchema(z.object(prologQuickReferenceSchema)),
  prologAnalyzeKnowledgeBase: zodToJsonSchema(z.object(prologAnalyzeKnowledgeBaseSchema)),
  prologKnowledgeBaseBuilder: zodToJsonSchema(z.object(prologKnowledgeBaseBuilderSchema)),
  prologQueryOptimizer: zodToJsonSchema(z.object(prologQueryOptimizerSchema)),
  prologLogicPuzzleSolver: zodToJsonSchema(z.object(prologLogicPuzzleSolverSchema)),
} as const;
