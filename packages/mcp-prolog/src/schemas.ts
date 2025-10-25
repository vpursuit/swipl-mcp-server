import { z } from "zod";
import { zodToJsonSchema } from "zod-to-json-schema";

/**
 * Zod schemas for Prolog plugin tools and prompts
 * These will be converted to JSON schemas by the plugin loader using zodToJsonSchema
 */

// Tool schemas
export const helpSchema = z.object({
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
});

export const knowledgeBaseLoadSchema = z.object({
  filename: z.string().describe("Path to the Prolog file to load"),
});

export const queryStartSchema = z.object({
  query: z.string().min(1).describe("Prolog query to start"),
});

export const queryNextSchema = z.object({});

export const queryCloseSchema = z.object({});

export const queryStartEngineSchema = z.object({
  query: z.string().min(1).describe("Prolog query to start with engine-based iteration"),
});

export const knowledgeBaseDumpSchema = z.object({});

export const symbolsListSchema = z.object({});

export const knowledgeBaseAssertSchema = z.object({
  fact: z
    .string()
    .describe(
      "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
    ),
});

export const knowledgeBaseRetractSchema = z.object({
  fact: z
    .string()
    .describe(
      "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
    ),
});

export const knowledgeBaseAssertManySchema = z.object({
  facts: z.array(z.string()).describe("List of Prolog clauses to assert"),
});

export const knowledgeBaseRetractManySchema = z.object({
  facts: z.array(z.string()).describe("List of Prolog clauses to retract"),
});

export const knowledgeBaseClearSchema = z.object({});

export const capabilitiesSchema = z.object({});

export const licenseSchema = z.object({});

// Prompt schemas
export const prologInitExpertSchema = z.object({
  task: z.string().optional().describe("Optional task to focus expert setup and reasoning"),
});

export const prologQuickReferenceSchema = z.object({});

export const prologAnalyzeKnowledgeBaseSchema = z.object({});

export const prologKnowledgeBaseBuilderSchema = z.object({
  domain: z.string().describe("The domain to model (e.g., family relationships, expert system, planning)"),
});

export const prologQueryOptimizerSchema = z.object({
  query: z.string().describe("The Prolog query to analyze and optimize"),
});

/**
 * Aggregate Zod schemas object (for backward compatibility with tests)
 * These are plain shape objects (not wrapped in z.object()) for use in tests
 */
export const zodSchemas = {
  help: helpSchema.shape,
  knowledgeBaseLoad: knowledgeBaseLoadSchema.shape,
  queryStart: queryStartSchema.shape,
  queryNext: queryNextSchema.shape,
  queryClose: queryCloseSchema.shape,
  queryStartEngine: queryStartEngineSchema.shape,
  knowledgeBaseDump: knowledgeBaseDumpSchema.shape,
  symbolsList: symbolsListSchema.shape,
  knowledgeBaseAssert: knowledgeBaseAssertSchema.shape,
  knowledgeBaseRetract: knowledgeBaseRetractSchema.shape,
  knowledgeBaseAssertMany: knowledgeBaseAssertManySchema.shape,
  knowledgeBaseRetractMany: knowledgeBaseRetractManySchema.shape,
  knowledgeBaseClear: knowledgeBaseClearSchema.shape,
  capabilities: capabilitiesSchema.shape,
  license: licenseSchema.shape,
  prologInitExpert: prologInitExpertSchema.shape,
  prologQuickReference: prologQuickReferenceSchema.shape,
  prologAnalyzeKnowledgeBase: prologAnalyzeKnowledgeBaseSchema.shape,
  prologKnowledgeBaseBuilder: prologKnowledgeBaseBuilderSchema.shape,
  prologQueryOptimizer: prologQueryOptimizerSchema.shape,
} as const;

/**
 * JSON schemas for MCP tool registration
 * Converted from Zod schemas for backward compatibility
 */
export const jsonSchemas = {
  help: zodToJsonSchema(helpSchema),
  knowledgeBaseLoad: zodToJsonSchema(knowledgeBaseLoadSchema),
  queryStart: zodToJsonSchema(queryStartSchema),
  queryNext: zodToJsonSchema(queryNextSchema),
  queryClose: zodToJsonSchema(queryCloseSchema),
  queryStartEngine: zodToJsonSchema(queryStartEngineSchema),
  knowledgeBaseDump: zodToJsonSchema(knowledgeBaseDumpSchema),
  symbolsList: zodToJsonSchema(symbolsListSchema),
  knowledgeBaseAssert: zodToJsonSchema(knowledgeBaseAssertSchema),
  knowledgeBaseRetract: zodToJsonSchema(knowledgeBaseRetractSchema),
  knowledgeBaseAssertMany: zodToJsonSchema(knowledgeBaseAssertManySchema),
  knowledgeBaseRetractMany: zodToJsonSchema(knowledgeBaseRetractManySchema),
  knowledgeBaseClear: zodToJsonSchema(knowledgeBaseClearSchema),
  capabilities: zodToJsonSchema(capabilitiesSchema),
  license: zodToJsonSchema(licenseSchema),
  prologInitExpert: zodToJsonSchema(prologInitExpertSchema),
  prologQuickReference: zodToJsonSchema(prologQuickReferenceSchema),
  prologAnalyzeKnowledgeBase: zodToJsonSchema(prologAnalyzeKnowledgeBaseSchema),
  prologKnowledgeBaseBuilder: zodToJsonSchema(prologKnowledgeBaseBuilderSchema),
  prologQueryOptimizer: zodToJsonSchema(prologQueryOptimizerSchema),
} as const;
