import { z } from "zod";

// Zod input schemas (used for local validation in tests)
export const zodSchemas = {
  help: {
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
  },
  knowledgeBaseLoad: {
    filename: z.string().describe("Path to the Prolog file to load"),
  },
  queryStart: {
    query: z.string().min(1).describe("Prolog query to start"),
  },
  queryNext: {},
  queryClose: {},
  queryStartEngine: {
    query: z.string().min(1).describe("Prolog query to start with engine-based iteration"),
  },
  knowledgeBaseDump: {},
  symbolsList: {},
  rootsList: {},
  // Prompt schemas
  prologInitExpert: {
    task: z.string().optional().describe("Optional task to focus expert setup and reasoning"),
  },
  prologQuickReference: {},
  prologAnalyzeKnowledgeBase: {},
  // prologExpertReasoning merged into prologInitExpert
  prologKnowledgeBaseBuilder: {
    domain: z.string().describe("The domain to model (e.g., family relationships, expert system, planning)"),
  },
  prologQueryOptimizer: {
    query: z.string().describe("The Prolog query to analyze and optimize"),
  },
  
  knowledgeBaseAssert: {
    fact: z
      .string()
      .describe(
        "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
      ),
  },
  knowledgeBaseRetract: {
    fact: z
      .string()
      .describe(
        "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
      ),
  },
  knowledgeBaseAssertMany: {
    facts: z.array(z.string()).describe("List of Prolog clauses to assert"),
  },
  knowledgeBaseRetractMany: {
    facts: z.array(z.string()).describe("List of Prolog clauses to retract"),
  },
  knowledgeBaseClear: {},
} as const;

// JSON Schemas for MCP tool registration (must be plain, serializable objects)
export const jsonSchemas = {
  help: {
    type: "object",
    additionalProperties: false,
    properties: {
      topic: {
        type: "string",
        enum: [
          "overview",
          "standard_mode",
          "engine_mode",
          "safety",
          "security",
          "examples",
          "prompts",
          "roots",
          "troubleshooting",
        ],
        description:
          "Optional topic to focus help on (overview, standard_mode, engine_mode, safety, security, examples, prompts, roots, troubleshooting)",
      },
    },
  },
  knowledgeBaseLoad: {
    type: "object",
    additionalProperties: false,
    required: ["filename"],
    properties: {
      filename: {
        type: "string",
        description: "Path to the Prolog file to load",
      },
    },
  },
  queryStart: {
    type: "object",
    additionalProperties: false,
    required: ["query"],
    properties: {
      query: { type: "string", minLength: 1, description: "Prolog query to start" },
    },
  },
  queryNext: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  queryClose: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  queryStartEngine: {
    type: "object",
    additionalProperties: false,
    required: ["query"],
    properties: {
      query: {
        type: "string",
        minLength: 1,
        description: "Prolog query to start with engine-based iteration",
      },
    },
  },
  knowledgeBaseDump: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  symbolsList: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  rootsList: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  // Prompt JSON schemas
  prologInitExpert: {
    type: "object",
    additionalProperties: false,
    properties: {
      task: { type: "string", description: "Optional task to focus expert setup and reasoning" },
    },
  },
  prologQuickReference: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  prologAnalyzeKnowledgeBase: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  // prologExpertReasoning merged into prologInitExpert
  prologKnowledgeBaseBuilder: {
    type: "object",
    additionalProperties: false,
    required: ["domain"],
    properties: {
      domain: {
        type: "string",
        description: "The domain to model (e.g., family relationships, expert system, planning)",
      },
    },
  },
  prologQueryOptimizer: {
    type: "object",
    additionalProperties: false,
    required: ["query"],
    properties: {
      query: {
        type: "string",
        description: "The Prolog query to analyze and optimize",
      },
    },
  },
  
  knowledgeBaseAssert: {
    type: "object",
    additionalProperties: false,
    required: ["fact"],
    properties: {
      fact: {
        type: "string",
        description:
          "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
      },
    },
  },
  knowledgeBaseRetract: {
    type: "object",
    additionalProperties: false,
    required: ["fact"],
    properties: {
      fact: {
        type: "string",
        description:
          "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
      },
    },
  },
  knowledgeBaseAssertMany: {
    type: "object",
    additionalProperties: false,
    required: ["facts"],
    properties: {
      facts: {
        type: "array",
        items: { type: "string" },
        description: "List of Prolog clauses to assert",
      },
    },
  },
  knowledgeBaseRetractMany: {
    type: "object",
    additionalProperties: false,
    required: ["facts"],
    properties: {
      facts: {
        type: "array",
        items: { type: "string" },
        description: "List of Prolog clauses to retract",
      },
    },
  },
  knowledgeBaseClear: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
} as const;
