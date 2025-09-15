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
        "troubleshooting",
      ])
      .optional()
      .describe(
        "Optional topic to focus help on (overview, standard_mode, engine_mode, safety, security, examples, prompts, troubleshooting)",
      ),
  },
  dbLoad: {
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
  dbDump: {},
  symbolsList: {},
  // Prompt schemas
  prologInitExpert: {},
  prologQuickReference: {},
  prologAnalyzeKb: {},
  prologExpertReasoning: {
    task: z.string().optional().describe("The reasoning task to solve using Prolog"),
  },
  prologKbBuilder: {
    domain: z.string().optional().describe("The domain to model (e.g., family relationships, expert system, planning)"),
  },
  prologQueryOptimizer: {
    query: z.string().optional().describe("The Prolog query to analyze and optimize"),
  },
  
  dbAssert: {
    fact: z
      .string()
      .describe(
        "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
      ),
  },
  dbRetract: {
    fact: z
      .string()
      .describe(
        "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
      ),
  },
  dbAssertMany: {
    facts: z.array(z.string()).describe("List of Prolog clauses to assert"),
  },
  dbRetractMany: {
    facts: z.array(z.string()).describe("List of Prolog clauses to retract"),
  },
  dbRetractAll: {},
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
          "troubleshooting",
        ],
        description:
          "Optional topic to focus help on (overview, standard_mode, engine_mode, safety, security, examples, prompts, troubleshooting)",
      },
    },
  },
  dbLoad: {
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
  dbDump: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  symbolsList: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  // Prompt JSON schemas
  prologInitExpert: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  prologQuickReference: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  prologAnalyzeKb: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
  prologExpertReasoning: {
    type: "object",
    additionalProperties: false,
    properties: {
      task: {
        type: "string",
        description: "The reasoning task to solve using Prolog",
      },
    },
  },
  prologKbBuilder: {
    type: "object",
    additionalProperties: false,
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
    properties: {
      query: {
        type: "string",
        description: "The Prolog query to analyze and optimize",
      },
    },
  },
  
  dbAssert: {
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
  dbRetract: {
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
  dbAssertMany: {
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
  dbRetractMany: {
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
  dbRetractAll: {
    type: "object",
    additionalProperties: false,
    properties: {},
  },
} as const;
