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
        "troubleshooting",
      ])
      .optional()
      .describe(
        "Optional topic to focus help on (overview, standard_mode, engine_mode, safety, security, examples, troubleshooting)",
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
  dbAssert: {
    fact: z.union([
      z
        .string()
        .describe(
          "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
        ),
      z.array(z.string()).describe("List of Prolog clauses to assert"),
    ]),
  },
  dbRetract: {
    fact: z.union([
      z
        .string()
        .describe(
          "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
        ),
      z.array(z.string()).describe("List of Prolog clauses to retract"),
    ]),
  },
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
          "Optional topic to focus help on (overview, standard_mode, engine_mode, safety, security, examples, troubleshooting)",
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
  dbAssert: {
    type: "object",
    additionalProperties: false,
    required: ["fact"],
    properties: {
      fact: {
        anyOf: [
          {
            type: "string",
            description:
              "Single Prolog clause to assert (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
          },
          {
            type: "array",
            items: { type: "string" },
            description: "List of Prolog clauses to assert",
          },
        ],
      },
    },
  },
  dbRetract: {
    type: "object",
    additionalProperties: false,
    required: ["fact"],
    properties: {
      fact: {
        anyOf: [
          {
            type: "string",
            description:
              "Single Prolog clause to retract (e.g., 'parent(john, mary)' or 'grandparent(X,Z) :- parent(X,Y), parent(Y,Z)')",
          },
          {
            type: "array",
            items: { type: "string" },
            description: "List of Prolog clauses to retract",
          },
        ],
      },
    },
  },
} as const;
