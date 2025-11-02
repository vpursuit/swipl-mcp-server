# MCP-Prolog Package Transformation Guide

## Current Status

### Completed ✅
- Package structure: `packages/mcp-prolog/src/`, `/prolog/`, `/test/`
- `package.json` with dependencies
- `tsconfig.json` configuration
- Source files copied:
  - `PrologInterface.ts` (from `src/`)
  - `prompts.ts` (from `src/`)
  - `meta.ts` (from `src/`)
  - `server.pl` (from `src/prolog_server.pl` → `prolog/server.pl`)
- `constants.ts` - Prolog-specific constants
- `schemas.ts` - All Zod schemas for tools and prompts

### Remaining ⏳
1. Fix ToolResponse type compatibility issue
2. Create `tools.ts` with ToolDefinitions format (15 tools, ~943 lines)
3. Create `resources.ts` with Prolog resources
4. Create `index.ts` plugin export
5. Build and test the package

---

## Issue 1: ToolResponse Type Mismatch

### Problem
The `ToolResponse` interface in `@vpursuit/mcp-core/src/types.ts` doesn't match what MCP SDK expects.

**Current mcp-core type:**
```typescript
export interface ToolResponse {
  success: boolean;
  data?: any;
  error?: string;
  metadata?: {
    processingTime?: number;
    [key: string]: any;
  };
}
```

**What MCP SDK actually expects:**
```typescript
// From src/tools.ts (current implementation)
type ToolResponse = {
  content: any[];                      // Array of content blocks
  structuredContent: Record<string, unknown>;
  isError?: boolean;
};
```

### Solution
Update `packages/mcp-core/src/types.ts` to use the correct MCP SDK format:

```typescript
/**
 * Response from a tool handler (MCP SDK format)
 */
export interface ToolResponse {
  content: Array<{
    type: "text" | "image" | "resource";
    text?: string;
    data?: string;
    mimeType?: string;
  }>;
  structuredContent?: Record<string, unknown>;
  isError?: boolean;
}
```

**Impact:** This is a breaking change for mcp-core, but it's necessary for MCP SDK compatibility.

---

## Task 2: Create tools.ts with ToolDefinitions

### File Structure

```typescript
// packages/mcp-prolog/src/tools.ts
import { readFile } from "fs/promises";
import path from "path";
import os from "os";
import type { ToolDefinitions, ToolResponse } from "@vpursuit/mcp-core";
import { PrologInterface } from "./PrologInterface.js";
import {
  MAX_FILENAME_LENGTH,
  MAX_QUERY_LENGTH,
  MAX_FACT_LENGTH,
  ALLOWED_DIR
} from "./constants.js";
import {
  helpSchema,
  licenseSchema,
  capabilitiesSchema,
  knowledgeBaseLoadSchema,
  queryStartSchema,
  queryNextSchema,
  queryCloseSchema,
  symbolsListSchema,
  knowledgeBaseAssertSchema,
  knowledgeBaseAssertManySchema,
  knowledgeBaseRetractSchema,
  knowledgeBaseRetractManySchema,
  knowledgeBaseClearSchema,
  queryStartEngineSchema,
  knowledgeBaseDumpSchema,
} from "./schemas.js";
import { resolvePackageVersion, findNearestFile } from "./meta.js";

// Validation helper from src/utils/validation.ts
function validateStringInput(
  paramName: string,
  value: any,
  maxLength: number
): { ok: boolean; error?: string; code?: string } {
  // ... copy from src/utils/validation.ts
}

// Path validation
function validateFilePath(filename: string): { allowed: boolean; error?: string } {
  try {
    const absolutePath = path.resolve(filename);
    const relativePath = path.relative(ALLOWED_DIR, absolutePath);
    const isWithinAllowed = !relativePath.startsWith('..') && !path.isAbsolute(relativePath);

    if (isWithinAllowed) {
      return { allowed: true };
    }

    return {
      allowed: false,
      error: `Security Error: Files can only be loaded from ${ALLOWED_DIR}`
    };
  } catch (_error) {
    return {
      allowed: false,
      error: `Security Error: Invalid file path`
    };
  }
}

// Response helpers from src/utils/response.ts
function createErrorResponse(
  message: string,
  startTime: number,
  additionalData: Record<string, unknown> = {}
): ToolResponse {
  return {
    content: [{ type: "text", text: message }],
    structuredContent: {
      error: message,
      processingTime: Date.now() - startTime,
      ...additionalData,
    },
    isError: true,
  };
}

function createSuccessResponse(
  message: string,
  startTime: number,
  additionalData: Record<string, unknown> = {}
): ToolResponse {
  return {
    content: [{ type: "text", text: message }],
    structuredContent: {
      success: true,
      processingTime: Date.now() - startTime,
      ...additionalData,
    },
  };
}

// Capabilities helper
function getCapabilitiesSummary() {
  // ... copy from src/tools.ts (lines ~254-263)
}

// Knowledge base helpers
async function knowledgeBaseAssertClauses(
  facts: string[],
  { ignoreStartErrors = false }: { ignoreStartErrors?: boolean } = {},
): Promise<{ outcomes: any[]; successCount: number; errorCount: number }> {
  // ... copy from src/tools.ts (lines ~51-99)
}

// Global Prolog interface instance
const prologInterface = PrologInterface.getInstance();

/**
 * Tool definitions for Prolog plugin
 * Exported as ToolDefinitions for use with mcp-core plugin loader
 */
export const tools: ToolDefinitions = {
  help: {
    description: "Get usage guidelines and tips for this server (optional topic)",
    inputSchema: helpSchema,
    handler: async ({ topic }: { topic?: string } = {}) => {
      // Copy implementation from src/tools.ts lines 107-225
      // ... sectionsData definition
      // ... includeOrder array
      // ... composition logic
    },
  },

  license: {
    description: "Get the license text for this software",
    inputSchema: licenseSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 228-251
    },
  },

  capabilities: {
    description: "Get a machine-readable summary of tools, modes, env, and safety",
    inputSchema: capabilitiesSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 254-263
    },
  },

  knowledge_base_load: {
    description: "Load a Prolog file into the knowledge base",
    inputSchema: knowledgeBaseLoadSchema,
    handler: async ({ filename }: { filename: string }) => {
      // Copy implementation from src/tools.ts lines 265-323
    },
  },

  query_start: {
    description: "Start a new Prolog query session (call_nth/2 mode)",
    inputSchema: queryStartSchema,
    handler: async ({ query }: { query: string }) => {
      // Copy implementation from src/tools.ts lines 325-349
    },
  },

  query_next: {
    description: "Get the next solution from the current query (unified for both modes)",
    inputSchema: queryNextSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 352-412
    },
  },

  query_close: {
    description: "Close the current query session (unified for both modes)",
    inputSchema: queryCloseSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 414-447
    },
  },

  symbols_list: {
    description: "List predicates available in the knowledge base",
    inputSchema: symbolsListSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 449-484
    },
  },

  knowledge_base_assert: {
    description: "Add a single clause (fact or rule) to the knowledge base",
    inputSchema: knowledgeBaseAssertSchema,
    handler: async ({ fact }: { fact: string }) => {
      // Copy implementation from src/tools.ts lines 486-546
    },
  },

  knowledge_base_assert_many: {
    description: "Add multiple clauses (facts or rules) to the knowledge base",
    inputSchema: knowledgeBaseAssertManySchema,
    handler: async ({ facts }: { facts: string[] }) => {
      // Copy implementation from src/tools.ts lines 548-604
    },
  },

  knowledge_base_retract: {
    description: "Remove a single clause (fact or rule) from the knowledge base",
    inputSchema: knowledgeBaseRetractSchema,
    handler: async ({ fact }: { fact: string }) => {
      // Copy implementation from src/tools.ts lines 606-648
    },
  },

  knowledge_base_retract_many: {
    description: "Remove multiple clauses (facts or rules) from the knowledge base",
    inputSchema: knowledgeBaseRetractManySchema,
    handler: async ({ facts }: { facts: string[] }) => {
      // Copy implementation from src/tools.ts lines 650-730
    },
  },

  knowledge_base_clear: {
    description: "Remove ALL user-defined facts and rules from the knowledge base",
    inputSchema: knowledgeBaseClearSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 732-769
    },
  },

  query_startEngine: {
    description: "Start a new Prolog query session using SWI-Prolog engines for true backtracking",
    inputSchema: queryStartEngineSchema,
    handler: async ({ query }: { query: string }) => {
      // Copy implementation from src/tools.ts lines 771-811
    },
  },

  knowledge_base_dump: {
    description: "Export current knowledge base as Prolog facts",
    inputSchema: knowledgeBaseDumpSchema,
    handler: async () => {
      // Copy implementation from src/tools.ts lines 813-839
    },
  },
};
```

### Implementation Steps

1. **Copy helper functions** from `src/tools.ts`:
   - `validateFilePath()` (lines 19-39)
   - `knowledgeBaseAssertClauses()` (lines 51-99)
   - Copy validation from `src/utils/validation.ts`
   - Copy response helpers from `src/utils/response.ts`

2. **Transform each tool handler** from this pattern:
   ```typescript
   // OLD (src/tools.ts)
   async help({ topic }: { topic?: string } = {}): Promise<ToolResponse> {
     // ... implementation
   }
   ```

   To this pattern:
   ```typescript
   // NEW (packages/mcp-prolog/src/tools.ts)
   help: {
     description: "Get usage guidelines...",
     inputSchema: helpSchema,
     handler: async ({ topic }: { topic?: string } = {}) => {
       // ... same implementation
     },
   },
   ```

3. **Update imports** to use:
   - `@vpursuit/mcp-core` types
   - Local schemas from `./schemas.js`
   - Local constants from `./constants.js`
   - Remove dependency on `src/utils/*` (copy utilities locally or import from mcp-core)

---

## Task 3: Create resources.ts

Extract Prolog resources from `src/index.ts` (lines ~176-303):

```typescript
// packages/mcp-prolog/src/resources.ts
import type { ResourceDefinitions } from "@vpursuit/mcp-core";
import { PrologInterface } from "./PrologInterface.js";
import { resolvePackageVersion, findPrologScript } from "./meta.js";
import { readFile } from "fs/promises";

const prologInterface = PrologInterface.getInstance();

/**
 * Resource definitions for Prolog plugin
 */
export const resources: ResourceDefinitions = {
  "capabilities": {
    uri: "reference://capabilities",
    name: "Server Capabilities",
    description: "Complete server capabilities, tools, modes, security model, and environment configuration",
    mimeType: "application/json",
    handler: async () => {
      // Copy from src/index.ts lines ~189-209
    },
  },

  "help": {
    uri: "reference://help",
    name: "Usage Guidelines",
    description: "Comprehensive usage guidelines, examples, and troubleshooting",
    mimeType: "text/plain",
    handler: async () => {
      // Copy from src/index.ts lines ~212-219
    },
  },

  "license": {
    uri: "reference://license",
    name: "Software License",
    description: "BSD-3-Clause license text for this software",
    mimeType: "text/plain",
    handler: async () => {
      // Copy from src/index.ts lines ~222-238
    },
  },

  "prolog-server-script": {
    uri: "prolog://server/script",
    name: "Prolog Server Script",
    description: "The Prolog server implementation (server.pl)",
    mimeType: "text/x-prolog",
    handler: async () => {
      // Copy from src/index.ts lines ~241-257
    },
  },

  "knowledge-base-predicates": {
    uri: "prolog://knowledge_base/predicates",
    name: "Knowledge Base Predicates",
    description: "List of all predicates currently defined in the knowledge base",
    mimeType: "application/json",
    handler: async () => {
      // Copy from src/index.ts lines ~260-277
    },
  },

  "knowledge-base-dump": {
    uri: "prolog://knowledge_base/dump",
    name: "Knowledge Base Export",
    description: "Complete export of current knowledge base in Prolog syntax",
    mimeType: "text/x-prolog",
    handler: async () => {
      // Copy from src/index.ts lines ~280-296
    },
  },

  "logo": {
    uri: "image://logo",
    name: "SWI-Prolog MCP Server Logo",
    description: "Official logo for the SWI-Prolog MCP Server",
    mimeType: "image/svg+xml",
    handler: async () => {
      // Copy from src/index.ts lines ~299-303
    },
  },
};
```

---

## Task 4: Create prompts.ts Export

The `prompts.ts` file already exists and has the correct structure. We just need to restructure it:

```typescript
// packages/mcp-prolog/src/prompts.ts (restructure existing)
import type { PromptDefinitions } from "@vpursuit/mcp-core";

/**
 * Prompt definitions for Prolog plugin
 */
export const prompts: PromptDefinitions = {
  prolog_init_expert: {
    name: "prolog_init_expert",
    description: "Set up expert context; optionally focus on a specific task",
    arguments: [
      {
        name: "task",
        description: "Optional task to focus expert setup and reasoning",
        required: false
      },
    ],
    handler: async (args: Record<string, string>) => {
      // Convert existing prologPrompts.initExpert.messages() to handler format
      const messages = prologPrompts.initExpert.messages(args);
      return {
        messages: messages.map(msg => ({
          role: msg.role,
          content: msg.content,
        })),
      };
    },
  },

  // ... convert all other prompts similarly:
  // - prolog_quick_reference
  // - prolog_analyze_knowledge_base
  // - prolog_knowledge_base_builder
  // - prolog_query_optimizer
};
```

---

## Task 5: Create Plugin index.ts

```typescript
// packages/mcp-prolog/src/index.ts
import type { Plugin } from "@vpursuit/mcp-core";
import { PrologInterface } from "./PrologInterface.js";
import { tools } from "./tools.js";
import { resources } from "./resources.js";
import { prompts } from "./prompts.js";

// Re-export types and utilities
export { PrologInterface } from "./PrologInterface.js";
export * from "./constants.js";

/**
 * SWI-Prolog MCP Plugin
 *
 * Provides comprehensive Prolog integration:
 * - 15 tools for knowledge base and query management
 * - 7 resources for introspection and documentation
 * - 5 expert prompts for guided Prolog development
 */
export const plugin: Plugin = {
  name: "mcp-prolog",
  version: "2.1.0",
  description: "SWI-Prolog integration for Model Context Protocol servers",

  tools,
  resources,
  prompts,

  async onInit() {
    // Start Prolog interface
    const prolog = PrologInterface.getInstance();
    await prolog.start();
    console.log("[mcp-prolog] ✓ Prolog interface started");
  },

  async onShutdown() {
    // Stop Prolog interface
    const prolog = PrologInterface.getInstance();
    await prolog.stop();
    console.log("[mcp-prolog] Prolog interface stopped");
  },
};
```

---

## Task 6: Build and Test

### Build Steps

```bash
# Install dependencies
cd packages/mcp-prolog
npm install

# Build
npm run build

# Verify build output
ls -la build/
```

### Expected Build Output

```
build/
├── index.js
├── index.d.ts
├── PrologInterface.js
├── PrologInterface.d.ts
├── tools.js
├── tools.d.ts
├── resources.js
├── resources.d.ts
├── prompts.js
├── prompts.d.ts
├── schemas.js
├── schemas.d.ts
├── constants.js
├── constants.d.ts
├── meta.js
└── meta.d.ts
```

### Test Integration

1. **Import test** in a Node REPL:
   ```javascript
   import { plugin } from '@vpursuit/mcp-prolog';
   console.log(plugin.name); // "mcp-prolog"
   console.log(Object.keys(plugin.tools).length); // 15
   console.log(Object.keys(plugin.resources).length); // 7
   console.log(Object.keys(plugin.prompts).length); // 5
   ```

2. **Type checking**:
   ```bash
   npm run build -- --noEmit
   ```

---

## File Locations Reference

### Source Files to Copy From
- `src/tools.ts` → Copy to `packages/mcp-prolog/src/tools.ts` (restructured)
- `src/index.ts` (lines 176-303) → Extract to `packages/mcp-prolog/src/resources.ts`
- `src/prompts.ts` → Already copied, needs restructuring
- `src/PrologInterface.ts` → Already copied ✅
- `src/meta.ts` → Already copied ✅
- `src/prolog_server.pl` → Already copied to `prolog/server.pl` ✅
- `src/utils/validation.ts` → Copy helpers to `tools.ts` or add to mcp-core
- `src/utils/response.ts` → Copy helpers to `tools.ts` or add to mcp-core

### New Files to Create
- `packages/mcp-prolog/src/tools.ts` ⏳
- `packages/mcp-prolog/src/resources.ts` ⏳
- `packages/mcp-prolog/src/index.ts` ⏳

---

## Dependencies Summary

### Already in package.json ✅
- `zod`: Schema validation
- `@modelcontextprotocol/sdk`: MCP SDK (peer)
- `@vpursuit/mcp-core`: Plugin system (peer)

### Additional Imports Needed
None - all dependencies are satisfied.

---

## Testing Checklist

After completing all files:

- [ ] Package builds without errors: `npm run build`
- [ ] TypeScript type checking passes: `tsc --noEmit`
- [ ] Can import plugin: `import { plugin } from '@vpursuit/mcp-prolog'`
- [ ] Plugin has 15 tools
- [ ] Plugin has 7 resources
- [ ] Plugin has 5 prompts
- [ ] PrologInterface.getInstance() works
- [ ] All tool handlers have correct signatures
- [ ] All resource handlers have correct signatures
- [ ] All prompt handlers have correct signatures

---

## Next Steps After Phase 4

Once mcp-prolog package is complete:

1. **Phase 5**: Create orchestrator package (@vpursuit/swipl-mcp-server)
   - Minimal index.ts that loads plugins
   - Uses StdioServerTransport
   - Binary entry point

2. **Phase 6**: Update build & CI/CD
   - Workspace build scripts
   - GitHub Actions for multi-package publishing

3. **Phase 7**: Testing & documentation
   - Integration tests
   - README files for each package
   - ARCHITECTURE.md

---

**Ready to proceed when you review this guide!**
