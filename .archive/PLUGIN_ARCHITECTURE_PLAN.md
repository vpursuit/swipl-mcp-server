# Plugin Architecture Plan for SWI-Prolog MCP Server

## Executive Summary

**Goal**: Restructure swipl-mcp-server into a plugin-based monorepo with export-based composition.

**Key Decisions** (from user input):
- ✅ Simple export-based composition (no complex registry/auto-discovery)
- ✅ Prolog functionality becomes a plugin (@vpursuit/mcp-prolog)
- ✅ Pause previous monorepo migration, design plugin architecture first
- ✅ Internal use only (no third-party plugin complexity for now)

**Architecture Philosophy**: "Lego bricks" - modular packages that can be composed together, where even the Prolog core is just another plugin.

---

## Research Findings

### FastMCP 2.0 (Python)
**Supports**:
- Server composition via `mcp.mount()` (live-linking)
- Server import via `mcp.import_server()` (static copying)
- Proxy pattern via `FastMCP.as_proxy()`
- Modular teams defining tools independently

**Limitations**:
- No npm-style package discovery
- Manual wiring required
- Python-only (not applicable to TypeScript)

### @modelcontextprotocol/sdk (TypeScript)
**Supports**:
- Registration APIs: `server.registerTool()`, `server.registerResource()`, `server.registerPrompt()`
- Composable middleware (v1.17.4+)
- Modular, self-contained tools

**Limitations**:
- No built-in plugin discovery
- No standard plugin interface
- Manual registration required

### Current swipl-mcp-server Codebase
**Excellent Foundation**:
- ✅ Declarative registration pattern (src/index.ts:35-174)
- ✅ Dynamic prompt loading with for-loops (src/index.ts:305-341)
- ✅ Handler separation (src/tools.ts:106-839)
- ✅ Schema centralization (src/schemas.ts)
- ✅ Clear interfaces (src/prompts.ts: PrologPrompt, PromptArgument)

**Key Pattern Already Exists**: Prompt registration uses a for-loop over prompts object - this is exactly the pattern needed for plugins!

---

## Final Package Structure

```
swipl-mcp-server/  (monorepo root)
├── packages/
│   ├── mcp-core/              (@vpursuit/mcp-core v1.0.0)
│   │   ├── src/
│   │   │   ├── index.ts       (Main exports)
│   │   │   ├── types.ts       (Plugin interface definitions)
│   │   │   ├── loader.ts      (loadPlugins function)
│   │   │   └── utils/         (Shared utilities)
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   ├── mcp-roots/             (@vpursuit/mcp-roots v1.0.0)
│   │   ├── src/
│   │   │   ├── index.ts       (exports: { plugin })
│   │   │   ├── RootsManager.ts
│   │   │   ├── tools.ts
│   │   │   ├── resources.ts
│   │   │   └── schemas.ts
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   ├── mcp-prolog/            (@vpursuit/mcp-prolog v2.1.0)
│   │   ├── src/
│   │   │   ├── index.ts       (exports: { plugin })
│   │   │   ├── PrologInterface.ts
│   │   │   ├── tools.ts
│   │   │   ├── resources.ts
│   │   │   ├── prompts.ts
│   │   │   └── schemas.ts
│   │   ├── package.json
│   │   └── tsconfig.json
│   │
│   └── swipl-mcp-server/      (@vpursuit/swipl-mcp-server v3.0.0)
│       ├── src/
│       │   └── index.ts       (Main orchestrator)
│       ├── package.json
│       └── tsconfig.json
│
├── package.json               (Workspace root - private)
├── tsconfig.base.json         (Shared TypeScript config)
├── vitest.workspace.ts        (Unified testing)
└── .gitignore                 (Updated for monorepo)
```

---

## Plugin Interface Design

### Core Types (packages/mcp-core/src/types.ts)

```typescript
import type { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import type { z } from 'zod';

/**
 * Simple plugin interface for internal use.
 * Each plugin exports tools, resources, and prompts.
 */
export interface Plugin {
  /** Plugin metadata */
  name: string;
  version: string;
  description: string;

  /** Feature definitions */
  tools?: ToolDefinitions;
  resources?: ResourceDefinitions;
  prompts?: PromptDefinitions;

  /** Optional init hook (receives server instance) */
  onInit?: (server: McpServer) => Promise<void> | void;
}

/** Tool definition with handler */
export interface ToolDefinition {
  description: string;
  inputSchema: z.ZodSchema;
  handler: (args: any) => Promise<ToolResponse>;
}

export type ToolDefinitions = Record<string, ToolDefinition>;

/** Resource definition with handler */
export interface ResourceDefinition {
  uri: string;
  title: string;
  description: string;
  mimeType: string;
  handler: (uri: URL) => Promise<ResourceResponse>;
}

export type ResourceDefinitions = Record<string, ResourceDefinition>;

/** Prompt definition with messages factory */
export interface PromptDefinition {
  title: string;
  description: string;
  argsSchema?: z.ZodSchema;
  messages: (args: Record<string, string>) => PromptMessage[];
}

export type PromptDefinitions = Record<string, PromptDefinition>;

/** Standard responses */
export interface ToolResponse {
  success: boolean;
  data?: any;
  error?: string;
  processingTime?: number;
}

export interface ResourceResponse {
  contents: Array<{
    uri: string;
    text?: string;
    blob?: string;
    mimeType?: string
  }>;
}

export interface PromptMessage {
  role: 'user' | 'assistant';
  content: { type: 'text'; text: string };
}
```

### Plugin Loader (packages/mcp-core/src/loader.ts)

```typescript
import type { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import type { Plugin } from './types.js';
import { zodToJsonSchema } from 'zod-to-json-schema';

/**
 * Load and register multiple plugins with an MCP server.
 * Simple iteration - no complex discovery.
 */
export async function loadPlugins(
  server: McpServer,
  plugins: Plugin[]
): Promise<void> {
  for (const plugin of plugins) {
    await loadPlugin(server, plugin);
  }
}

/**
 * Load a single plugin.
 */
async function loadPlugin(
  server: McpServer,
  plugin: Plugin
): Promise<void> {
  console.log(`Loading plugin: ${plugin.name} v${plugin.version}`);

  // Call init hook if provided
  if (plugin.onInit) {
    await plugin.onInit(server);
  }

  // Register tools
  if (plugin.tools) {
    for (const [name, tool] of Object.entries(plugin.tools)) {
      server.registerTool(
        name,
        {
          description: tool.description,
          inputSchema: zodToJsonSchema(tool.inputSchema) as any,
        },
        tool.handler as any
      );
    }
  }

  // Register resources
  if (plugin.resources) {
    for (const [name, resource] of Object.entries(plugin.resources)) {
      server.registerResource(
        name,
        resource.uri,
        {
          title: resource.title,
          description: resource.description,
          mimeType: resource.mimeType,
        },
        resource.handler as any
      );
    }
  }

  // Register prompts
  if (plugin.prompts) {
    for (const [name, prompt] of Object.entries(plugin.prompts)) {
      server.registerPrompt(
        name,
        {
          title: prompt.title,
          description: prompt.description,
          argsSchema: prompt.argsSchema
            ? zodToJsonSchema(prompt.argsSchema) as any
            : undefined,
        },
        async (args: Record<string, unknown>) => {
          const stringArgs = Object.fromEntries(
            Object.entries(args).map(([k, v]) => [k, String(v ?? '')])
          );
          const messages = prompt.messages(stringArgs);
          return {
            messages: messages.map(msg => ({
              role: msg.role,
              content: msg.content
            }))
          };
        }
      );
    }
  }

  console.log(`✓ Loaded plugin: ${plugin.name}`);
}
```

---

## Plugin Implementations

### mcp-roots Plugin (packages/mcp-roots/src/index.ts)

```typescript
import type { Plugin } from '@vpursuit/mcp-core';
import { z } from 'zod';
import { RootsManager } from './RootsManager.js';

/**
 * MCP Roots Plugin
 * Provides dynamic filesystem root discovery and management.
 */
export const plugin: Plugin = {
  name: 'mcp-roots',
  version: '1.0.0',
  description: 'Dynamic filesystem root discovery for MCP servers',

  tools: {
    roots_list: {
      description: 'List all discovered filesystem roots',
      inputSchema: z.object({}),
      handler: async () => {
        const manager = RootsManager.getInstance();
        const roots = await manager.getRoots();

        return {
          success: true,
          data: {
            roots: roots.map(r => ({
              uri: r.uri,
              name: r.name
            })),
            count: roots.length,
          },
        };
      },
    },
  },

  resources: {
    'roots-list': {
      uri: 'roots://list',
      title: 'Available Filesystem Roots',
      description: 'Dynamically discovered filesystem roots',
      mimeType: 'application/json',
      handler: async () => {
        const manager = RootsManager.getInstance();
        const roots = await manager.getRoots();

        return {
          contents: [{
            uri: 'roots://list',
            text: JSON.stringify(roots, null, 2),
            mimeType: 'application/json'
          }]
        };
      },
    },
  },

  async onInit(server) {
    // Initialize roots manager with server instance
    const manager = RootsManager.getInstance();
    await manager.initialize(server as any);
    console.log('✓ Roots manager initialized');
  },
};
```

### mcp-prolog Plugin Structure

```typescript
// packages/mcp-prolog/src/index.ts
import type { Plugin } from '@vpursuit/mcp-core';
import { PrologInterface } from './PrologInterface.js';
import { tools } from './tools.js';
import { resources } from './resources.js';
import { prompts } from './prompts.js';

export const plugin: Plugin = {
  name: 'mcp-prolog',
  version: '2.1.0',
  description: 'SWI-Prolog integration for MCP servers',

  tools,      // Exported from tools.ts
  resources,  // Exported from resources.ts
  prompts,    // Exported from prompts.ts

  async onInit() {
    const prolog = PrologInterface.getInstance();
    await prolog.start();
    console.log('✓ Prolog interface started');
  },
};

// packages/mcp-prolog/src/tools.ts
import { z } from 'zod';
import type { ToolDefinitions } from '@vpursuit/mcp-core';
import { PrologInterface } from './PrologInterface.js';

export const tools: ToolDefinitions = {
  knowledge_base_load: {
    description: 'Load a Prolog file into the knowledge base',
    inputSchema: z.object({
      filename: z.string().max(1000),
    }),
    handler: async ({ filename }) => {
      const prolog = PrologInterface.getInstance();
      return await prolog.loadFile(filename);
    },
  },

  query_start: {
    description: 'Start a new Prolog query',
    inputSchema: z.object({
      query: z.string().max(5000),
    }),
    handler: async ({ query }) => {
      const prolog = PrologInterface.getInstance();
      return await prolog.startQuery(query);
    },
  },

  // ... all other Prolog tools (15+)
};
```

### Main Orchestrator (packages/swipl-mcp-server/src/index.ts)

```typescript
#!/usr/bin/env node
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { loadPlugins } from '@vpursuit/mcp-core';
import { plugin as rootsPlugin } from '@vpursuit/mcp-roots';
import { plugin as prologPlugin } from '@vpursuit/mcp-prolog';

/**
 * SWI-Prolog MCP Server
 *
 * Orchestrates plugins:
 * - @vpursuit/mcp-roots: Filesystem root discovery
 * - @vpursuit/mcp-prolog: SWI-Prolog integration
 */
async function main() {
  console.log('Starting SWI-Prolog MCP Server...');

  // Create MCP server instance
  const server = new McpServer({
    name: 'swipl-mcp-server',
    version: '3.0.0',
  });

  // Load all plugins (simple array)
  await loadPlugins(server, [
    rootsPlugin,
    prologPlugin,
  ]);

  // Connect stdio transport
  const transport = new StdioServerTransport();
  await server.connect(transport);

  console.log('✓ Server ready');
}

main().catch((error) => {
  console.error('Fatal error:', error);
  process.exit(1);
});
```

---

## Package Dependencies

### Root package.json (Workspace Configuration)

```json
{
  "name": "@vpursuit/workspace",
  "version": "1.0.0",
  "private": true,
  "workspaces": [
    "packages/*"
  ],
  "scripts": {
    "build": "npm run build --workspaces --if-present",
    "test": "vitest",
    "clean": "rm -rf packages/*/build"
  },
  "devDependencies": {
    "@types/node": "^22.10.2",
    "typescript": "^5.7.2",
    "vitest": "^2.1.8"
  }
}
```

### packages/mcp-core/package.json

```json
{
  "name": "@vpursuit/mcp-core",
  "version": "1.0.0",
  "description": "Core plugin system for MCP servers",
  "type": "module",
  "main": "./build/index.js",
  "types": "./build/index.d.ts",
  "exports": {
    ".": "./build/index.js",
    "./types": "./build/types.js"
  },
  "files": [
    "build",
    "LICENSE",
    "README.md"
  ],
  "scripts": {
    "build": "tsc",
    "test": "vitest run"
  },
  "dependencies": {
    "@modelcontextprotocol/sdk": "^1.0.4",
    "zod": "^3.24.1",
    "zod-to-json-schema": "^3.24.1"
  },
  "license": "BSD-3-Clause"
}
```

### packages/mcp-roots/package.json

```json
{
  "name": "@vpursuit/mcp-roots",
  "version": "1.0.0",
  "description": "Dynamic filesystem root discovery for MCP servers",
  "type": "module",
  "main": "./build/index.js",
  "types": "./build/index.d.ts",
  "exports": {
    ".": "./build/index.js"
  },
  "files": [
    "build",
    "LICENSE",
    "README.md"
  ],
  "scripts": {
    "build": "tsc",
    "test": "vitest run"
  },
  "peerDependencies": {
    "@vpursuit/mcp-core": "^1.0.0"
  },
  "dependencies": {
    "@modelcontextprotocol/sdk": "^1.0.4",
    "zod": "^3.24.1"
  },
  "license": "BSD-3-Clause"
}
```

### packages/mcp-prolog/package.json

```json
{
  "name": "@vpursuit/mcp-prolog",
  "version": "2.1.0",
  "description": "SWI-Prolog integration for MCP servers",
  "type": "module",
  "main": "./build/index.js",
  "types": "./build/index.d.ts",
  "exports": {
    ".": "./build/index.js"
  },
  "files": [
    "build",
    "prolog",
    "LICENSE",
    "README.md"
  ],
  "scripts": {
    "build": "tsc",
    "test": "vitest run"
  },
  "peerDependencies": {
    "@vpursuit/mcp-core": "^1.0.0"
  },
  "dependencies": {
    "@modelcontextprotocol/sdk": "^1.0.4",
    "zod": "^3.24.1"
  },
  "license": "BSD-3-Clause"
}
```

### packages/swipl-mcp-server/package.json

```json
{
  "name": "@vpursuit/swipl-mcp-server",
  "version": "3.0.0",
  "description": "SWI-Prolog MCP Server with plugin architecture",
  "type": "module",
  "bin": {
    "swipl-mcp-server": "./build/index.js"
  },
  "files": [
    "build",
    "LICENSE",
    "README.md"
  ],
  "scripts": {
    "build": "tsc",
    "server": "node build/index.js",
    "test": "vitest run"
  },
  "dependencies": {
    "@vpursuit/mcp-core": "workspace:*",
    "@vpursuit/mcp-roots": "workspace:*",
    "@vpursuit/mcp-prolog": "workspace:*",
    "@modelcontextprotocol/sdk": "^1.0.4"
  },
  "license": "BSD-3-Clause"
}
```

**Note**: Use `workspace:*` for development, replaced with actual versions during npm publish.

---

## Implementation Phases

### Phase 1: Create Monorepo Structure (30 min)

**Tasks**:
1. Create `packages/` directory
2. Create root `package.json` with workspaces configuration
3. Create `tsconfig.base.json`:
   ```json
   {
     "compilerOptions": {
       "target": "ES2022",
       "module": "Node16",
       "moduleResolution": "Node16",
       "strict": true,
       "esModuleInterop": true,
       "skipLibCheck": true,
       "declaration": true,
       "outDir": "./build",
       "rootDir": "./src"
     }
   }
   ```
4. Create `vitest.workspace.ts`:
   ```typescript
   import { defineWorkspace } from 'vitest/config';

   export default defineWorkspace([
     'packages/*/vitest.config.ts'
   ]);
   ```
5. Update root `.gitignore`:
   ```
   node_modules/
   packages/*/build/
   packages/*/node_modules/
   *.log
   .DS_Store
   ```

**Git**:
```bash
git checkout -b feature/plugin-architecture
git add packages/ package.json tsconfig.base.json vitest.workspace.ts .gitignore
git commit -m "Create monorepo workspace structure"
```

### Phase 2: Create mcp-core Package (45 min)

**Tasks**:
1. Create directory structure:
   ```
   packages/mcp-core/
   ├── src/
   │   ├── index.ts
   │   ├── types.ts
   │   ├── loader.ts
   │   └── utils/
   │       ├── response.ts
   │       └── validation.ts
   ├── package.json
   └── tsconfig.json
   ```

2. Copy shared utilities from current codebase:
   - `src/utils/response.ts` (createErrorResponse, createSuccessResponse)
   - `src/utils/validation.ts` (validateInput)
   - `src/constants.ts` → `src/utils/constants.ts`

3. Create `src/index.ts`:
   ```typescript
   export * from './types.js';
   export * from './loader.js';
   export * from './utils/response.js';
   export * from './utils/validation.js';
   export * from './utils/constants.js';
   ```

4. Build: `npm install && npm run build`

5. Test: Create basic unit tests for loadPlugins

**Git**:
```bash
git add packages/mcp-core/
git commit -m "Create mcp-core package with plugin system"
```

### Phase 3: Create mcp-roots Package (1 hour)

**Tasks**:
1. Create directory structure:
   ```
   packages/mcp-roots/
   ├── src/
   │   ├── index.ts
   │   ├── RootsManager.ts
   │   ├── tools.ts
   │   ├── resources.ts
   │   └── schemas.ts
   ├── package.json
   └── tsconfig.json
   ```

2. Copy roots implementation from `feature/roots-implementation` branch:
   ```bash
   git show feature/roots-implementation:src/utils/roots.ts > packages/mcp-roots/src/RootsManager.ts
   ```

3. Restructure as plugin:
   - Create `tools.ts` with `roots_list` tool
   - Create `resources.ts` with roots resources
   - Create `schemas.ts` with Zod schemas
   - Create `index.ts` exporting `plugin` object

4. Build: `cd packages/mcp-roots && npm install && npm run build`

5. Test: Port roots tests from feature branch

**Git**:
```bash
git add packages/mcp-roots/
git commit -m "Create mcp-roots plugin package"
```

### Phase 4: Create mcp-prolog Package (2 hours)

**Tasks**:
1. Create directory structure:
   ```
   packages/mcp-prolog/
   ├── src/
   │   ├── index.ts
   │   ├── PrologInterface.ts
   │   ├── tools.ts
   │   ├── resources.ts
   │   ├── prompts.ts
   │   └── schemas.ts
   ├── prolog/
   │   └── server.pl
   ├── package.json
   └── tsconfig.json
   ```

2. Move files from current `src/`:
   - `PrologInterface.ts` → `packages/mcp-prolog/src/`
   - Extract all Prolog tools from `tools.ts` → `packages/mcp-prolog/src/tools.ts`
   - Extract all Prolog resources from `index.ts` → `packages/mcp-prolog/src/resources.ts`
   - Move `prompts.ts` → `packages/mcp-prolog/src/prompts.ts`
   - Extract Prolog schemas from `schemas.ts` → `packages/mcp-prolog/src/schemas.ts`
   - Copy `prolog/server.pl` → `packages/mcp-prolog/prolog/`

3. Restructure tools.ts:
   ```typescript
   import type { ToolDefinitions } from '@vpursuit/mcp-core';

   export const tools: ToolDefinitions = {
     help: { /* ... */ },
     license: { /* ... */ },
     knowledge_base_load: { /* ... */ },
     // ... all 15+ tools
   };
   ```

4. Create plugin export in index.ts

5. Build: `cd packages/mcp-prolog && npm install && npm run build`

6. Test: Port all existing Prolog tests

**Git**:
```bash
git add packages/mcp-prolog/
git commit -m "Create mcp-prolog plugin package"
```

### Phase 5: Create Orchestrator Package (1 hour)

**Tasks**:
1. Create directory structure:
   ```
   packages/swipl-mcp-server/
   ├── src/
   │   └── index.ts
   ├── package.json
   └── tsconfig.json
   ```

2. Create minimal `index.ts` (see "Main Orchestrator" section above)

3. Copy logger from current src/:
   ```
   packages/swipl-mcp-server/src/logger.ts
   ```

4. Build: `cd packages/swipl-mcp-server && npm install && npm run build`

5. Test manually:
   ```bash
   node packages/swipl-mcp-server/build/index.js
   ```

6. Verify all features work via MCP Inspector

**Git**:
```bash
git add packages/swipl-mcp-server/
git commit -m "Create swipl-mcp-server orchestrator package"
```

### Phase 6: Update Build & CI/CD (1 hour)

**Tasks**:
1. Create workspace build script in root:
   ```json
   {
     "scripts": {
       "build": "npm run build --workspaces --if-present",
       "build:core": "npm run build -w @vpursuit/mcp-core",
       "build:roots": "npm run build -w @vpursuit/mcp-roots",
       "build:prolog": "npm run build -w @vpursuit/mcp-prolog",
       "build:server": "npm run build -w @vpursuit/swipl-mcp-server"
     }
   }
   ```

2. Update `.github/workflows/npm-publish.yml`:
   - Add separate jobs for each package
   - Use matrix strategy for parallel builds
   - Tag-based triggers: `core-v*`, `roots-v*`, `prolog-v*`, `v*`
   - Publish only changed packages

3. Create release script:
   ```bash
   # scripts/release.sh
   #!/bin/bash
   PACKAGE=$1
   VERSION=$2

   cd packages/$PACKAGE
   npm version $VERSION
   git tag "${PACKAGE}-v${VERSION}"
   git push origin "${PACKAGE}-v${VERSION}"
   ```

**Git**:
```bash
git add .github/ scripts/
git commit -m "Update CI/CD for multi-package publishing"
```

### Phase 7: Testing & Documentation (1 hour)

**Tasks**:
1. Run full test suite:
   ```bash
   npm test
   ```

2. Create `ARCHITECTURE.md`:
   - Explain plugin system
   - Show how to create new plugins
   - Document Plugin interface

3. Update root `README.md`:
   - New architecture section
   - Installation for each package
   - Usage examples

4. Create `MIGRATION.md`:
   - Guide for upgrading from v2.x to v3.x
   - Breaking changes (if any)
   - New features

5. Update package READMEs:
   - `packages/mcp-core/README.md`
   - `packages/mcp-roots/README.md`
   - `packages/mcp-prolog/README.md`

**Git**:
```bash
git add *.md packages/*/README.md
git commit -m "Add documentation for plugin architecture"
```

---

## File Migration Map

### From Current Codebase to New Structure

| Current File | New Location | Notes |
|--------------|--------------|-------|
| `src/constants.ts` | `packages/mcp-core/src/utils/constants.ts` | Shared constants |
| `src/utils/response.ts` | `packages/mcp-core/src/utils/response.ts` | Shared utilities |
| `src/utils/validation.ts` | `packages/mcp-core/src/utils/validation.ts` | Shared utilities |
| `src/logger.ts` | `packages/swipl-mcp-server/src/logger.ts` | Server-specific |
| `src/PrologInterface.ts` | `packages/mcp-prolog/src/PrologInterface.ts` | Prolog plugin |
| `src/tools.ts` | `packages/mcp-prolog/src/tools.ts` | Restructured as ToolDefinitions |
| `src/prompts.ts` | `packages/mcp-prolog/src/prompts.ts` | Restructured as PromptDefinitions |
| `src/schemas.ts` | `packages/mcp-prolog/src/schemas.ts` | Prolog schemas |
| `src/index.ts` (resources) | `packages/mcp-prolog/src/resources.ts` | Extract resource handlers |
| `src/index.ts` (main) | `packages/swipl-mcp-server/src/index.ts` | Simplified orchestrator |
| `prolog/server.pl` | `packages/mcp-prolog/prolog/server.pl` | Prolog server script |
| `feature/roots-implementation:src/utils/roots.ts` | `packages/mcp-roots/src/RootsManager.ts` | From feature branch |
| `test/unit/*.test.ts` | `packages/mcp-prolog/test/unit/*.test.ts` | Split by package |
| `test/integration/*.test.ts` | `packages/swipl-mcp-server/test/integration/*.test.ts` | Integration tests at top level |

---

## Code Transformation Examples

### Before: Current tools.ts (monolithic)

```typescript
// src/tools.ts
export const toolHandlers = {
  async help({ topic }: { topic?: string } = {}): Promise<ToolResponse> {
    // ...
  },
  async knowledgeBaseLoad({ filename }: { filename: string }): Promise<ToolResponse> {
    // ...
  },
  // ... 15+ more handlers
};
```

### After: Plugin-based tools

```typescript
// packages/mcp-prolog/src/tools.ts
import type { ToolDefinitions } from '@vpursuit/mcp-core';
import { z } from 'zod';

export const tools: ToolDefinitions = {
  help: {
    description: 'Get usage guidelines and tips',
    inputSchema: z.object({
      topic: z.string().optional(),
    }),
    handler: async ({ topic }) => {
      // ... same logic as before
    },
  },

  knowledge_base_load: {
    description: 'Load a Prolog file into the knowledge base',
    inputSchema: z.object({
      filename: z.string().max(1000),
    }),
    handler: async ({ filename }) => {
      // ... same logic as before
    },
  },

  // ... all other tools
};
```

### Before: Current index.ts (manual registration)

```typescript
// src/index.ts
server.registerTool(
  "help",
  {
    description: "Get usage guidelines",
    inputSchema: inputSchemas.help,
  },
  toolHandlers.help as any,
);

server.registerTool(
  "knowledge_base_load",
  {
    description: "Load a Prolog file",
    inputSchema: inputSchemas.knowledgeBaseLoad,
  },
  toolHandlers.knowledgeBaseLoad as any,
);

// ... repeat 15+ times
```

### After: Plugin-based registration (automatic)

```typescript
// packages/swipl-mcp-server/src/index.ts
import { loadPlugins } from '@vpursuit/mcp-core';
import { plugin as prologPlugin } from '@vpursuit/mcp-prolog';

// All tools registered automatically
await loadPlugins(server, [prologPlugin]);
```

---

## Success Criteria

**Before Merging to Main**:
- ✅ All existing tests pass (98/98)
- ✅ All 4 packages build successfully without errors
- ✅ `npx @vpursuit/swipl-mcp-server` works identically to v2.0.6
- ✅ Each plugin can be imported and used independently
- ✅ No code duplication (shared utilities in mcp-core)
- ✅ GitHub Actions can build and publish all packages
- ✅ MCP Inspector shows all tools, resources, and prompts
- ✅ Documentation is complete and accurate

**Functional Testing**:
- ✅ Load Prolog file and query works
- ✅ All Prolog prompts work
- ✅ Roots discovery works (when roots feature is implemented)
- ✅ Error handling works correctly
- ✅ Timeouts work correctly
- ✅ Security validation works (path restrictions, dangerous predicates)

---

## Rollback Strategy

**Safety Mechanisms**:
1. Safety tag: `pre-plugin-arch-v2.0.6` already exists on main
2. Feature branch: All work happens in `feature/plugin-architecture`
3. Previous work saved: `feature/roots-implementation` branch on GitHub
4. Can abandon feature branch if issues arise

**Rollback Steps** (if needed):
```bash
# Abandon feature branch
git checkout main

# Or if merged and need to revert
git revert <merge-commit-sha>

# Or nuclear option
git reset --hard pre-plugin-arch-v2.0.6
```

---

## Future Extension Points

**After v3.0.0 Launch**:

1. **New Plugin: mcp-database**
   - SQL query tools
   - Database resources
   - Schema introspection

2. **New Plugin: mcp-web**
   - Web scraping tools
   - HTTP request tools
   - URL resources

3. **Plugin Configuration System**
   - Environment-based plugin loading
   - Plugin-specific config files
   - Dynamic plugin enable/disable

4. **Third-Party Plugin Support**
   - Document plugin API
   - Create plugin template
   - npm scoped packages: `@mcp-plugin/*`

5. **Plugin Marketplace**
   - Curated plugin registry
   - Plugin discovery
   - Version compatibility matrix

---

## Estimated Timeline

| Phase | Time | Cumulative |
|-------|------|------------|
| 1. Monorepo Structure | 30 min | 30 min |
| 2. mcp-core Package | 45 min | 1h 15min |
| 3. mcp-roots Package | 1 hour | 2h 15min |
| 4. mcp-prolog Package | 2 hours | 4h 15min |
| 5. Orchestrator Package | 1 hour | 5h 15min |
| 6. Build & CI/CD | 1 hour | 6h 15min |
| 7. Testing & Docs | 1 hour | 7h 15min |

**Total: ~7-8 hours** (can be done in 2-3 sessions)

---

## Key Differences from Previous Monorepo Plan

| Aspect | Previous Plan (MONOREPO_MIGRATION_PLAN.md) | New Plugin Plan |
|--------|---------------------------------------------|-----------------|
| **Philosophy** | Split roots and server into separate packages | Modular "Lego bricks" - everything is a plugin |
| **Prolog Role** | Stays in server core | Becomes a plugin (@vpursuit/mcp-prolog) |
| **Architecture** | 2 packages (roots + server) | 4 packages (core + roots + prolog + server) |
| **Extensibility** | Hard to add new features | Easy to add new plugins |
| **Server Role** | Contains Prolog logic | Just an orchestrator |
| **Reusability** | Roots reusable | Core, roots, AND prolog reusable |
| **Complexity** | Lower | Slightly higher upfront, simpler long-term |

---

## Questions to Resolve Before Implementation

1. **Version Strategy**: How to version packages independently?
   - Option A: Independent semantic versioning (core v1.0.0, prolog v2.1.0)
   - Option B: Synchronized versioning (all start at v3.0.0)

2. **Build Order**: Should we enforce build dependency order?
   - core must build before roots/prolog
   - roots/prolog must build before server
   - Use npm workspaces automatic resolution?

3. **Testing Strategy**: Where should integration tests live?
   - In swipl-mcp-server package (tests the orchestrator)
   - In each plugin package (tests plugin independently)
   - Both?

4. **Shared Assets**: Where should shared assets go?
   - `prolog/server.pl` script stays in mcp-prolog
   - `images/logo.svg` - duplicate or shared?
   - LICENSE file - duplicate or shared?

5. **Breaking Changes**: Is v3.0.0 acceptable for this refactor?
   - API stays the same (npx @vpursuit/swipl-mcp-server still works)
   - But internal structure completely changes
   - Warrant major version bump?

---

## Current State

**Branch**: `main` (clean, tagged with `pre-monorepo-v2.0.6`)

**Git Status**:
```
M README.md
M SECURITY.md
M scripts/build-package.js
M src/constants.ts
M src/index.ts
M src/schemas.ts
M src/tools.ts
M test/integration/security.test.ts
M test/npx-integration.test.js
?? IMPLEMENTATION_SUMMARY.md
?? NOTIFICATION_SUPPORT.md
?? PLUGIN_ARCHITECTURE_PLAN.md  (this file)
?? ROOTS_IMPLEMENTATION_PLAN.md
?? ROOTS_PROGRESS.md
?? src/utils/roots.ts
?? test/unit/roots.test.ts
```

**Saved Work**:
- Roots feature: `feature/roots-implementation` branch (on GitHub)
- Monorepo plan: `MONOREPO_MIGRATION_PLAN.md` (in that branch)

**Ready**: Create `feature/plugin-architecture` branch and begin Phase 1.

---

## References

**Current Codebase**:
- `src/index.ts` (lines 35-174: tool registration)
- `src/index.ts` (lines 305-341: prompt registration loop)
- `src/tools.ts` (lines 106-839: tool handlers)
- `src/prompts.ts` (prompt interfaces)
- `src/schemas.ts` (schema centralization)

**Feature Branch**:
- `feature/roots-implementation` (commit 4e13766)
- Contains: roots.ts, roots.test.ts, updated docs

**Documentation**:
- FastMCP: https://gofastmcp.com/
- MCP TypeScript SDK: https://github.com/modelcontextprotocol/typescript-sdk
- npm workspaces: https://docs.npmjs.com/cli/v10/using-npm/workspaces

---

*This plan is designed to be safely memorizable and comprehensive enough to execute the plugin architecture migration across multiple sessions.*
