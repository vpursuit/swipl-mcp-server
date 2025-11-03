# @vpursuit/mcp-server-prolog

**Internal SWI-Prolog integration plugin for Model Context Protocol servers.**

> ⚠️ **Internal Package**: This plugin is private to the [@vpursuit/swipl-mcp-server](https://github.com/vpursuit/model-context-lab) monorepo and is bundled into the main product. It is not published separately to npm.

## Overview

`@vpursuit/mcp-server-prolog` is a comprehensive plugin that adds SWI-Prolog knowledge base management and query capabilities to MCP servers. It provides tools for loading Prolog files, managing facts and rules, executing queries with two modes (standard and engine-based), and includes security sandboxing.

## Installation

**For monorepo development only:**

```bash
# Clone the monorepo
git clone https://github.com/vpursuit/model-context-lab.git
cd model-context-lab
npm install
npm run build
```

This package is available as a workspace dependency within the monorepo.

## Requirements

- Node.js >= 20.0.0
- SWI-Prolog installed and available in PATH (`swipl --version` should work)

## Usage

### As a Plugin

```typescript
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { loadPlugins } from '@vpursuit/mcp-server-core';
import { plugin as prologPlugin } from '@vpursuit/mcp-server-prolog';

const server = new McpServer({
  name: 'my-prolog-server',
  version: '1.0.0',
});

// Load the Prolog plugin
await loadPlugins(server, [prologPlugin]);
```

### Direct Access

```typescript
import { prologInterface, toolHandlers } from '@vpursuit/mcp-server-prolog';

// Start Prolog process
await prologInterface.start();

// Execute commands directly
const result = await prologInterface.executeCommand('parent(X, mary)');
```

## Features

### Tools

- **Knowledge Base Management**: load, assert, retract, clear, dump
- **Query Execution**: Two modes - standard (`call_nth/2`) and engine (true backtracking)
- **Symbol Listing**: Enumerate available predicates
- **Metadata**: capabilities, help, license information

### Resources

- `prolog://knowledge_base/predicates` - List all predicates
- `prolog://knowledge_base/dump` - Export knowledge base
- `reference://help` - Usage guidelines
- `reference://license` - License text
- `reference://capabilities` - Machine-readable capabilities

### Prompts

- `prolog_init_expert` - Initialize expert Prolog assistance
- `prolog_quick_reference` - Server overview and capabilities
- `prolog_analyze_knowledge_base` - Analyze KB structure
- `prolog_knowledge_base_builder` - Build domain-specific KBs
- `prolog_query_optimizer` - Optimize query performance

### Security

- File operations restricted to `~/.model-context-lab/`
- Dangerous predicates blocked (shell, system, call, halt)
- Pre-execution validation via `library(sandbox)`
- Timeout protection against infinite loops

## Examples

### Loading and Querying

```typescript
// Load a Prolog file
await toolHandlers.knowledge_base_load({ filename: '~/.model-context-lab/family.pl' });

// Add facts
await toolHandlers.knowledge_base_assert({ fact: 'parent(john, mary)' });

// Start query
await toolHandlers.query_start({ query: 'parent(X, mary)' });

// Get solutions
await toolHandlers.query_next(); // First solution
await toolHandlers.query_next(); // Next solution

// Close query
await toolHandlers.query_close();
```

### Engine Mode (True Backtracking)

```typescript
// Start engine query
await toolHandlers.query_startEngine({ query: 'member(X, [1,2,3])' });

// Iterate through all solutions
await toolHandlers.query_next(); // X = 1
await toolHandlers.query_next(); // X = 2
await toolHandlers.query_next(); // X = 3
await toolHandlers.query_next(); // No more solutions

await toolHandlers.query_close();
```

## Documentation

Full documentation available in the [swipl-mcp-server monorepo](https://github.com/vpursuit/model-context-lab).

## Related Components

Internal monorepo plugins:
- [mcp-server-core](../core) - Plugin system
- [mcp-server-roots](../roots) - Filesystem roots

Published product:
- [@vpursuit/swipl-mcp-server](https://npmjs.com/package/@vpursuit/swipl-mcp-server) - Complete server (includes this plugin)

## License

BSD-3-Clause - see [LICENSE](./LICENSE) file for details.
