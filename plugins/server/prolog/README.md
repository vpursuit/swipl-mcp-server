# @vpursuit/mcp-server-prolog

SWI-Prolog integration plugin for Model Context Protocol servers.

## Overview

`@vpursuit/mcp-server-prolog` is a comprehensive plugin that adds SWI-Prolog knowledge base management and query capabilities to MCP servers. It provides tools for loading Prolog files, managing facts and rules, executing queries with two modes (standard and engine-based), and includes security sandboxing.

## Installation

```bash
npm install @vpursuit/mcp-server-prolog
```

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

- File operations restricted to `~/.swipl-mcp-server/`
- Dangerous predicates blocked (shell, system, call, halt)
- Pre-execution validation via `library(sandbox)`
- Timeout protection against infinite loops

## Examples

### Loading and Querying

```typescript
// Load a Prolog file
await toolHandlers.knowledge_base_load({ filename: '~/.swipl-mcp-server/family.pl' });

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

Full documentation available in the [swipl-mcp-server monorepo](https://github.com/vpursuit/swipl-mcp-server).

## Related Packages

- [@vpursuit/mcp-server-core](https://npmjs.com/package/@vpursuit/mcp-server-core) - Plugin system
- [@vpursuit/mcp-server-roots](https://npmjs.com/package/@vpursuit/mcp-server-roots) - Filesystem roots
- [@vpursuit/swipl-mcp-server](https://npmjs.com/package/@vpursuit/swipl-mcp-server) - Complete server

## License

BSD-3-Clause - see [LICENSE](./LICENSE) file for details.
