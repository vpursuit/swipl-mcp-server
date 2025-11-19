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

- **query**: Unified tool for query operations - start queries (standard or engine mode), iterate solutions, close sessions
- **capabilities**: Get machine-readable summary of available tools, query modes, environment config, and security model
- **clauses**: Assert or retract facts/rules in knowledge base with source preservation
- **files**: Import/unimport Prolog files with provenance tracking (tracks which clauses came from which files)
- **workspace**: Introspection and management - get snapshots with preserved formatting, reset workspace, list symbols
- **explain_error**: Analyze and explain Prolog errors with 15+ fast-path heuristics (instant), AI-powered explanations (via sampling), and rule-based fallbacks. Prioritizes `clauses` tool over `files` tool (no file access needed)

### Resources

- `mcp://workspace/symbols` - List all user-defined predicates in the workspace
- `mcp://workspace/snapshot` - Export workspace with original source text (preserved formatting)
- `mcp://server/branding/logo` - Server logo in SVG format
- `mcp://server/capabilities` - Machine-readable capabilities summary (JSON)

### Prompts

- `genealogy` - Build and query family trees using relational logic. Demonstrates assert operations, recursive rules, query modes, and relationship inference.
- `scheduling` - Schedule tasks with dependencies using CLP(FD) constraints. Demonstrates library loading, constraint solving, and labeling optimization.
- `puzzle` - Solve logic puzzles using constraint programming. Demonstrates constraint encoding, all_different, and labeling strategies.

### Security

- File operations restricted to `~/.model-context-lab/`
- Dangerous predicates blocked (shell, system, call, halt)
- Pre-execution validation via `library(sandbox)`
- Timeout protection against infinite loops

### Library Loading

Prolog libraries provide additional predicates for specialized tasks. This server pre-loads several safe libraries and allows configuration of additional ones.

#### Pre-loaded Libraries

The following libraries are automatically available:
- `lists` - List manipulation (member, append, length, etc.)
- `apply` - Higher-order predicates (maplist, include, etc.)
- `between` - Range generation
- `pairs` - Key-value pair operations
- `ordsets` - Ordered set operations
- `clpfd` - Constraint Logic Programming over Finite Domains

#### Configuring Additional Libraries

Use the `KB_LIBRARIES` environment variable to load additional safe libraries at server startup:

```bash
# Load multiple libraries (comma-separated)
KB_LIBRARIES="aggregate,assoc,rbtrees" npm start

# Or with npx
KB_LIBRARIES="aggregate,assoc" npx @vpursuit/swipl-mcp-server
```

#### Safe Libraries List

Only libraries approved by SWI-Prolog's sandbox system can be loaded:
- `aggregate` - Aggregation predicates
- `assoc` - Association lists (balanced trees)
- `clpb` - Boolean constraint solving
- `clpfd` - Finite domain constraints
- `heaps` - Heap operations
- `lists` - List manipulation
- `nb_set` - Non-backtrackable sets
- `occurs` - Occurs check utilities
- `option` - Option list processing
- `ordsets` - Ordered sets
- `pairs` - Key-value pairs
- `random` - Random number generation
- `rbtrees` - Red-black trees
- `solution_sequences` - findall/bagof/setof alternatives
- `terms` - Term manipulation
- `ugraphs` - Graph operations

**Note**: Libraries are loaded server-wide and apply to all AI agent sessions. Library loading via MCP tools is not currently supported but could be added if needed.

### Error Analysis Configuration

The `explain_error` tool uses AI-powered analysis via MCP sampling. The timeout for LLM requests can be configured:

#### SWI_MCP_ERROR_ANALYSIS_TIMEOUT_MS

Set the timeout (in milliseconds) for error analysis requests. This is especially important when using slow LLMs like LM Studio or local models.

```bash
# Default: 60000ms (60 seconds)
# Increase for slow local models
export SWI_MCP_ERROR_ANALYSIS_TIMEOUT_MS=120000

# Or inline with server start
SWI_MCP_ERROR_ANALYSIS_TIMEOUT_MS=120000 npm start
```

**Default**: 60 seconds (60000ms)

**When to increase**:
- Using LM Studio with local models
- Using slow cloud LLMs with high latency
- Complex error analysis requiring longer generation time

**Note**: Fast-path explanations (15+ common error patterns) are instant and not affected by this timeout.

### Common Pitfalls

#### Understanding the `clauses` Tool Input Format

**The most common mistake**: Treating array elements as lines of code instead of complete clauses.

❌ **WRONG** - Splitting a single rule across array elements:
```typescript
// This FAILS with syntax errors - each element is incomplete
await clauses({
  operation: 'assert',
  clauses: [
    "solve(X) :-",
    "  length(X, 10),",
    "  X ins 1..10."
  ]
});
```

✅ **CORRECT** - Multi-line rule as a single string:
```typescript
// This works - entire rule is one complete clause
await clauses({
  operation: 'assert',
  clauses: "solve(X) :- length(X, 10), X ins 1..10, label(X)."
});
```

✅ **CORRECT** - Array of multiple complete clauses:
```typescript
// Each element is a complete, independent clause
await clauses({
  operation: 'assert',
  clauses: [
    "safe(_, _, _, []).",
    "safe(Q, D, R, [H|T]) :- Q #\\= H, abs(Q - H) #\\= D, safe(Q, D + 1, R, T).",
    "solve(Board) :- length(Board, 20), Board ins 1..20, all_distinct(Board)."
  ]
});
```

#### Comments Are Not Supported in `clauses` Tool

❌ **WRONG** - Including comments in assertions:
```typescript
await clauses({
  operation: 'assert',
  clauses: "% This is a comment\nsolve(X) :- length(X, 10)."
});
// Fails: Comments cause Prolog syntax errors in assertz()
```

✅ **CORRECT** - Use files tool for commented code:
```prolog
% Create a file: queens.pl
% N-Queens problem solver using CLP(FD)
solve(Board) :-
    length(Board, 20),
    Board ins 1..20,
    all_distinct(Board).
```

```typescript
// Load the file
await files({ operation: 'import', filename: 'queens.pl' });
```

#### When to Use Which Tool

**Use `clauses` tool when:**
- Adding simple facts: `parent(john, mary).`
- Adding rules without comments
- Loading additional libraries (note: clpfd is pre-loaded)
- Working with complete, self-contained clauses

**Use `files` tool when:**
- Code includes comments (% annotations)
- Complex multi-predicate systems
- Code already exists in .pl files
- Want to preserve documentation

**Example: 20 Queens Problem**

❌ **Inefficient** - Fighting with `clauses` tool (will fail many times):
```typescript
// Agent tries to pass multi-line code with comments
// Gets syntax errors, retries 15+ times, wastes tokens
```

✅ **Efficient** - Correct approach:
```typescript
// Option 1: Complete clauses without comments (clpfd pre-loaded - use directly)
await clauses({ operation: 'assert', clauses: 'safe(_, _, _, []).' });
await clauses({ operation: 'assert', clauses: 'safe(Q, D, R, [H|T]) :- Q #\\= H, abs(Q - H) #\\= D, safe(Q, D + 1, R, T).' });
// ... etc

// Option 2: Create file and import (BEST for complex code)
// 1. Write queens.pl with full comments
// 2. await files({ operation: 'import', filename: 'queens.pl' })
```

### Limitations

#### DCG Rules Not Supported

Definite Clause Grammar (DCG) notation using `-->` is not supported in imported files. Use the expanded difference-list form instead:

```prolog
% ❌ DCG notation (not supported)
sentence --> noun, verb.

% ✅ Expanded difference-list form (supported)
sentence(S0, S) :- noun(S0, S1), verb(S1, S).
```

## Examples

### Loading and Querying

```typescript
// Load a Prolog file
await toolHandlers.files({ operation: 'load', filename: '~/.model-context-lab/family.pl' });

// Add facts
await toolHandlers.clauses({ operation: 'assert', clauses: 'parent(john, mary)' });

// Start query
await toolHandlers.query({ operation: 'start', query: 'parent(X, mary)' });

// Iterate through solutions using standard iterator pattern
let result;
do {
  result = await toolHandlers.query({ operation: 'next' });
  if (result.structuredContent?.status === 'success') {
    console.log(result.structuredContent.solution);
  }
} while (result.structuredContent?.status !== 'done');

// Close query
await toolHandlers.query({ operation: 'close' });
```

### Engine Mode (True Backtracking)

```typescript
// Start engine query (use_engine: true for true backtracking)
await toolHandlers.query({ operation: 'start', use_engine: true, query: 'member(X, [1,2,3])' });

// Iterate using standard iterator pattern
let result;
do {
  result = await toolHandlers.query({ operation: 'next' });
  if (result.structuredContent?.status === 'success') {
    console.log(result.structuredContent.solution); // X=1, X=2, X=3
  }
} while (result.structuredContent?.status !== 'done');

await toolHandlers.query({ operation: 'close' });
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
