# Features Reference

Complete reference for all SWI-Prolog MCP Server features including prompts, resources, and tools.

## MCP Prompts

The server provides 4 expert Prolog assistance prompts that follow a resource-first approach, guiding AI agents to discover and use server capabilities efficiently.

### Slash Command Access

In some MCP clients like Claude Code CLI and Codex, these prompts are available as convenient slash commands. This provides an intuitive way to access expert Prolog assistance without having to remember exact prompt names or syntax.

### `expert`

Expert guidance and comprehensive server reference.

**Arguments:**
- `task` (optional, string) - Specific task to focus expert setup
- `mode` (optional, string) - Mode: 'expert' (default) for guidance, 'reference' for complete overview

**Purpose:**
When mode='expert', establishes expert context with comprehensive Prolog knowledge covering:
- SWI-Prolog syntax, unification, and DCGs
- Logic programming paradigms and best practices
- Built-in predicates and meta-programming
- Query optimization techniques
- Security awareness of server constraints

When mode='reference', provides complete server overview and capabilities orientation.

**Example Usage:**
```json
{
  "name": "expert",
  "arguments": {
    "task": "family relationship modeling",
    "mode": "expert"
  }
}
```

### `knowledge`

Build or analyze knowledge bases with expert guidance.

**Arguments:**
- `domain` (optional, string) - Domain to model (required when mode='build')
- `mode` (optional, string) - Mode: 'build' (default) to create KB, 'analyze' to examine existing KB

**Purpose:**
When mode='build', guides systematic knowledge base construction:
- Domain modeling best practices
- Predicate design patterns
- Fact and rule organization
- Testing and validation strategies
- Documentation and maintainability

When mode='analyze', performs comprehensive analysis of:
- Current predicates and their relationships
- Knowledge base structure and dependencies
- Data integrity and consistency
- Query optimization opportunities
- Suggested improvements or extensions

**Example Usage:**
```json
{
  "name": "knowledge",
  "arguments": {
    "domain": "medical diagnosis expert system",
    "mode": "build"
  }
}
```

### `optimize`

Optimize Prolog queries for performance and efficiency.

**Arguments:**
- `query` (required, string) - Prolog query to analyze and optimize

**Purpose:**
Expert query optimization covering:
- Predicate ordering for efficiency
- Cut placement strategies
- Indexing considerations
- Alternative formulations
- Performance measurement techniques

**Example Usage:**
```json
{
  "name": "optimize",
  "arguments": {
    "query": "ancestor(X, Y) :- parent(X, Y). ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)."
  }
}
```

### `puzzle`

Solve logic puzzles using Prolog and constraint programming.

**Arguments:**
- `puzzle` (optional, string) - Logic puzzle to solve with numbered clues. If empty, agent chooses an interesting puzzle.

**Purpose:**
Direct problem-solving approach for logic puzzles using:
- Constraint programming with library(clpfd)
- Systematic encoding of puzzle constraints
- Solution finding and verification
- Step-by-step explanation

**Example Usage:**
```json
{
  "name": "puzzle",
  "arguments": {
    "puzzle": "Einstein's Riddle: There are 5 houses in 5 different colors..."
  }
}
```

## MCP Resources

The server provides 5 resources for accessing knowledge base state and server information.

### Knowledge Base Resources

#### `prolog://knowledge_base/predicates`

**URI:** `prolog://knowledge_base/predicates`
**MIME Type:** `text/plain`
**Type:** Dynamic

Lists all predicates currently defined in the knowledge_base module.

**Content Format:**
```prolog
parent/2
ancestor/2
grandparent/2
```

**Use Case:** Check what predicates are available before querying or to avoid name conflicts when adding new rules.

#### `prolog://knowledge_base/dump`

**URI:** `prolog://knowledge_base/dump`
**MIME Type:** `text/plain`
**Type:** Dynamic

Complete export of all facts and rules in the knowledge_base module as valid Prolog source code.

**Content Format:**
```prolog
:- module(knowledge_base, [parent/2, ancestor/2]).

parent(john, mary).
parent(mary, susan).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

**Use Case:** Backup knowledge base state, review all defined knowledge, or export for use in other Prolog systems.

### Reference Resources

#### `reference://help`

**URI:** `reference://help`
**MIME Type:** `text/plain`
**Type:** Static

Comprehensive usage guidelines, best practices, and tips for using the SWI-Prolog MCP Server effectively.

**Topics Covered:**
- Tool usage patterns and examples
- Session state management
- Security model and restrictions
- Query modes (standard vs engine)
- Performance optimization tips
- Troubleshooting common issues

#### `reference://license`

**URI:** `reference://license`
**MIME Type:** `text/plain`
**Type:** Static

Full text of the BSD-3-Clause license under which this server is distributed.

#### `reference://capabilities`

**URI:** `reference://capabilities`
**MIME Type:** `application/json`
**Type:** Static

Machine-readable summary of all server capabilities, tools, security constraints, and available predicates.

**Content Structure:**
```json
{
  "server": { "name": "swipl-mcp-server", "version": "..." },
  "modes": ["standard", "engine"],
  "predicates": {
    "standard_prolog": "All standard SWI-Prolog predicates available",
    "clpfd_available": true
  },
  "tools": {
    "core": ["capabilities"],
    "knowledge_base": ["clauses", "files", "workspace"],
    "query": ["query"],
    "analysis": ["explain_error"]
  },
  "prompts": {
    "domain_examples": ["genealogy", "scheduling", "puzzle", "grammar"]
  },
  "security": {...}
}
```

**Use Case:** Programmatic capability discovery, automated documentation generation, or validation of server features.

## MCP Tools

Complete reference of all available tools organized by category.

### Core Tools

#### `capabilities`

Get machine-readable server capabilities summary.

**Arguments:** None

**Returns:** JSON object with complete capability information including tools, query modes, security model, and available predicates.

### Query Tool

#### `query`

Unified tool for managing query sessions with support for both standard pagination and engine-based backtracking.

**Arguments:**
- `operation` (required) - Query operation: "start", "next", or "close"
- `query` (required for "start") - Prolog query string
- `use_engine` (optional, default: false) - Use SWI-Prolog engine mode for true backtracking. Required for CLP(FD) constraint solving.

**Operations:**

**start** - Begin a new query session
```json
{
  "operation": "start",
  "query": "parent(X, mary)",
  "use_engine": false
}
```

**next** - Get next solution from active query
```json
{
  "operation": "next"
}
```
Returns: `{solution: "X=john", status: "success"}` or `{solution: null, status: "done"}`

**close** - Close active query and free resources
```json
{
  "operation": "close"
}
```

**Query Modes:**
- **Standard mode** (`use_engine: false`) - Pagination using call_nth/2, efficient for deterministic queries
- **Engine mode** (`use_engine: true`) - True Prolog backtracking, required for CLP(FD) and complex constraint solving

### Knowledge Base Management Tools

#### `clauses`

Unified tool for asserting and retracting facts/rules with source text preservation.

**Arguments:**
- `operation` (required) - "assert" or "retract"
- `clauses` (required) - Single clause string or array of clauses

**Operations:**

**assert** - Add facts/rules to workspace
```json
{
  "operation": "assert",
  "clauses": "parent(john, mary)"
}
```

Multi-clause assertion:
```json
{
  "operation": "assert",
  "clauses": [
    "parent(john, mary)",
    "parent(mary, susan)",
    "ancestor(X,Y) :- parent(X,Y)"
  ]
}
```

**retract** - Remove matching facts/rules
```json
{
  "operation": "retract",
  "clauses": "parent(john, mary)"
}
```

**Source Preservation:** Original clause text, formatting, and variable names are preserved for snapshot export.

#### `files`

Manage Prolog file imports with provenance tracking.

**Arguments:**
- `operation` (required) - "import", "unimport", or "list"
- `filename` (required for import/unimport) - Path to .pl file (must be within configured roots)

**Operations:**

**import** - Load Prolog file into workspace
```json
{
  "operation": "import",
  "filename": "/Users/you/prolog/family.pl"
}
```

**unimport** - Remove all clauses from specific file
```json
{
  "operation": "unimport",
  "filename": "/Users/you/prolog/family.pl"
}
```

**list** - Show all imported files with clause counts
```json
{
  "operation": "list"
}
```

**Security:** File must be in allowed directory. See Configuration section.

#### `workspace`

Workspace introspection and management.

**Arguments:**
- `operation` (required) - "snapshot", "reset", or "list_symbols"

**Operations:**

**snapshot** - Export workspace with original source text
```json
{
  "operation": "snapshot"
}
```
Returns: Complete workspace content with preserved formatting and variable names

**reset** - FULL workspace reset
```json
{
  "operation": "reset"
}
```
Removes all facts/rules, clears source storage, and clears file import history

**list_symbols** - List all user-defined predicates
```json
{
  "operation": "list_symbols"
}
```
Returns: Array of predicate names (e.g., ["parent", "ancestor", "member"])

### Analysis Tools

#### `explain_error`

Analyze and explain Prolog errors using domain expertise and MCP sampling.

**Arguments:**
- `error` (required) - Structured error object with kind, message, and optional details
- `query` (optional) - The query that caused the error
- `include_kb` (optional, default: true) - Include current knowledge base state for context

**Example:**
```json
{
  "error": {
    "kind": "instantiation_error",
    "message": "Arguments are not sufficiently instantiated",
    "details": {
      "predicate": "member/2",
      "goal": "member(X, Y)"
    }
  },
  "query": "member(X, Y)",
  "include_kb": true
}
```

**Returns:** Structured explanation with:
- `explanation` - What went wrong
- `cause` - Root cause analysis
- `suggestions` - Concrete fixes
- `examples` - Code examples (optional)
- `tool_guidance` - Correct tool usage (optional)

## Session State Management

The server maintains session state to prevent conflicts between query modes:

- **idle**: No active query
- **query**: Standard mode query active (from `query` with `use_engine: false`)
- **engine**: Engine mode query active (from `query` with `use_engine: true`)
- **query_completed**: Query finished but not closed
- **engine_completed**: Engine finished but not closed

**Mutual Exclusion:** Cannot start Standard Mode query while Engine Mode is active and vice versa. Must use `query` with `operation: "close"` to switch modes.

## Security Model

### Sandbox Execution

All Prolog code executes in a sandboxed environment with:
- Dangerous predicates blocked (shell, system, call, assert, retract, abolish, halt)
- File operations restricted to configured root directories
- Pre-execution validation of all queries and assertions

### File Access Control

- **Secure by Default:** File operations disabled without explicit configuration
- **Configuration Required:** Via MCP client roots or `SWI_MCP_ALLOWED_ROOTS` environment variable
- **Validation:** All file paths validated before execution

### Predicate Blocking

Dangerous predicates are detected and blocked before execution:
- `call/1` - Prevents arbitrary code execution
- `assert/1`, `retract/1`, `abolish/1` - Use knowledge_base tools instead
- `system/1`, `shell/1` - No system command execution
- `halt/1` - Prevents server termination

Error format: `Security Error: Operation blocked - contains dangerous predicate 'X'`

## Performance Considerations

### Query Timeouts

- Default: 30 seconds per query
- Configurable via `SWI_MCP_QUERY_TIMEOUT_MS` environment variable
- Circuit breaker activates after consecutive timeouts
- Automatic recovery with process restart

### Resource Management

- Knowledge base persists across queries within session
- Query results are stateful (Standard Mode) or engine-based (Engine Mode)
- Always call `query_close` to free resources
- Use `knowledge_base_clear` to reset between different problem domains

### Best Practices

- Use `knowledge_base_assert_many` for batch operations (more efficient)
- Choose appropriate query mode (Standard for simple queries, Engine for complex backtracking)
- Close queries promptly to free resources
- Monitor token usage when reading large resources
