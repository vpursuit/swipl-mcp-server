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
    "core": ["help", "license", "capabilities"],
    "knowledge_base": [...],
    "query": [...],
    "symbols": [...]
  },
  "prompts": {
    "expert_guidance": ["expert", "optimize"],
    "knowledge_base": ["knowledge"],
    "problem_solving": ["puzzle"]
  },
  "security": {...}
}
```

**Use Case:** Programmatic capability discovery, automated documentation generation, or validation of server features.

## MCP Tools

Complete reference of all available tools organized by category.

### Core Tools

#### `help`

Get comprehensive usage guidelines and best practices.

**Arguments:**
- `topic` (optional) - Specific help topic: "overview", "standard_mode", "engine_mode", "safety", "security", "examples", "prompts", "troubleshooting"

**Returns:** Detailed help text for the requested topic or general overview.

#### `license`

Get the BSD-3-Clause license text.

**Arguments:** None

**Returns:** Full license text.

#### `capabilities`

Get machine-readable server capabilities summary.

**Arguments:** None

**Returns:** JSON object with complete capability information (see `reference://capabilities` above).

### Knowledge Base Management Tools

#### `knowledge_base_load`

Load Prolog facts and rules from a file into the knowledge_base module.

**Arguments:**
- `filename` (required, string) - Path to Prolog file (must be within configured roots)

**Security:** File must be in allowed directory (see Configuration section).

**Behavior:**
- Consults the file into the knowledge_base module
- Only accepts facts and rules (directives are rejected for security)
- Validates all predicates against security constraints
- Fails if file contains dangerous predicates

#### `knowledge_base_assert`

Add a single Prolog fact or rule to the knowledge_base.

**Arguments:**
- `fact` (required, string) - Prolog clause to assert (e.g., "parent(john, mary)" or "ancestor(X,Z) :- parent(X,Y), parent(Y,Z)")

**Example:**
```json
{
  "fact": "parent(john, mary)"
}
```

#### `knowledge_base_assert_many`

Add multiple Prolog facts or rules in a single operation (more efficient than individual assertions).

**Arguments:**
- `facts` (required, array of strings) - List of Prolog clauses to assert

**Example:**
```json
{
  "facts": [
    "parent(john, mary)",
    "parent(mary, susan)",
    "ancestor(X,Y) :- parent(X,Y)",
    "ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z)"
  ]
}
```

#### `knowledge_base_retract`

Remove a single fact or rule from the knowledge_base.

**Arguments:**
- `fact` (required, string) - Prolog clause to retract

#### `knowledge_base_retract_many`

Remove multiple facts or rules in a single operation.

**Arguments:**
- `facts` (required, array of strings) - List of Prolog clauses to retract

#### `knowledge_base_clear`

Remove all facts and rules from the knowledge_base module.

**Arguments:** None

**Use Case:** Reset to clean state before loading new knowledge.

#### `knowledge_base_dump`

Export all current knowledge base content as Prolog source code.

**Arguments:** None

**Returns:** Complete Prolog module source with all facts and rules.

#### `knowledge_base_load_library`

Load a safe SWI-Prolog library into the knowledge_base module.

**Arguments:**
- `library` (required, string) - Library name (e.g., "clpfd", "lists", "apply")

**Security:** Only sandbox-approved libraries are allowed. See [SECURITY.md](../../../SECURITY.md#swi-prolog-mcp-server-security) for complete security documentation.

### Query Tools

#### `query_start`

Start a deterministic query with pagination support (Standard Mode).

**Arguments:**
- `query` (required, string) - Prolog query to execute

**Behavior:**
- Finds first solution
- Use `query_next` to get subsequent solutions
- Automatically limits solutions to prevent infinite loops
- Must call `query_close` when done

#### `query_next`

Get next solution from active query (Standard Mode).

**Arguments:** None

**Returns:** Next solution or indication that no more solutions exist.

**Session State:** Query must be active from `query_start`.

#### `query_close`

Close active query and free resources (Standard Mode).

**Arguments:** None

#### `query_startEngine`

Start a query using true Prolog backtracking engine (Engine Mode).

**Arguments:**
- `query` (required, string) - Prolog query to execute

**Behavior:**
- Creates Prolog engine for true backtracking
- Use `query_next` to iterate through solutions
- More powerful than Standard Mode but requires careful resource management
- Must call `query_close` when done

**Use Case:** Complex queries requiring full backtracking semantics, multiple solution paths, or constraint solving.

### Symbol Inspection Tools

#### `symbols_list`

List all predicates currently defined in the knowledge_base module.

**Arguments:** None

**Returns:** Array of predicate indicators (e.g., ["parent/2", "ancestor/2"]).

**Use Case:** Verify what predicates are available, check if predicate exists before defining, or explore knowledge base structure.

## Session State Management

The server maintains session state to prevent conflicts between query modes:

- **idle**: No active query
- **query**: Standard mode query active (from `query_start`)
- **engine**: Engine mode query active (from `query_startEngine`)
- **query_completed**: Query finished but not closed
- **engine_completed**: Engine finished but not closed

**Mutual Exclusion:** Cannot start Standard Mode query while Engine Mode is active and vice versa. Must call `query_close` to switch modes.

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
