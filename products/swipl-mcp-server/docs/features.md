# Features Reference

Complete reference for all SWI-Prolog MCP Server features including prompts, resources, and tools.

## MCP Prompts

The server provides 5 expert Prolog assistance prompts that follow a resource-first approach, guiding AI agents to discover and use server capabilities efficiently.

### Slash Command Access

In Claude Code CLI and Codex, these prompts are available as convenient slash commands. Simply type `/swipl` to see all available commands:

![SWI-Prolog slash commands in Claude Code CLI](../images/swipl-slash-commands.png)

This provides an intuitive way to access expert Prolog assistance without having to remember exact prompt names or syntax.

### `prolog_init_expert`

Initialize expert Prolog assistance mode with comprehensive knowledge and optional task focus.

**Arguments:**
- `task` (optional, string) - Specific task to focus expert setup and reasoning

**Purpose:**
Establishes expert context with comprehensive Prolog knowledge covering:
- SWI-Prolog syntax, unification, and DCGs
- Logic programming paradigms and best practices
- Built-in predicates and meta-programming
- Query optimization techniques
- Security awareness of server constraints

**Example Usage:**
```json
{
  "name": "prolog_init_expert",
  "arguments": {
    "task": "family relationship modeling"
  }
}
```

### `prolog_quick_reference`

Get comprehensive server overview and capabilities orientation.

**Arguments:** None

**Purpose:**
Provides complete guidance on:
- Available tools and their usage patterns
- Security model and file restrictions
- Session state management
- Resource discovery and access
- Best practices for token-efficient interaction

**Example Usage:**
```json
{
  "name": "prolog_quick_reference",
  "arguments": {}
}
```

### `prolog_analyze_knowledge_base`

Analyze current knowledge base state and structure using available resources.

**Arguments:** None

**Purpose:**
Performs comprehensive analysis of:
- Current predicates and their relationships
- Knowledge base structure and dependencies
- Data integrity and consistency
- Query optimization opportunities
- Suggested improvements or extensions

**Example Usage:**
```json
{
  "name": "prolog_analyze_knowledge_base",
  "arguments": {}
}
```

### `prolog_knowledge_base_builder`

Build domain-specific knowledge bases with guided construction.

**Arguments:**
- `domain` (required, string) - Domain to model (e.g., "family relationships", "expert system")

**Purpose:**
Guides systematic knowledge base construction:
- Domain modeling best practices
- Predicate design patterns
- Fact and rule organization
- Testing and validation strategies
- Documentation and maintainability

**Example Usage:**
```json
{
  "name": "prolog_knowledge_base_builder",
  "arguments": {
    "domain": "medical diagnosis expert system"
  }
}
```

### `prolog_query_optimizer`

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
  "name": "prolog_query_optimizer",
  "arguments": {
    "query": "ancestor(X, Y) :- parent(X, Y). ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)."
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
```
predicate_name/arity
another_predicate/2
fact_name/1
```

**Usage:**
Access current predicates to understand knowledge base structure before analysis or querying.

#### `prolog://knowledge_base/dump`

**URI:** `prolog://knowledge_base/dump`
**MIME Type:** `text/prolog`
**Type:** Dynamic

Complete export of the knowledge base as Prolog clauses.

**Content Format:**
```prolog
% Exported knowledge base
parent(john, mary).
parent(mary, alice).
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

**Usage:**
Export current knowledge base for backup, analysis, or transfer to other systems.

### Reference Resources

#### `reference://help`

**URI:** `reference://help`
**MIME Type:** `text/plain`
**Type:** Static

Usage guidelines and server tips.

**Content:**
Comprehensive help covering:
- Tool usage patterns
- Security considerations
- Best practices
- Troubleshooting guidance
- Session management

#### `reference://license`

**URI:** `reference://license`
**MIME Type:** `text/plain`
**Type:** Static

BSD-3-Clause license text for the software.

#### `reference://capabilities`

**URI:** `reference://capabilities`
**MIME Type:** `application/json`
**Type:** Dynamic

Machine-readable summary of server capabilities.

**Content Format:**
```json
{
  "tools": {
    "count": 13,
    "categories": ["core", "knowledge_base", "query", "symbols"]
  },
  "prompts": {
    "count": 5,
    "available": ["prolog_init_expert", "prolog_quick_reference", ...]
  },
  "resources": {
    "count": 5,
    "available": ["prolog://knowledge_base/predicates", ...]
  },
  "security": {
    "file_restrictions": "~/.model-context-lab/ only",
    "dangerous_predicates_blocked": true,
    "sandbox_validation": true
  },
  "modes": {
    "available": ["standard", "engine"],
    "description": "Standard pagination and true backtracking"
  }
}
```

## Tools Reference

### Core Tools

#### `help`

Get usage guidelines and tips for the server.

**Arguments:**
- `topic` (optional) - Specific help topic: "overview", "standard_mode", "engine_mode", "safety", "security", "examples", "prompts", "troubleshooting"

#### `license`

Get the BSD-3-Clause license text.

**Arguments:** None

#### `capabilities`

Get machine-readable summary of tools, modes, environment, and safety features.

**Arguments:** None

### Knowledge Base Tools

#### `knowledge_base_load`

Load a Prolog file into the knowledge base.

**Arguments:**
- `filename` (required, string) - Path to Prolog file (must be in `~/.model-context-lab/`)

**Security:** File path validation ensures access only to allowed directory.

#### `knowledge_base_assert`

Add a single clause (fact or rule) to the knowledge base.

**Arguments:**
- `fact` (required, string) - Prolog clause to assert

**Examples:**
```json
{"fact": "parent(john, mary)"}
{"fact": "grandparent(X,Z) :- parent(X,Y), parent(Y,Z)"}
```

#### `knowledge_base_assert_many`

Add multiple clauses to the knowledge base.

**Arguments:**
- `facts` (required, array of strings) - List of Prolog clauses to assert

#### `knowledge_base_retract`

Remove a single clause from the knowledge base.

**Arguments:**
- `fact` (required, string) - Prolog clause to retract

#### `knowledge_base_retract_many`

Remove multiple clauses from the knowledge base.

**Arguments:**
- `facts` (required, array of strings) - List of Prolog clauses to retract

#### `knowledge_base_clear`

Remove ALL user-defined facts and rules from the knowledge base.

**Arguments:** None

**Warning:** This operation cannot be undone. Use `knowledge_base_dump` to backup before clearing.

#### `knowledge_base_dump`

Export current knowledge base as Prolog facts and rules.

**Arguments:** None

**Returns:** Complete knowledge base in Prolog syntax for backup or analysis.

### Query Tools

#### `query_start`

Start a new query session using standard mode (deterministic pagination via `call_nth/2`).

**Arguments:**
- `query` (required, string) - Prolog query to execute

**Session Management:** Only one session type (query or engine) can be active at a time.

#### `query_startEngine`

Start a new query session using engine mode (true backtracking via SWI-Prolog engines).

**Arguments:**
- `query` (required, string) - Prolog query to execute

**Advantages:** True backtracking, better for complex queries with many solutions.

#### `query_next`

Get the next solution from the current query session.

**Arguments:** None

**Returns:** Next solution or "No more solutions available" when exhausted.

#### `query_close`

Close the current query session and clean up resources.

**Arguments:** None

**Important:** Always close sessions to free resources and allow new sessions to start.

### Symbol Tools

#### `symbols_list`

List predicates available in the knowledge base.

**Arguments:** None

**Returns:** List of currently defined predicates with their arities.

## Session State Management

The server maintains session state with mutual exclusion between query modes:

### States
- `idle` - No active session
- `query` - Standard query session active
- `engine` - Engine query session active
- `query_completed` - Query exhausted, awaiting close
- `engine_completed` - Engine exhausted, awaiting close
- `closing_query` - Query session closing
- `closing_engine` - Engine session closing

### Transitions
- Only one session type can be active at a time
- Sessions must be explicitly closed before starting new ones
- Completed sessions keep context until closed
- Invalid transitions are logged when `SWI_MCP_TRACE=1`

## Security Model

### File Path Restrictions
- **Allowed:** `~/.model-context-lab/` directory only
- **Blocked:** System directories (`/etc`, `/usr`, `/bin`, `/var`, etc.)
- **Validation:** Pre-execution path validation with realpath resolution

### Dangerous Predicate Blocking
Pre-execution detection and blocking of dangerous operations:
- `shell()` - System command execution
- `system()` - Operating system calls
- `call()` - Dynamic predicate calls
- `halt()` - System termination
- `assert()` - Direct database modification (use knowledge_base tools)

### Additional Protections
- **Sandbox Validation:** Uses `library(sandbox)` for built-in predicate validation
- **Timeout Protection:** Configurable timeouts prevent infinite loops
- **Module Isolation:** Knowledge base runs in dedicated `knowledge_base` module
- **Input Validation:** Query length limits and syntax validation

## Best Practices

### Resource-First Approach
1. **Discover capabilities** using `reference://capabilities` resource
2. **Read current state** using knowledge base resources
3. **Plan actions** based on discovered information
4. **Execute efficiently** with appropriate tools

### Session Management
1. **Use appropriate mode** - Standard for simple queries, Engine for complex backtracking
2. **Close sessions explicitly** - Always call `query_close` when done
3. **Handle errors gracefully** - Sessions are automatically cleaned up on errors
4. **Monitor state** - Check session state before starting new operations

### Performance Optimization
1. **Optimize predicate order** - Place most selective predicates first
2. **Use cuts judiciously** - Balance efficiency with correctness
3. **Batch operations** - Use `assert_many` for multiple clauses
4. **Export for backup** - Use `knowledge_base_dump` before major changes

### Security Awareness
1. **Validate file paths** - Ensure files are in allowed directory
2. **Review clauses** - Check for dangerous predicates before asserting
3. **Limit query complexity** - Use timeouts for complex operations
4. **Monitor resource usage** - Export and analyze knowledge base regularly