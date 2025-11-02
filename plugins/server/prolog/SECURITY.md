# Security Policy - @vpursuit/mcp-server-prolog

SWI-Prolog integration plugin for Model Context Protocol servers.

## Overview

This package provides Prolog execution capabilities for MCP servers. While it implements comprehensive security measures, executing user-provided Prolog code always carries inherent risks.

## Threat Model

**Primary Risks:**
- Arbitrary Prolog code execution
- File system access attempts
- Operating system command execution
- Resource exhaustion (CPU, memory)
- Information disclosure through queries

**Trust Boundaries:**
- MCP clients are partially trusted (provide queries)
- Prolog files are untrusted (must be validated)
- SWI-Prolog process is sandboxed

## Built-in Security Features

### 1. Dangerous Predicate Blocking

Pre-execution validation blocks dangerous predicates before they reach the Prolog interpreter:

**Blocked Predicates:**
- `shell()`, `system()` - OS command execution
- `call()` - Arbitrary goal execution
- `assert()`, `retract()`, `abolish()` - Direct database modification
- `halt()` - Process termination
- `open()`, `close()`, `read()`, `write()` - Direct file I/O

**Example:**
```prolog
% This is blocked before execution
malware :- shell('rm -rf /').
% Returns: Security Error: Operation blocked - contains dangerous predicate 'shell'
```

### 2. Library(sandbox) Integration

All predicates are validated through SWI-Prolog's `library(sandbox)`:
- Built-in predicates classified as safe/unsafe
- Unsafe goals rejected during execution
- Module system enforces isolation

### 3. Safe Library Module Loading

**NEW:** The server now supports loading SWI-Prolog standard libraries that are sandbox-approved:

**Allowed Libraries** (whitelisted):
- `library(clpfd)` - Constraint Logic Programming over Finite Domains
- `library(clpb)` - Boolean constraint solving
- `library(lists)`, `library(apply)`, `library(aggregate)` - List and data operations
- `library(assoc)`, `library(pairs)`, `library(ordsets)` - Data structures
- `library(rbtrees)`, `library(heaps)`, `library(ugraphs)` - Advanced data structures
- And other pure, side-effect-free libraries (see prolog_server.pl for full list)

**Blocked Libraries:**
- `library(process)` - System process execution
- `library(filesex)` - Extended file operations
- `library(http/*)` - Network operations
- `library(unix)` - Unix system calls
- Any library not explicitly whitelisted

**Security Model:**
- Two-pass loading: directives processed before code parsing
- Libraries loaded into isolated `knowledge_base` module
- Only `use_module(library(...))` directives allowed
- User module paths blocked (e.g., `use_module('/path/to/file.pl')`)
- Libraries validated against sandbox safety declarations

**Example:**
```prolog
% In ~/.swipl-mcp-server/puzzle.pl
:- use_module(library(clpfd)).  % ✓ Allowed

solve_sudoku(Rows) :-
    append(Rows, Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    % ... additional constraints
    label(Vs).

% Blocked examples:
:- use_module(library(process)).      % ✗ Blocked - unsafe
:- use_module('/tmp/malicious.pl').   % ✗ Blocked - not library(...)
```

### 4. File Path Restrictions

File operations restricted to allowed directories:
- Default: `~/.swipl-mcp-server/`
- System directories blocked: `/etc`, `/usr`, `/bin`, `/var`, etc.
- Path traversal protection
- Configurable via `SWI_MCP_ALLOWED_ROOTS`

### 5. Module Isolation

User code runs in dedicated `knowledge_base` module:
- Prevents pollution of system modules
- `unknown=fail` policy (undefined predicates fail safely)
- Clear separation from server internals

### 6. Timeout Protection

Query execution has configurable timeouts:
- Default: 30 seconds per query
- Prevents infinite loops and resource exhaustion
- Configurable via `SWI_MCP_QUERY_TIMEOUT_MS`

### 7. Prolog Server Script Security

The `prolog_server.pl` script:
- Runs in isolated child process
- No network access by default
- Limited to stdio communication
- Validated during startup

## Usage Security Guidelines

### For Server Implementers

**1. Validate Input:**
```typescript
import { toolHandlers } from '@vpursuit/mcp-server-prolog';

// Input is validated automatically by tool handlers
await toolHandlers.knowledge_base_assert({
  fact: userProvidedFact  // Dangerous predicates blocked
});
```

**2. Set Conservative Timeouts:**
```typescript
// In environment or MCP client config
process.env.SWI_MCP_QUERY_TIMEOUT_MS = '15000'; // 15 seconds
```

**3. Restrict File Access:**
```typescript
// Only allow specific directories
process.env.SWI_MCP_ALLOWED_ROOTS = '/app/data,/app/uploads';
process.env.SWI_MCP_STRICT_ROOTS = 'true'; // No fallback directory
```

**4. Handle Errors Gracefully:**
```typescript
try {
  const result = await toolHandlers.query_start({ query: userQuery });
} catch (error) {
  // Log but don't expose internal details to users
  console.error('Query failed:', error.message);
  return { error: 'Query execution failed' };
}
```

### For Library Users

**Safe Predicates** (allowed):
- Arithmetic: `is/2`, `</2`, `>/2`, `=:=/2`
- Lists: `append/3`, `member/2`, `length/2`, `sort/2`
- Meta: `findall/3`, `bagof/3`, `setof/3`
- Logic: `true/0`, `false/0`, `!/0`, `;/2`, `,/2`

**Unsafe Predicates** (blocked):
- File I/O: `open/3`, `see/1`, `tell/1`, `consult/1`
- System: `shell/1`, `system/1`, `halt/0`, `abort/0`
- Dynamic: `assert/1`, `retract/1` (use tool handlers instead)
- Execution: `call/1`, `catch/3` (without validation)

**Example - Safe Usage:**
```prolog
% Safe: arithmetic and list operations
calculate(X, Y) :- X is 2 + 3, Y = [1,2,3].

% Safe: recursive definitions
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

% Unsafe: will be blocked
dangerous :- shell('cat /etc/passwd').
```

## Configuration

**Environment Variables:**
- `SWI_MCP_QUERY_TIMEOUT_MS`: Query timeout (default: 30000)
- `SWI_MCP_READY_TIMEOUT_MS`: Startup timeout (default: 5000)
- `SWI_MCP_ALLOWED_ROOTS`: Comma-separated allowed directories
- `SWI_MCP_STRICT_ROOTS`: Set to `"true"` to disable fallback directory
- `SWI_MCP_PROLOG_PATH`: Override Prolog server script path
- `DEBUG`: Set to `"mcp-prolog"` for debug logging

**Security Note:** These settings cannot disable security features. Dangerous predicate blocking and sandbox validation are always active.

## Reporting Vulnerabilities

If you discover a security vulnerability in @vpursuit/mcp-server-prolog:

1. **Do not** open a public GitHub issue
2. Use [GitHub Security Advisories](https://github.com/vpursuit/swipl-mcp-server/security/advisories/new) for private reporting
3. Include:
   - Affected version
   - Proof of Concept (minimal Prolog code)
   - Expected vs. actual behavior
   - Impact assessment

See the [root SECURITY.md](../../SECURITY.md) for ecosystem-wide security policy.

## Testing Security

**Test Dangerous Predicate Blocking:**
```typescript
// Should fail with security error
await toolHandlers.knowledge_base_assert({
  fact: "malware :- shell('rm -rf /')"
});
// Expected: Security Error: Operation blocked - contains dangerous predicate 'shell'
```

**Test File Path Restrictions:**
```typescript
// Should fail with security error
await toolHandlers.knowledge_base_load({
  filename: "/etc/passwd"
});
// Expected: Security Error: Access to system directories is blocked
```

**Test Safe Operations:**
```typescript
// Should succeed
await toolHandlers.knowledge_base_assert({
  fact: "parent(john, mary)"
});

await toolHandlers.query_start({
  query: "X is 2 + 3"
});
// Expected: X = 5
```

## Resources

- [SWI-Prolog Sandbox Documentation](https://www.swi-prolog.org/pldoc/man?section=sandbox)
- [Root Security Policy](../../SECURITY.md)
- [Package README](./README.md)
- [Model Context Protocol](https://modelcontextprotocol.io)

## License

BSD-3-Clause - See [LICENSE](./LICENSE) file for details.
