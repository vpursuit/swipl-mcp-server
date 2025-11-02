# mcp-prolog Architecture

## Overview

The `mcp-prolog` package implements a secure, sandboxed SWI-Prolog execution environment for MCP servers. The architecture uses a **two-module design** to enforce security boundaries while enabling dynamic library loading for advanced features like CLP(FD).

## Two-Module Architecture

### Core Modules

```
┌────────────────────────────────────────────────────────────┐
│                    prolog_server                           │
│                  (Trusted Control Layer)                   │
│                                                             │
│  • Receives commands from TypeScript                       │
│  • Parses query strings                                    │
│  • Validates security constraints                          │
│  • Dispatches to execution layer                           │
└────────────────────────────────────────────────────────────┘
                          ↓
                   (validated queries)
                          ↓
┌────────────────────────────────────────────────────────────┐
│                   knowledge_base                           │
│                 (Isolated Execution Layer)                 │
│                                                             │
│  • Stores user-defined predicates                          │
│  • Executes validated queries                              │
│  • Provides sandbox isolation                              │
│  • Returns results                                         │
└────────────────────────────────────────────────────────────┘
```

### Module Responsibilities

| Module | Purpose | Contains |
|--------|---------|----------|
| **prolog_server** | Command & Control | Dispatch logic, security validation, query parsing |
| **knowledge_base** | Code Execution | User predicates, imported library predicates |

### Why Two Modules?

**Security Isolation:**
- User code executes in isolated `knowledge_base` module
- Server infrastructure stays in protected `prolog_server` module
- Clear trust boundary between external input and execution

**Separation of Concerns:**
- Control logic separate from execution logic
- Parser in trusted context prevents operator injection attacks
- Validation happens before entering untrusted space

## Operators vs Predicates

Understanding the distinction is critical to the architecture:

### Operators

**Definition:** Syntax rules that control how text is parsed into terms.

**Example:**
```prolog
% #= is an infix operator with precedence 700
X #= 1   % Parses as: #=(X, 1)
```

**Scope:** Module-specific! Each module maintains its own operator table.

**When Needed:** During parsing (`read_term_from_atom`, `read_file_to_terms`)

### Predicates

**Definition:** Executable code that performs operations.

**Example:**
```prolog
% #=/2 is a predicate that creates CLP(FD) constraints
#=(X, 1)  % Executes the constraint
```

**Scope:** Module-specific! Each module has its own predicate definitions.

**When Needed:** During execution (`engine_create`, `call`)

## Query Processing Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. TypeScript Client                                            │
│    queryStart({ query: "X #= 1, X #> 0" })                     │
└─────────────────────────────────────────────────────────────────┘
                           ↓ (JSON-RPC over stdio)
┌─────────────────────────────────────────────────────────────────┐
│ 2. PrologInterface (Node.js)                                    │
│    Sends: cmd(123, start_engine_string("X #= 1, X #> 0"))      │
└─────────────────────────────────────────────────────────────────┘
                           ↓ (IPC pipe)
┌─────────────────────────────────────────────────────────────────┐
│ 3. prolog_server Module - PARSING PHASE                        │
│                                                                  │
│    read_term_from_atom("X #= 1, X #> 0", Query, [...])         │
│                                                                  │
│    ⚠️  Uses prolog_server's operator table                      │
│    ⚠️  Must have #= and #> operators defined HERE               │
│                                                                  │
│    Result: Query = (#=(X,1), #>(X,0))                          │
└─────────────────────────────────────────────────────────────────┘
                           ↓ (parsed term)
┌─────────────────────────────────────────────────────────────────┐
│ 4. prolog_server Module - VALIDATION PHASE                     │
│                                                                  │
│    safe_goal_ok(knowledge_base:(#=(X,1), #>(X,0)))            │
│                                                                  │
│    Checks:                                                       │
│    • No explicitly dangerous predicates (shell, etc.)           │
│    • Passes sandbox:safe_goal/1 validation                     │
│    • Structure is safe per body_safe/1                         │
│                                                                  │
│    ✓ Approved for execution                                    │
└─────────────────────────────────────────────────────────────────┘
                           ↓ (validated term)
┌─────────────────────────────────────────────────────────────────┐
│ 5. knowledge_base Module - EXECUTION PHASE                     │
│                                                                  │
│    engine_create(VarNames, knowledge_base:(#=(X,1), #>(X,0)), E)│
│                                                                  │
│    ⚠️  Uses knowledge_base's predicate table                    │
│    ⚠️  Must have #=/2 and #>/2 predicates defined HERE          │
│                                                                  │
│    Executes constraints, returns bindings                       │
└─────────────────────────────────────────────────────────────────┘
                           ↓ (results)
┌─────────────────────────────────────────────────────────────────┐
│ 6. Back to TypeScript                                           │
│    { solution: "X = 1" }                                        │
└─────────────────────────────────────────────────────────────────┘
```

## Safe Library Loading

### The Challenge

When users load files that import libraries like `library(clpfd)`:

```prolog
:- use_module(library(clpfd)).

solve(X) :- X ins 1..10, X #> 5.
```

We need:
1. Operators in **prolog_server** for parsing query strings
2. Operators in **knowledge_base** for parsing file contents
3. Predicates in **knowledge_base** for executing queries

### Two-Pass Loading Mechanism

**Pass 1: Process Directives**
```prolog
safe_consult_pass1_directives(File) :-
    % Read file term by term
    % For each :- use_module(library(LibName)):
    %   1. Validate LibName is on whitelist
    %   2. Load into knowledge_base (for execution + file parsing)
    %   3. Load into prolog_server (for query parsing)
```

**Pass 2: Load Code**
```prolog
% Read all terms with knowledge_base operators available
read_file_to_terms(File, Terms, [module(knowledge_base)]),
% Assert into knowledge_base module
maplist(assert_knowledge_base_term_safe, Terms).
```

### Why Load into BOTH Modules?

| Parsing Location | Module Context | Purpose | Operators Needed |
|------------------|----------------|---------|------------------|
| **Query strings** | prolog_server | TypeScript queries | ✅ Must load into prolog_server |
| **File contents** | knowledge_base | User Prolog files | ✅ Must load into knowledge_base |

**Example:**

```prolog
% In safe_consult_pass1_process_term:
safe_consult_pass1_process_term((:- use_module(library(clpfd)))) :- !,
    % For execution AND file parsing:
    knowledge_base:use_module(library(clpfd)),
    % For query parsing:
    catch(use_module(library(clpfd)), _, true).
```

### Whitelisted Libraries

Only sandbox-safe libraries are allowed:

```prolog
library_safe_to_load(clpfd).      % Constraint Logic Programming
library_safe_to_load(clpb).       % Boolean constraints
library_safe_to_load(lists).      % List manipulation
library_safe_to_load(apply).      % maplist, include, etc.
library_safe_to_load(aggregate).  % Aggregation
% ... 18 total libraries
```

**Blocked Libraries:**
- `library(process)` - OS command execution
- `library(filesex)` - File system operations
- `library(http/*)` - Network operations
- Any non-library paths

## Security Architecture

### Defense in Depth

**Layer 1: Parsing in Trusted Context**
- Query strings parsed in `prolog_server`
- User cannot inject custom operators into query parser
- Prevents operator-based syntax attacks

**Layer 2: Pre-Execution Validation**
```prolog
safe_goal_ok(knowledge_base:Query) :-
    % 1. Check explicit blacklist
    \+ explicitly_dangerous(Query),
    % 2. Validate structure is safe
    body_safe(Query),
    % 3. Defer to library(sandbox)
    sandbox:safe_goal(Query).
```

**Layer 3: Module Isolation**
- User code executes in `knowledge_base` module
- Cannot access `prolog_server` internals
- Unknown predicates fail harmlessly

**Layer 4: Resource Limits**
- Query timeouts (default 30s)
- Buffer size limits (1MB)
- Process isolation

### Operator Injection Prevention

**Attack Scenario (if parsing happened in knowledge_base):**

```prolog
% Malicious file:
:- op(1200, fx, safe).
safe(Goal) :- Goal.  % Wrapper that bypasses checks

% User sends query: "safe shell('rm -rf /')"
% Would parse as: safe(shell(...))
% Might bypass security checks!
```

**Current Architecture (Secure):**

```prolog
% prolog_server does NOT have 'safe' operator
% Query "safe shell(...)" fails to parse
% Attack prevented before validation even runs
```

### Why Loading Safe Libraries is Secure

When we load `library(clpfd)` into `prolog_server`:

1. ✅ Library is on explicit whitelist
2. ✅ We trust SWI-Prolog's clpfd operators
3. ✅ Operators only affect parsing, not what's allowed
4. ✅ Security validation still checks parsed terms
5. ✅ No sandbox bypass possible

The operator `#=` affects HOW `"X #= 1"` parses, not WHETHER it's safe.

## File Loading Flow

```
User File: test.pl
┌─────────────────────────────────────────────────────────────┐
│ :- use_module(library(clpfd)).                              │
│ solve(X) :- X ins 1..10, X #> 5.                           │
└─────────────────────────────────────────────────────────────┘
                        ↓
            ┌──────────────────────┐
            │ Pass 1: Directives   │
            └──────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────────┐
│ safe_consult_pass1_directives(test.pl)                     │
│                                                              │
│ Reads term by term until EOF:                               │
│   Found: :- use_module(library(clpfd))                     │
│                                                              │
│   1. Validate: library_safe_to_load(clpfd) ✓               │
│   2. Execute: knowledge_base:use_module(library(clpfd))    │
│      → Loads operators + predicates into knowledge_base     │
│   3. Execute: use_module(library(clpfd))                   │
│      → Loads operators + predicates into prolog_server     │
└─────────────────────────────────────────────────────────────┘
                        ↓
            ┌──────────────────────┐
            │ Pass 2: Code Loading │
            └──────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────────┐
│ read_file_to_terms(test.pl, Terms,                         │
│                    [module(knowledge_base)])                │
│                                                              │
│ Parses: "X ins 1..10, X #> 5"                              │
│   ✓ Works! knowledge_base has ins and #> operators         │
│                                                              │
│ Terms = [                                                    │
│   (:- use_module(library(clpfd))),   % Skipped in pass 2   │
│   (solve(X) :- X ins 1..10, X #> 5)  % Asserted            │
│ ]                                                            │
└─────────────────────────────────────────────────────────────┘
                        ↓
┌─────────────────────────────────────────────────────────────┐
│ maplist(assert_knowledge_base_term_safe, Terms)            │
│                                                              │
│ For directive: already processed, skip                      │
│ For rule: assertz(knowledge_base:(solve(X) :- ...))        │
└─────────────────────────────────────────────────────────────┘
```

## Common Questions

### Q: Why not just parse everything in knowledge_base?

**A:** Security. If parsing happened in `knowledge_base`:
- User-defined operators would affect query parsing
- Operator injection attacks become possible
- Parser state would mix with execution state
- Validation would happen AFTER entering untrusted context

### Q: Isn't loading libraries into prolog_server a security risk?

**A:** No, because:
- Only whitelisted libraries are allowed
- We trust SWI-Prolog's standard libraries
- Operators are syntax rules, not execution
- Security validation still checks the PARSED terms
- No sandbox bypass is possible

### Q: What about the `module(knowledge_base)` option in read_file_to_terms?

**A:** This option only affects parsing, not execution:
- Tells parser to use knowledge_base's operators
- Does NOT execute directives
- Does NOT import into the module
- Just provides parsing context

### Q: Why was the old `knowledge_base:maplist` guard removed?

**A:** It was obsolete:
- Created before dynamic library loading was possible
- Now that we load `library(apply)` into knowledge_base, the real predicates work
- Guard was overriding the real imports and causing failures
- Removing it lets the proper library predicates work

## Design Principles

1. **Security First**: Validation before execution, isolation between trust boundaries
2. **Module Isolation**: Clear separation between control and execution
3. **Defense in Depth**: Multiple security layers (parsing, validation, sandbox, isolation)
4. **Predictable Behavior**: Controlled operator sets, explicit whitelists
5. **User Empowerment**: Safe libraries enabled while maintaining security

## See Also

- [SECURITY.md](./SECURITY.md) - Comprehensive security documentation
- [prolog_server.pl](./prolog/prolog_server.pl) - Server implementation
- [PrologInterface.ts](./src/PrologInterface.ts) - Node.js interface
