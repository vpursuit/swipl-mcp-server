# SWI-Prolog MCP Server - Example Queries

A collection of practical examples for using the SWI-Prolog MCP Server.

## Inspector vs Production Usage

This document demonstrates **inspector usage** where you manually call each tool individually. This is useful for:
- Testing and debugging the MCP server
- Learning how the tools work
- Interactive exploration of Prolog knowledge bases

In **production usage**, an AI agent automatically calls these tools based on natural language requests. For example:
- You: "Find all parents of Mary in the family data"
- AI agent: Automatically calls `query` with `operation: "start"`, then `operation: "next"` (multiple times), then `operation: "close"`

The JSON examples below show the exact tool calls that happen behind the scenes in both scenarios.

## Resources and Prompts

Beyond the interactive tools, this MCP server also provides:

### Resources
Static documentation accessible via MCP resource URIs:
- `swipl://docs/features` - Complete feature documentation
- `swipl://docs/installation` - Installation and setup guide

In production, AI agents can read these resources to understand server capabilities and provide better assistance.

### Expert Prompts
Four specialized prompts for Prolog development assistance:
- `genealogy` - Build and query family trees using relational logic
- `scheduling` - Schedule tasks with dependencies using CLP(FD)
- `puzzle` - Solve logic puzzles using constraint programming
- `grammar` - Parse natural language using Definite Clause Grammars (DCGs)

These prompts help AI agents provide more specialized Prolog expertise when working with your knowledge bases.

## Setup

1. Build server and start inspector:
```bash
npm run build
npx @modelcontextprotocol/inspector --transport stdio node dist/index.js
```

2. Browser opens automatically with the Inspector interface

## Basic Examples

### 1. Load Test File

**Tool:** `files`
```json
{
  "operation": "import",
  "filename": "test.pl"
}
```

**Expected Response:** Confirmation that the file was loaded

### 2. List Available Predicates

**Tool:** `workspace`
```json
{
  "operation": "list_symbols"
}
```

**Expected Response:** List of all available predicates (e.g. `["parent", "male", "ancestor"]`)

## Family Data Examples

### 3. Find All Parents of Mary

**Tool:** `query`
```json
{
  "operation": "start",
  "operation": "start",
  "query": "parent(X, mary)"
}
```

**Tool:** `query`
```json
{
  "operation": "next"
}
```
**Expected Response:** First parent (e.g., "X = john")

**Tool:** `query`
```json
{
  "operation": "next"
}
```
**Expected Response:** Next parent or "no more solutions"

**Tool:** `query`
```json
{
  "operation": "close"
}
```

### 4. Step-by-Step List Member Query

**Tool:** `query`
```json
{
  "operation": "start",
  "operation": "start",
  "query": "member(X, [apple, banana, cherry])"
}
```

**Tool:** `query`
```json
{
  "operation": "next"
}
```
**Response:** "X = apple"

**Tool:** `query`
```json
{
  "operation": "next"
}
```
**Response:** "X = banana"

**Tool:** `query`
```json
{
  "operation": "next"
}
```
**Response:** "X = cherry"

**Tool:** `query`
```json
{
  "operation": "close"
}
```

## Advanced Examples

### 5. Add New Family Relations (Single Facts)

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "parent(alice, bob)"
}
```

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "parent(bob, charlie)"
}
```

### 5b. Add Multiple Family Relations at Once

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": [
    "parent(alice, bob)",
    "parent(bob, charlie)",
    "parent(charlie, david)",
    "male(bob)",
    "male(charlie)",
    "female(alice)"
  ]
}
```
**Expected Response:** Success count and details for each fact

### 6. Query with Rules

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)"
}
```

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "grandparent(alice, charlie)"
}
```

**Tool:** `query`
```json
{}
```
**Expected:** "true" (alice is grandmother of charlie)

### 7. Number Range Queries

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "between(1, 5, X)"
}
```

**Tool:** `query` (repeat multiple times)
```json
{}
```
**Responses:** "X = 1", "X = 2", "X = 3", "X = 4", "X = 5", then "no more solutions"

### 8. Mathematical Calculations

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "X is 2 + 3 * 4"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "X = 14"

### 9. List Operations

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "append([1,2], [3,4], X)"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "X = [1,2,3,4]"

### 10. Backtracking with Multiple Solutions

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "member(X, [red, green, blue]), member(Y, [car, bike])"
}
```

This finds all combinations of colors and vehicles:

**Tool:** `query` (multiple times)
- "X = red, Y = car"
- "X = red, Y = bike"  
- "X = green, Y = car"
- "X = green, Y = bike"
- "X = blue, Y = car"
- "X = blue, Y = bike"

### 11. Working with Strings

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "likes(john, pizza)"
}
```

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "likes(mary, pasta)"
}
```

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "likes(Person, Food)"
}
```

**Tool:** `query` (repeat)
- "Person = john, Food = pizza"
- "Person = mary, Food = pasta"

### 12. Remove Facts

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "likes(john, pizza)"
}
```

### 13. Complex Arithmetic

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "X is sqrt(16), Y is X + 10"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "X = 4, Y = 14"

### 14. Type Checking

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "number(42)"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "true"

### 15. Variable Instantiation Check

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "var(X)"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "true" (X is uninstantiated)

### 16. Remove Facts (Single and Multiple)

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "parent(alice, bob)"
}
```
**Expected Response:** Confirmation that the fact was removed

**Tool:** `clauses`
```json
{
  "facts": [
    "parent(bob, charlie)",
    "male(bob)",
    "female(alice)"
  ]
}
```
**Expected Response:** Success count and details for each retraction

### 17. Clear All User-Defined Facts and Rules

**Tool:** `workspace`
```json
{}
```
**Expected Response:** Count of predicates removed from knowledge base

## Error Handling Examples

### 18. Invalid Syntax

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "this is not valid prolog"
}
```
**Expected:** Error message about syntax

### 17. Non-existent Predicate

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "nonexistent_predicate(X)"
}
```
**Expected:** "no_solutions" or error message

### 18. File Not Found

**Tool:** `files`
```json
{
  "filename": "nonexistent_file.pl"
}
```
**Expected:** Error message about file not found

## Performance Examples

### 19. Large List Operations

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "length(List, 1000), member(500, List)"
}
```

This tests performance with large data structures.

### 20. Infinite Solutions (Controlled)

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "between(1, inf, X)"
}
```

**Tool:** `query` (a few times, then close)
**Tool:** `query` (to prevent infinite computation)

## Integration Workflow

### 21. Complete Knowledge Base Session

1. Load data: `files` → `{"operation": "import", "filename": "family.pl"}`
2. Explore structure: `workspace` → `{"operation": "list_symbols"}`
3. Query base facts: `query` → `{"operation": "start", "query": "parent(X, Y)"}`
4. Get solutions step by step with `query` → `{"operation": "next"}`
5. Add new family: `clauses` → `{"operation": "assert", "clauses": "parent(alice, bob)"}`
6. Complex query: `query` → `{"operation": "start", "query": "grandparent(X, Y)"}`
7. Cleanup: `clauses` → `{"operation": "retract", "clauses": "parent(alice, bob)"}`

## Best Practices

### Query Session Management
- Always close queries with `query` operation "close" when done to free resources
- Use step-by-step retrieval for potentially large result sets
- Use standard iterator pattern: call `query` with operation "next" until `status === "done"`

### Error Prevention
- Validate file paths before using `files` with operation "import"
- Test simple queries before complex ones
- **Backup creation**: Use `query` with operation "start" to document facts before using `clauses` with operation "retract"

### Performance Tips
- Close infinite or very large solution queries early
- Use specific queries instead of overly general ones
- Consider closing queries with `query` operation "close" if you only need the first few solutions

## Debugging Workflow

1. List predicates: `workspace` with operation "list_symbols"
2. Test basic facts: `query` → simple fact checking
3. Build complexity gradually
4. Use step-by-step `query` with operation "next" to understand solution flow
5. Add debugging facts with `clauses` operation "assert" for tracing

## Known Variations

Depending on SWI‑Prolog and environment settings, response formatting may vary slightly:

- Atoms/strings: some outputs include quotes (e.g., `S="hello_world"`) while others show atoms without quotes (`S=hello_world`).
- Lists and bindings: values may appear inside brackets depending on the term structure (e.g., `Solution: [S=world]`).
- Whitespace: spacing around `=` can vary.

Tips:
- Prefer pattern‑based matching in clients/tests (e.g., `/S\s*=\s*hello_world/`) rather than strict string equality.
- For structured parsing, use the server’s `solution([...])` format as the source of truth and then format for display.

## Safe Built-ins (Whitelist) & Examples

The server enables a safe subset of pure built-ins and list utilities in the `knowledge_base` module so you can write useful queries without side effects. Below are practical examples that work out of the box.

### Arithmetic and Filtering

Find all even numbers between 1 and 10:

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "findall(X, (between(1,10,X), 0 is X mod 2), L)"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "L = [2,4,6,8,10]"

### Collections with findall/3

Collect elements from a list:

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "findall(X, member(X, [a,b,c]), L)"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "L = [a,b,c]"

### Guarded maplist/2-3 (knowledge base only)

Define a pure helper, then map it over a list. The server exposes `knowledge_base:maplist` which only accepts `knowledge_base:` goals to keep execution safe.

1) Define a helper in `knowledge_base`:

**Tool:** `clauses`
```json
{
  "operation": "assert",
  "clauses": "double(X, Y) :- Y is X * 2"
}
```

2) Map over a list:

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "knowledge_base:maplist(double, [1,2,3], L)"
}
```

**Tool:** `query`
```json
{}
```
**Response:** "L = [2,4,6]"

### String and Atom Utilities

Pure string/atom helpers such as `sub_atom/5`, `atom_string/2`, and `string_concat/3` are available under the sandboxed safe set.

Examples:

- Extract a substring:

**Tool:** `query`
```json
{
  "operation": "start",
  "query": "sub_atom('hello_world', 6, 5, 0, S)"
}
```
**Tool:** `query` → "S = world"

- Convert between atom and string:

**Tool:** `query`
```json
{
  "query": "atom_string(A, \"hello\")"
}
```
**Tool:** `query` → "A = hello"

- Concatenate atoms (portable across SWI versions):

**Tool:** `query`
```json
{
  "query": "atom_concat(\"hello\", \"_world\", S)"
}
```
**Tool:** `query` → "S = hello_world"

### Engine Mode Equivalents

You can run these in engine mode for true backtracking control.

Example: enumerate even numbers with an engine session.

1) Start engine
```json
{
  "operation": "start",
  "query": "(between(1,10,X), 0 is X mod 2)"
}
```
Use tool `query_startEngine` with the JSON above.

2) Fetch solutions iteratively with `query_next`
```json
{}
```
Each call yields a new solution until you receive "No more solutions available".

3) Close the engine session
```json
{}
```
Use tool `query_close`.

Notes:
- All examples rely on the server’s whitelist of pure predicates and knowledge-base-restricted calls.
- If you need additional helpers, open an issue to request specific pure predicates.
