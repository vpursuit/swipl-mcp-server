# SWI-Prolog MCP Server - Example Queries

A collection of practical examples for using the SWI-Prolog MCP Server.

## Inspector vs Production Usage

This document demonstrates **inspector usage** where you manually call each tool individually. This is useful for:
- Testing and debugging the MCP server
- Learning how the tools work
- Interactive exploration of Prolog knowledge bases

In **production usage**, an AI agent (like Claude) automatically calls these tools based on natural language requests. For example:
- You: "Find all parents of Mary in the family data"
- AI agent: Automatically calls `query_start`, `query_next` (multiple times), then `query_close`

The JSON examples below show the exact tool calls that happen behind the scenes in both scenarios.

## Resources and Prompts

Beyond the interactive tools, this MCP server also provides:

### Resources
Static documentation accessible via MCP resource URIs:
- `swipl://docs/features` - Complete feature documentation
- `swipl://docs/installation` - Installation and setup guide

In production, AI agents can read these resources to understand server capabilities and provide better assistance.

### Expert Prompts
Six specialized prompts for Prolog development assistance:
- `init_expert` - Initialize expert Prolog assistance for complex projects
- `quick_reference` - Get quick syntax and built-in predicate references
- `analyze_kb` - Analyze knowledge base structure and suggest improvements
- `expert_reasoning` - Expert help with complex logical reasoning problems
- `kb_builder` - Guided knowledge base construction assistance
- `query_optimizer` - Optimize query performance and suggest alternatives

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

**Tool:** `knowledge_base_load`
```json
{
  "filename": "test.pl"
}
```

**Expected Response:** Confirmation that the file was loaded

### 2. List Available Predicates

**Tool:** `symbols_list`
```json
{}
```

**Expected Response:** List of all available predicates with arity (e.g. `parent/2`, `male/1`)

## Family Data Examples

### 3. Find All Parents of Mary

**Tool:** `query_start`
```json
{
  "query": "parent(X, mary)"
}
```

**Tool:** `query_next`
```json
{}
```
**Expected Response:** First parent (e.g., "X = john")

**Tool:** `query_next`
```json
{}
```
**Expected Response:** Next parent or "no more solutions"

**Tool:** `query_close`
```json
{}
```

### 4. Step-by-Step List Member Query

**Tool:** `query_start`
```json
{
  "query": "member(X, [apple, banana, cherry])"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "X = apple"

**Tool:** `query_next`
```json
{}
```
**Response:** "X = banana"

**Tool:** `query_next`
```json
{}
```
**Response:** "X = cherry"

**Tool:** `query_close`
```json
{}
```

## Advanced Examples

### 5. Add New Family Relations (Single Facts)

**Tool:** `knowledge_base_assert`
```json
{
  "fact": "parent(alice, bob)"
}
```

**Tool:** `knowledge_base_assert`
```json
{
  "fact": "parent(bob, charlie)"
}
```

### 5b. Add Multiple Family Relations at Once

**Tool:** `knowledge_base_assert_many`
```json
{
  "facts": [
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

**Tool:** `knowledge_base_assert`
```json
{
  "fact": "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)"
}
```

**Tool:** `query_start`
```json
{
  "query": "grandparent(alice, charlie)"
}
```

**Tool:** `query_next`
```json
{}
```
**Expected:** "true" (alice is grandmother of charlie)

### 7. Number Range Queries

**Tool:** `query_start`
```json
{
  "query": "between(1, 5, X)"
}
```

**Tool:** `query_next` (repeat multiple times)
```json
{}
```
**Responses:** "X = 1", "X = 2", "X = 3", "X = 4", "X = 5", then "no more solutions"

### 8. Mathematical Calculations

**Tool:** `query_start`
```json
{
  "query": "X is 2 + 3 * 4"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "X = 14"

### 9. List Operations

**Tool:** `query_start`
```json
{
  "query": "append([1,2], [3,4], X)"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "X = [1,2,3,4]"

### 10. Backtracking with Multiple Solutions

**Tool:** `query_start`
```json
{
  "query": "member(X, [red, green, blue]), member(Y, [car, bike])"
}
```

This finds all combinations of colors and vehicles:

**Tool:** `query_next` (multiple times)
- "X = red, Y = car"
- "X = red, Y = bike"  
- "X = green, Y = car"
- "X = green, Y = bike"
- "X = blue, Y = car"
- "X = blue, Y = bike"

### 11. Working with Strings

**Tool:** `knowledge_base_assert`
```json
{
  "fact": "likes(john, pizza)"
}
```

**Tool:** `knowledge_base_assert`
```json
{
  "fact": "likes(mary, pasta)"
}
```

**Tool:** `query_start`
```json
{
  "query": "likes(Person, Food)"
}
```

**Tool:** `query_next` (repeat)
- "Person = john, Food = pizza"
- "Person = mary, Food = pasta"

### 12. Remove Facts

**Tool:** `knowledge_base_retract`
```json
{
  "fact": "likes(john, pizza)"
}
```

### 13. Complex Arithmetic

**Tool:** `query_start`
```json
{
  "query": "X is sqrt(16), Y is X + 10"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "X = 4, Y = 14"

### 14. Type Checking

**Tool:** `query_start`
```json
{
  "query": "number(42)"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "true"

### 15. Variable Instantiation Check

**Tool:** `query_start`
```json
{
  "query": "var(X)"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "true" (X is uninstantiated)

### 16. Remove Facts (Single and Multiple)

**Tool:** `knowledge_base_retract`
```json
{
  "fact": "parent(alice, bob)"
}
```
**Expected Response:** Confirmation that the fact was removed

**Tool:** `knowledge_base_retract_many`
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

**Tool:** `knowledge_base_clear`
```json
{}
```
**Expected Response:** Count of predicates removed from knowledge base

## Error Handling Examples

### 18. Invalid Syntax

**Tool:** `query_start`
```json
{
  "query": "this is not valid prolog"
}
```
**Expected:** Error message about syntax

### 17. Non-existent Predicate

**Tool:** `query_start`
```json
{
  "query": "nonexistent_predicate(X)"
}
```
**Expected:** "no_solutions" or error message

### 18. File Not Found

**Tool:** `knowledge_base_load`
```json
{
  "filename": "nonexistent_file.pl"
}
```
**Expected:** Error message about file not found

## Performance Examples

### 19. Large List Operations

**Tool:** `query_start`
```json
{
  "query": "length(List, 1000), member(500, List)"
}
```

This tests performance with large data structures.

### 20. Infinite Solutions (Controlled)

**Tool:** `query_start`
```json
{
  "query": "between(1, inf, X)"
}
```

**Tool:** `query_next` (a few times, then close)
**Tool:** `query_close` (to prevent infinite computation)

## Integration Workflow

### 21. Complete Knowledge Base Session

1. Load data: `knowledge_base_load` → `{"filename": "family.pl"}`
2. Explore structure: `symbols_list` → `{}`
3. Query base facts: `query_start` → `{"query": "parent(X, Y)"}`
4. Get solutions step by step with `query_next`
5. Add new family: `knowledge_base_assert` → `{"fact": "parent(alice, bob)"}`
6. Complex query: `query_start` → `{"query": "grandparent(X, Y)"}`
7. Cleanup: `knowledge_base_retract` → `{"fact": "parent(alice, bob)"}`

## Best Practices

### Query Session Management
- Always `query_close` queries when done to free resources
- Use step-by-step retrieval for potentially large result sets
- Use standard iterator pattern: call `query_next()` until `status === "done"`

### Error Prevention
- Validate file paths before `knowledge_base_load`
- Test simple queries before complex ones
- **Backup creation**: Use `query_start` to document facts before `knowledge_base_retract`

### Performance Tips
- Close infinite or very large solution queries early
- Use specific queries instead of overly general ones
- Consider using `query_close` if you only need the first few solutions

## Debugging Workflow

1. List predicates: `symbols_list`
2. Test basic facts: `query` → simple fact checking
3. Build complexity gradually
4. Use step-by-step `next` to understand solution flow
5. Add debugging facts with `knowledge_base_assert` for tracing

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

**Tool:** `query_start`
```json
{
  "query": "findall(X, (between(1,10,X), 0 is X mod 2), L)"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "L = [2,4,6,8,10]"

### Collections with findall/3

Collect elements from a list:

**Tool:** `query_start`
```json
{
  "query": "findall(X, member(X, [a,b,c]), L)"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "L = [a,b,c]"

### Guarded maplist/2-3 (knowledge base only)

Define a pure helper, then map it over a list. The server exposes `knowledge_base:maplist` which only accepts `knowledge_base:` goals to keep execution safe.

1) Define a helper in `knowledge_base`:

**Tool:** `knowledge_base_assert`
```json
{
  "fact": "double(X, Y) :- Y is X * 2"
}
```

2) Map over a list:

**Tool:** `query_start`
```json
{
  "query": "knowledge_base:maplist(double, [1,2,3], L)"
}
```

**Tool:** `query_next`
```json
{}
```
**Response:** "L = [2,4,6]"

### String and Atom Utilities

Pure string/atom helpers such as `sub_atom/5`, `atom_string/2`, and `string_concat/3` are available under the sandboxed safe set.

Examples:

- Extract a substring:

**Tool:** `query_start`
```json
{
  "query": "sub_atom('hello_world', 6, 5, 0, S)"
}
```
**Tool:** `query_next` → "S = world"

- Convert between atom and string:

**Tool:** `query_start`
```json
{
  "query": "atom_string(A, \"hello\")"
}
```
**Tool:** `query_next` → "A = hello"

- Concatenate atoms (portable across SWI versions):

**Tool:** `query_start`
```json
{
  "query": "atom_concat(\"hello\", \"_world\", S)"
}
```
**Tool:** `query_next` → "S = hello_world"

### Engine Mode Equivalents

You can run these in engine mode for true backtracking control.

Example: enumerate even numbers with an engine session.

1) Start engine
```json
{
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
