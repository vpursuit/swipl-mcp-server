# SWI-Prolog MCP Server

A Model Context Protocol (MCP) server for integrating SWI-Prolog with Large Language Model applications.

## Quick Start

```bash
# Run directly with npx (no installation required)
npx @vpursuit/swipl-mcp-server

# Or install globally
npm install -g @vpursuit/swipl-mcp-server
swipl-mcp-server
```

## Requirements

- **Node.js** ≥ 18.0.0
- **SWI-Prolog** installed and available in PATH

## Configuration

### Environment Variables

Configure timeouts and logging via environment variables:

| Variable | Description | Default | Notes |
|----------|-------------|---------|-------|
| `SWI_MCP_READY_TIMEOUT_MS` | Server startup timeout | `5000` | Increase if SWI-Prolog starts slowly |
| `SWI_MCP_QUERY_TIMEOUT_MS` | Query execution timeout | `30000` | Increase for complex queries |
| `MCP_LOG_LEVEL` | Logging verbosity | `warn` | `debug`, `info`, `warn`, `error`, `silent` |
| `DEBUG` | Enable debug logging | - | Set to `swipl-mcp-server` |

### Claude Desktop Integration

#### Basic Setup
```json
{
  "mcpServers": {
    "swipl": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"]
    }
  }
}
```

#### With Custom Timeouts
```json
{
  "mcpServers": {
    "swipl": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "env": {
        "SWI_MCP_READY_TIMEOUT_MS": "10000",
        "SWI_MCP_QUERY_TIMEOUT_MS": "120000"
      }
    }
  }
}
```

## Features

This MCP server provides 14 tools for Prolog interaction:

### Core Tools
- **`help`** - Usage guidelines and examples
- **`consult_file`** - Load Prolog files (.pl)
- **`list_predicates`** - Show available predicates
- **`assert_clause`** - Add facts/rules to knowledge base
- **`retract_clause`** - Remove facts/rules

### Query Tools - Standard Mode
- **`start_query`** - Begin query session with deterministic pagination
- **`next_solution`** - Get next solution (memory efficient)
- **`close_query`** - End query session

### Query Tools - Engine Mode  
- **`start_engine`** - Begin query with true backtracking
- **`next_engine`** - Get next solution (no recomputation)
- **`close_engine`** - End engine session

### System Tools
- **`get_capabilities`** - Machine-readable server summary



## MCP ↔ Prolog Protocol

- Each request/response is a single line Prolog term.
- Requests are enveloped as `cmd(ID, Term)` and responses as `id(ID, Reply)` for correlation.
- Backward compatible with bare terms; safety checks apply to the inner term.
- Most users don’t need these details — basic commands work out of the box.

## Structured Outputs

- Tools return text content for humans first, and a JSON content item second where useful.
- JSON is included for: `get_capabilities`, `start_query`, `next_solution`, `close_query`, `list_predicates`, `assert_clause`, `retract_clause`, `start_engine`, `next_engine`, `close_engine`, `help`, and `license`.
- Clients can prefer the `json` item if present or fall back to `text`. Basic usage can ignore the JSON item.

## Usage Examples

### With Claude Desktop

Add to your Claude Desktop MCP configuration:

```json
{
  "mcpServers": {
    "swipl": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"]
    }
  }
}
```

### With MCP Inspector

```bash
npx @modelcontextprotocol/inspector --transport stdio npx @vpursuit/swipl-mcp-server
```

### Basic Workflow

1. **Load knowledge**: `consult_file({filename: "family.pl"})`
2. **Start query**: `start_query({query: "parent(X, mary)"})`
3. **Get solutions**: `next_solution()` → `"X = john"`
4. **Continue**: `next_solution()` → `"X = alice"`
5. **Close**: `close_query()`

## Safety & Security

- **Sandboxed execution** - Only safe, side-effect-free predicates allowed
- **Query whitelisting** - Blocks I/O, OS, and network operations
- **Secure consultation** - Safe file loading with directive filtering
- **Session management** - Proper resource cleanup and timeout protection
- **Protocol metadata** - `cmd(ID, Term)`/`id(ID, Reply)` tags do not change the sandbox; safety checks apply to the inner `Term`.

## Supported Predicates

The server allows only pure, mathematical and logical operations:

| Category | Examples |
|----------|----------|
| **Logic** | `true`, `,`, `;`, `->`, `\+`, `!` |
| **Comparison** | `=`, `\=`, `==`, `\==`, `>`, `<`, `>=`, `=<` |
| **Arithmetic** | `is/2`, `=:=`, `=\=` |
| **Terms** | `=../2`, `functor/3`, `arg/3`, `atom/1`, `var/1` |
| **Lists** | `member/2`, `append/3`, `length/2`, `select/3` |
| **Collections** | `findall/3`, `between/3` |

## Environment Variables

- **`MCP_LOG_LEVEL`**: `debug`, `info`, `warn`, `error`, `silent` (default: warn)
- **`SWI_MCP_PROLOG_PATH`**: Override Prolog server script location
- **`SWI_MCP_EXTENDED_SAFE`**: Enable string/atom helpers (`sub_atom/5`, `atom_string/2`)

## Examples

### Family Relationships
```prolog
% Load family.pl, then query:
parent(X, mary).           % → X = john; X = alice
father(X, Y).             % → All father relationships
grandparent(X, Z).        % → All grandparent relationships
```

### List Processing
```prolog
member(X, [1,2,3]).                    % → X = 1; X = 2; X = 3
findall(X, member(X, [a,b,c]), L).    % → L = [a,b,c]
append([1,2], [3,4], L).              % → L = [1,2,3,4]
```

### Arithmetic
```prolog
findall(X, (between(1,10,X), 0 is X mod 2), L).  % → L = [2,4,6,8,10]
```

## Troubleshooting

**Server won't start:**
- Ensure SWI-Prolog is installed: `swipl --version`
- Check Node.js version: `node --version` (need ≥18)

**"unsafe_goal" errors:**
- Your query uses blocked predicates (I/O, OS operations)
- Use only mathematical and logical operations
- Enable extended helpers: `SWI_MCP_EXTENDED_SAFE=true`

## Package Info

- **Size**: ~21KB compressed, ~88KB installed
- **Dependencies**: Only `@modelcontextprotocol/sdk` and `zod`
- **License**: ISC
- **Repository**: [GitHub](https://github.com/vpursuit/swipl-mcp-server)

## Support

For issues, feature requests, and contributions, visit the [GitHub repository](https://github.com/vpursuit/swipl-mcp-server).

---

Built with [SWI-Prolog](https://www.swi-prolog.org/) and the [Model Context Protocol](https://modelcontextprotocol.io/).