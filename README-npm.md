# SWI-Prolog MCP Server

An MCP server that lets tools-enabled LLMs work directly with SWI‑Prolog. It supports loading Prolog files, adding/removing facts and rules, listing symbols, and running queries with deterministic pagination and true engine backtracking.

## Quick Start

```bash
# Run directly with npx (no installation required)
npx @vpursuit/swipl-mcp-server

# Or install globally
npm install -g @vpursuit/swipl-mcp-server
swipl-mcp-server
```

## Requirements

- Node.js ≥ 18.0.0
- SWI‑Prolog installed and available in PATH

## Configuration

### Environment Variables

Configure timeouts and logging via environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `SWI_MCP_READY_TIMEOUT_MS` | Server startup timeout (ms) | `5000` |
| `SWI_MCP_QUERY_TIMEOUT_MS` | Query execution timeout (ms) | `30000` |
| `MCP_LOG_LEVEL` | Logging verbosity | `warn` |
| `DEBUG` | Enable debug logging (`swipl-mcp-server`) | - |
| `SWI_MCP_TRACE` | Low-level spawn/protocol trace | - |
| `SWI_MCP_PROLOG_PATH` | Override Prolog server script path | - |

### Claude Desktop Integration

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

## Tools Overview

- Core: `help`, `license`, `capabilities`
- Knowledge base: `knowledge_base_load`, `knowledge_base_assert`, `knowledge_base_retract`, `knowledge_base_dump`
- Query: `query_start`, `query_startEngine`, `query_next`, `query_close`
- Symbols: `symbols_list`

## Protocol (short)

- Each request/response is a single-line Prolog term.
- Requests are enveloped as `cmd(ID, Term)` and responses as `id(ID, Reply)`; bare terms remain accepted.

## Usage Examples

1. Load knowledge: `knowledge_base_load({"filename":"~/.swipl-mcp-server/family.pl"})`
2. Start query: `query_start({"query":"parent(X, mary)"})`
3. Get solutions: `query_next()` … then `query_close()`
4. Try engine mode: `query_startEngine({"query":"member(X,[1,2,3])"})`

More examples in docs/examples.md.

## Safety & Security

- **File Path Restrictions**: Only `~/.swipl-mcp-server/` directory allowed for file operations
- **Dangerous Predicate Blocking**: Pre-execution detection of `shell()`, `system()`, `call()`, etc.
- **Hybrid Validation**: library(sandbox) validation plus explicit security checks
- **Session Management**: Mutual exclusion between modes; timeouts for protection

## Troubleshooting

- SWI‑Prolog not found: `swipl --version` must work
- Increase `SWI_MCP_READY_TIMEOUT_MS`/`SWI_MCP_QUERY_TIMEOUT_MS` if timing out
- Use `SWI_MCP_PROLOG_PATH` to point at a custom `server.pl`

## Package Info

- Dependencies: `@modelcontextprotocol/sdk`, `zod`
- License: BSD‑3‑Clause
- Repository: https://github.com/vpursuit/swipl-mcp-server

## Support

File issues and feature requests on GitHub.

---

Built with [SWI‑Prolog](https://www.swi-prolog.org/) and the [Model Context Protocol](https://modelcontextprotocol.io/).
