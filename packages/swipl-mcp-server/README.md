# @vpursuit/swipl-mcp-server

**SWI-Prolog integration for Model Context Protocol (MCP)** - Orchestrator package

This is the main orchestrator package that integrates multiple MCP plugins to provide comprehensive Prolog and filesystem functionality.

## Architecture

This package uses a plugin-based architecture composed of:

- **[@vpursuit/mcp-core](../mcp-core)**: Plugin system for MCP servers
- **[@vpursuit/mcp-roots](../mcp-roots)**: Dynamic filesystem root discovery
- **[@vpursuit/mcp-prolog](../mcp-prolog)**: SWI-Prolog knowledge base and query tools

## Installation

```bash
npm install -g @vpursuit/swipl-mcp-server
```

Or use via npx:

```bash
npx @vpursuit/swipl-mcp-server
```

## Prerequisites

- **Node.js** 20.0.0 or higher
- **SWI-Prolog** 8.4.0 or higher installed and available in PATH

## Usage

### As MCP Server

Add to your MCP client configuration (e.g., Claude Desktop):

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

### Direct Execution

```bash
# Using installed binary
swipl-mcp-server

# Using npx
npx @vpursuit/swipl-mcp-server
```

## Available Tools

### From @vpursuit/mcp-roots

- **roots_list**: List filesystem roots for path validation

### From @vpursuit/mcp-prolog

- **knowledge_base_load**: Load Prolog files
- **knowledge_base_assert**: Add single fact/rule
- **knowledge_base_assert_many**: Batch add facts/rules
- **knowledge_base_retract**: Remove single fact/rule
- **knowledge_base_retract_many**: Batch remove facts/rules
- **knowledge_base_clear**: Clear knowledge base
- **knowledge_base_dump**: Export knowledge base as Prolog clauses
- **query_start**: Start query session (call_nth/2 mode)
- **query_startEngine**: Start query session (engine mode for true backtracking)
- **query_next**: Get next solution from current query
- **query_close**: Close current query session
- **symbols_list**: List predicates in knowledge base
- **capabilities**: Get machine-readable capabilities summary
- **help**: Get usage guidelines
- **license**: Get license text

## Available Resources

- `prolog://knowledge_base/predicates`: List predicates in knowledge_base module
- `prolog://knowledge_base/dump`: Export current knowledge base
- `reference://help`: Usage guidelines
- `reference://license`: License text
- `reference://logo`: Server logo (SVG)
- `reference://capabilities`: Capabilities summary (JSON)

## Available Prompts

- **prolog_init_expert**: Initialize expert Prolog context with guidance
- **prolog_quick_reference**: Get quick reference for common Prolog tasks
- **prolog_analyze_knowledge_base**: Analyze current knowledge base state
- **prolog_knowledge_base_builder**: Build knowledge base from requirements
- **prolog_query_optimizer**: Optimize Prolog query performance

## Security

All Prolog operations are sandboxed and validated:

- File operations restricted to allowed directories
- Dangerous predicates blocked (shell, system, call, halt)
- Pre-execution validation via library(sandbox)
- Path traversal protection

## Environment Variables

- `SWI_MCP_ALLOWED_ROOTS`: Comma-separated list of allowed root paths
- `SWI_MCP_STRICT_ROOTS`: Set to "true" to disable fallback directory
- `DEBUG`: Set to "swipl-mcp-server" or "*" for debug logging

## Development

```bash
# Build all packages
npm run build

# Build this package only
npm run build -w packages/swipl-mcp-server

# Run tests
npm test
```

## License

BSD-3-Clause

## Links

- [GitHub Repository](https://github.com/vpursuit/swipl-mcp-server)
- [Model Context Protocol](https://modelcontextprotocol.io)
- [SWI-Prolog](https://www.swi-prolog.org)
