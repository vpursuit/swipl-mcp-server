<img src="https://raw.githubusercontent.com/vpursuit/model-context-lab/main/products/swipl-mcp-server/images/logo.svg" alt="SWI-Prolog MCP Server logo" width="100" />

# SWI-Prolog MCP Server

[![Build Status](https://github.com/vpursuit/model-context-lab/actions/workflows/npm-publish.yml/badge.svg)](https://github.com/vpursuit/model-context-lab/actions/workflows/npm-publish.yml)

| Package | License | Node |
|---------|---------|------|
| [@vpursuit/swipl-mcp-server](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server) | BSD-3-Clause | ≥20.0.0 |

> 🔒 **Supply Chain Security**: This package is published with [npm provenance attestation](https://docs.npmjs.com/generating-provenance-statements), providing cryptographic proof of build integrity and source verification. Look for the ✓ badge on [npm](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server).

An MCP server that lets tools-enabled LLMs work directly with SWI‑Prolog. It supports loading Prolog files, adding/removing facts and rules, listing symbols, and running queries with two modes: deterministic pagination and true engine backtracking.

## Table of Contents

- [Requirements](#requirements)
- [Installation](#installation)
  - [Claude Code CLI](#claude-code-cli)
  - [Claude Desktop](#claude-desktop)
  - [Cline (VS Code Extension)](#cline-vs-code-extension)
  - [Codex](#codex)
  - [MCP Inspector (for testing)](#mcp-inspector-for-testing)
  - [Development Setup](#development-setup)
- [Configuration](#configuration)
  - [Environment Variables](#environment-variables)
  - [State & Lifecycle](#state--lifecycle)
- [Features](#features)
  - [MCP Prompts](#mcp-prompts)
  - [MCP Resources](#mcp-resources)
  - [Tools](#tools)
- [Examples](#examples)
- [Architecture](#architecture)
- [Security](#security)
- [Troubleshooting](#troubleshooting)
- [Development](#development)
- [Contributing](#contributing)
- [Documentation](#documentation)
- [License](#license)


## Requirements

- Node.js ≥ 20.0.0
- SWI‑Prolog installed and available in PATH

## Installation

### Claude Code CLI
```bash
claude mcp add swipl-mcp-server npx @vpursuit/swipl-mcp-server
```

### Claude Desktop
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

- MacOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
- Windows: `%APPDATA%/Claude/claude_desktop_config.json`

### Cline (VS Code Extension)
```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "autoApprove": [],
      "disabled": false,
      "timeout": 60,
      "type": "stdio",
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"]
    }
  }
}
```
Configure via Cline's MCP settings in VS Code.

### Codex
```toml
[mcp_servers.swipl-mcp-server]
transport = "stdio"
enabled = true
command = "npx"
args = ["@vpursuit/swipl-mcp-server"]
```
Add to `~/.codex/config.toml`

### MCP Inspector (for testing)
```bash
npx @modelcontextprotocol/inspector --transport stdio npx @vpursuit/swipl-mcp-server
```

### ... and many others may also work

### Development Setup
If you cloned the repo, you may use this configuration. Note: change <path to your development directory> to your local setup.
```json
{
  "mcpServers": {
    "swipl": {
      "command": "node",
      "args": ["<path to your development directory>/model-context-lab/products/swipl-mcp-server/dist/index.js"],
      "env": {
        "SWI_MCP_READY_TIMEOUT_MS": "10000",
        "SWI_MCP_QUERY_TIMEOUT_MS": "120000",
        "MCP_LOG_LEVEL": "debug",
        "DEBUG": "swipl-mcp-server"
      }
    }
  }
}
```

## Configuration

### Filesystem Roots

**Required for file operations:** Configure allowed directories for loading Prolog files.

**Option 1: MCP Client Roots (Recommended)**
Configure roots in your MCP client. For Claude Desktop, add a `roots` array:
```json
{
  "mcpServers": {
    "swipl": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": ["/Users/you/prolog", "/Users/you/knowledge"]
    }
  }
}
```

**Option 2: Environment Variable**
Set `SWI_MCP_ALLOWED_ROOTS` with comma-separated absolute paths:
```json
{
  "mcpServers": {
    "swipl": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "env": {
        "SWI_MCP_ALLOWED_ROOTS": "/Users/you/prolog,/Users/you/knowledge"
      }
    }
  }
}
```

**Security:** Without configuration, file operations are disabled by default. Use the `roots_list` tool to verify configured roots.

### Environment Variables

Configure timeouts, logging, and behavior via environment variables:

- `SWI_MCP_ALLOWED_ROOTS`: comma-separated absolute paths for file access (see above)
- `SWI_MCP_READY_TIMEOUT_MS`: server startup timeout (ms), default 5000
- `SWI_MCP_QUERY_TIMEOUT_MS`: query execution timeout (ms), default 30000
- `MCP_LOG_LEVEL`: `debug` | `info` | `warn` | `error` | `silent` (default `warn`)
- `DEBUG`: enable debug logs, set to `swipl-mcp-server`
- `SWI_MCP_TRACE`: optional low-level trace of child I/O and protocol
- `SWI_MCP_PROLOG_PATH`: override Prolog server script path

## State & Lifecycle

- Transport: `stdio`. The MCP client owns the connection lifecycle.
- Shutdown: the server exits on `SIGINT`/`SIGTERM` or when the client closes stdio. On stdio close, a small grace (~25ms) allows final responses to flush before exit.
- Stateful per connection: asserted facts/rules live in memory for the lifetime of the MCP connection (one Node process and one SWI‑Prolog child). When the client disconnects and the server exits, in‑memory state is reset on next start.
- Client guidance: keep a single stdio connection open for workflows that depend on shared state across multiple tool calls; avoid closing stdin immediately after a request.
- Durability (optional): if persistent Knowledge Base is desired across restarts, use `knowledge_base_dump` to save to a configured root directory and `knowledge_base_load` (or `knowledge_base_assert_many`) to restore on startup. See [docs/lifecycle.md](./docs/lifecycle.md) for patterns.

## Features

### MCP Prompts
Prompts guide AI assistants to help you with Prolog programming, knowledge base building and query optimization.

**How it works:**
1. You select a prompt (via `/swipl` command in Claude Code CLI)
2. The prompt guides the AI assistant on how to approach your Prolog task
3. The AI assistant helps you with expert knowledge and step-by-step guidance

*Note: Other AI assistants may access and use these prompts differently depending on their MCP implementation.*

In Claude Code CLI, these prompts are available as slash commands. Simply type `/swipl` to see all available commands:

![SWI-Prolog slash commands in Claude Code CLI](./images/swipl-slash-commands.png)

Available prompts:
- **`prolog_init_expert`** - Initialize expert Prolog assistance mode with optional task focus
- **`prolog_quick_reference`** - Get comprehensive server overview and capabilities
- **`prolog_analyze_knowledge_base`** - Analyze current knowledge base state and structure
- **`prolog_knowledge_base_builder`** - Build domain-specific knowledge bases with guided construction
- **`prolog_query_optimizer`** - Optimize Prolog queries for performance and efficiency

### MCP Resources
Dynamic and static resources for knowledge base access:

- **`prolog://knowledge_base/predicates`** - List all predicates in the knowledge base
- **`prolog://knowledge_base/dump`** - Export complete knowledge base as Prolog clauses
- **`reference://help`** - Usage guidelines and server tips
- **`reference://license`** - BSD-3-Clause license text
- **`reference://capabilities`** - Machine-readable server capabilities (JSON)

### Tools

- **Core:** `help`, `license`, `capabilities`
- **Knowledge base:** `knowledge_base_load`, `knowledge_base_assert`, `knowledge_base_assert_many`, `knowledge_base_retract`, `knowledge_base_retract_many`, `knowledge_base_clear`, `knowledge_base_dump`
- **Query:** `query_start`, `query_startEngine`, `query_next`, `query_close`
- **Symbols:** `symbols_list`

## Available Predicates

All standard SWI-Prolog predicates are available (lists, arithmetic, meta-predicates, etc.).

**Note:** CLP(FD) (`library(clpfd)`) is **not available** for security reasons. Use standard Prolog alternatives:
- `between/3` instead of `X in 1..10`
- `is/2` instead of `#=`
- `permutation/2` for generating unique values
- Generate-and-test pattern instead of constraint propagation

## Examples

### Loading and Querying Knowledge Base

Load a Prolog file (must be in a configured root directory):
```json
knowledge_base_load { "filename": "/Users/you/prolog/family.pl" }
```

Start a query and iterate through solutions:
```json
query_start { "query": "parent(X, mary)" }
query_next()  // Returns {solution: "X=john", status: "success"}
query_next()  // Returns {solution: "X=bob", status: "success"}
query_next()  // Returns {solution: null, status: "done"}
query_close() // Close when done
```

Use the standard iterator pattern - call `query_next()` repeatedly until `status === "done"`.

### Engine Mode (True Backtracking)

For queries requiring all solutions or complex backtracking:
```json
query_startEngine { "query": "member(X, [1,2,3])" }
query_next()  // Returns {solution: "X=1", status: "success"}
query_next()  // Returns {solution: "X=2", status: "success"}
query_next()  // Returns {solution: "X=3", status: "success"}
query_next()  // Returns {solution: null, status: "done"}
query_close()
```

### Database Operations

**Add facts:**
```json
// Single fact
knowledge_base_assert { "fact": "parent(john, mary)" }

// Multiple facts
knowledge_base_assert_many {
  "facts": ["parent(john, mary)", "parent(mary, alice)"]
}
```

**Remove facts:**
```json
// Single fact
knowledge_base_retract { "fact": "parent(john, mary)" }

// Multiple facts
knowledge_base_retract_many {
  "facts": ["parent(john, mary)", "parent(mary, alice)"]
}

// Clear all user facts
knowledge_base_clear {}
```

### More Examples

See [docs/examples.md](./docs/examples.md) for comprehensive examples including arithmetic, list operations, collections, and string/atom helpers.

## Architecture

This package uses a plugin-based architecture with three internal components:

- **@vpursuit/mcp-server-core** - Plugin system for MCP servers
- **@vpursuit/mcp-server-roots** - Dynamic filesystem root discovery
- **@vpursuit/mcp-server-prolog** - SWI-Prolog knowledge base and query tools

These plugins are bundled within this package and are not published separately.

The Prolog integration uses:
- Single persistent SWI‑Prolog process with two query modes (standard via `call_nth/2`, engine via SWI engines)
- Term-based wire protocol: Node wraps requests as `cmd(ID, Term)`, replies as `id(ID, Reply)`; back‑compatible with bare terms
- Enhanced security model with file path restrictions, library(sandbox) validation, and dangerous predicate blocking

Details: see [docs/architecture.md](./docs/architecture.md).

### Session State Machine

The server maintains a session state machine to coordinate query and engine sessions. Key points:

- Exactly one session type can be active at a time (query or engine)
- The `*_completed` states keep context so that subsequent `next` calls respond with "no more solutions" until explicitly closed
- Transient `closing_*` states serialize shutdown before new sessions begin
- Invalid transitions are logged when `SWI_MCP_TRACE=1`

For the detailed state transition diagram, see [docs/session-state.md](./docs/session-state.md).

## Security

The server implements multiple security layers to protect your system:

### File Path Restrictions
- **Allowed Directory**: Files can only be loaded from `~/.model-context-lab/`
- **Blocked Directories**: System directories (`/etc`, `/usr`, `/bin`, `/var`, etc.) are automatically blocked
- **Example**:
  ```json
  knowledge_base_load { "filename": "/etc/passwd" }
  ```
  → `Security Error: Access to system directories is blocked`

### Dangerous Predicate Detection
- **Pre-execution Blocking**: Dangerous operations are caught before execution
- **Blocked Predicates**: `shell()`, `system()`, `call()`, `assert()`, `halt()`, etc.
- **Example**:
  ```json
  knowledge_base_assert { "fact": "malware :- shell('rm -rf /')" }
  ```
  → `Security Error: Operation blocked - contains dangerous predicate 'shell'`

### Additional Protections
- Library(sandbox) validation for built-in predicates
- Timeout protection against infinite loops
- Module isolation in dedicated `knowledge_base` namespace

### Supply Chain Security

This package is published with **npm provenance attestation**, providing:
- **Build Transparency**: Cryptographic proof packages are built from source in GitHub Actions
- **Source Verification**: Direct link between published package and source repository commit
- **Signature Verification**: Packages signed by Sigstore, logged in public transparency ledger

**Verify Package Provenance:**
```bash
npm view @vpursuit/swipl-mcp-server --json | jq .dist.attestations
```

**Learn More:** [Our Security Policy](./SECURITY.md#npm-publishing-security) | [npm Provenance Docs](https://docs.npmjs.com/generating-provenance-statements)

See [SECURITY.md](./SECURITY.md) for complete security documentation.

## Troubleshooting

- "Prolog not found": ensure `swipl --version` works; SWI‑Prolog must be in PATH
- Startup timeout: increase `SWI_MCP_READY_TIMEOUT_MS`
- Query timeout: increase `SWI_MCP_QUERY_TIMEOUT_MS`
- Session conflicts: close current session before starting a different mode
- `Security Error: ...`: file access blocked or dangerous predicates detected; see Security
- Custom script path: set `SWI_MCP_PROLOG_PATH`
- Query sessions: after exhausting solutions, `query_next` returns "No more solutions available" until explicitly closed

## Development

```bash
# From monorepo root
npm install

# Build all packages
npm run build

# Build this package only
npm run build -w products/swipl-mcp-server

# Run tests
npm test
```

Publishing and release workflows are documented in [PUBLISHING.md](../../PUBLISHING.md).

## Contributing

See [CONTRIBUTING.md](../../CONTRIBUTING.md) for local setup, workflow, and the PR checklist.

For security practices, reporting, and hardening guidance, see [SECURITY.md](./SECURITY.md).

## Documentation

- **[Installation Guide](./docs/installation.md)** — Complete setup for all MCP clients
- **[Features Reference](./docs/features.md)** — Detailed prompts, resources, and tools documentation
- **[Examples](./docs/examples.md)** — Copy-paste usage examples
- **[Architecture](./docs/architecture.md)** — Components, modes, and wire protocol
- **[Lifecycle](./docs/lifecycle.md)** — Server lifecycle, state, and persistence patterns
- **[Deployment](./docs/deployment.md)** — Release, packaging, and install from source

## Internal Architecture

This package bundles three internal plugins (not published separately):
- **@vpursuit/mcp-server-core** - Plugin system for MCP servers
- **@vpursuit/mcp-server-prolog** - Prolog integration plugin
- **@vpursuit/mcp-server-roots** - Filesystem roots management

These components are private to the monorepo and bundled into the main package during the build process.

## License

BSD‑3‑Clause. See [LICENSE](./LICENSE) for details.

## Links

- [GitHub Repository](https://github.com/vpursuit/model-context-lab)
- [NPM Package](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server)
- [Model Context Protocol](https://modelcontextprotocol.io)
- [SWI-Prolog](https://www.swi-prolog.org)
