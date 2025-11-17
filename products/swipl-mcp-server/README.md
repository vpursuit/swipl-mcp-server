<img src="https://raw.githubusercontent.com/vpursuit/model-context-lab/main/products/swipl-mcp-server/images/logo.svg" alt="SWI-Prolog MCP Server logo" width="100" />

# SWI-Prolog MCP Server

[![Build Status](https://github.com/vpursuit/model-context-lab/actions/workflows/npm-publish.yml/badge.svg)](https://github.com/vpursuit/model-context-lab/actions/workflows/npm-publish.yml)

| Package | License | Node |
|---------|---------|------|
| [@vpursuit/swipl-mcp-server](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server) | BSD-3-Clause | ≥20.0.0 |

> 🔒 **Supply Chain Security**: This package is published with [npm provenance attestation](https://docs.npmjs.com/generating-provenance-statements), providing cryptographic proof of build integrity and source verification. Look for the ✓ badge on [npm](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server).

A MCP server that lets tools-enabled LLMs work directly with SWI‑Prolog. It supports loading Prolog files, adding/removing facts and rules, listing symbols, and running queries with two modes: deterministic pagination and true engine backtracking.

## Table of Contents

- [Requirements](#requirements)
- [Installation](#installation)
  - [Claude Code CLI](#claude-code-cli)
  - [Claude Desktop](#claude-desktop)
  - [Cline (VS Code Extension)](#cline-vs-code-extension)
  - [Codex CLI](#codex-cli)
  - [Gemini CLI](#gemini-cli)
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

### Node.js
- Version ≥ 20.0.0
- [Download Node.js](https://nodejs.org/)

### SWI-Prolog
- **Required** - Version 9.0 or higher
- **Installation:** [Official Download Page](https://www.swi-prolog.org/download/stable)
  - **macOS:** `brew install swi-prolog`
  - **Ubuntu/Debian:** `sudo apt-get install swi-prolog` or `sudo snap install swi-prolog`
  - **Windows:** Download and run installer from official page
- **Verify installation:** `swipl --version`

**Note:** SWI-Prolog must be installed and available in your PATH before using this server.

## Installation

### Claude Code CLI
```bash
claude mcp add swipl-mcp-server npx @vpursuit/swipl-mcp-server
```

**Verification:**
```bash
claude mcp list
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

**Configuration file location:**
- macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
- Windows: `%APPDATA%/Claude/claude_desktop_config.json`
- Linux: `~/.config/claude/claude_desktop_config.json`

**Restart Required:** After modifying the configuration file, restart Claude Desktop for changes to take effect.

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

### Codex CLI
```toml
[mcp_servers.swipl-mcp-server]
transport = "stdio"
enabled = true
command = "npx"
args = ["@vpursuit/swipl-mcp-server"]
```
Add to `~/.codex/config.toml`

### Gemini CLI
```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "type": "stdio",
      "command": "npx",
      "args": ["-y", "@vpursuit/swipl-mcp-server"],
      "trust": true
    }
  }
}
```
Add to `.gemini/settings.json`

### MCP Inspector (for testing)
```bash
npx @modelcontextprotocol/inspector --transport stdio npx @vpursuit/swipl-mcp-server
```

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
- `KB_LIBRARIES`: comma-separated library names to load at startup (e.g., `clpb,aggregate,random`). Default libraries always loaded: `lists`, `between`, `clpfd`, `apply`, `pairs`, `ordsets`
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
- Durability (optional): if persistent Knowledge Base is desired across restarts, use the `workspace` tool with `operation: "snapshot"` to save to a configured root directory and the `files` tool with `operation: "import"` (or `clauses` with `operation: "assert"`) to restore on startup. See [docs/lifecycle.md](./docs/lifecycle.md) for patterns.

## Features

### MCP Prompts
Prompts guide AI assistants to help you with Prolog programming, knowledge base building and query optimization.

**How it works:**
1. You select a prompt through your MCP client
2. The prompt guides the AI assistant on how to approach your Prolog task
3. The AI assistant helps you with expert knowledge and step-by-step guidance

**Slash Commands:** In some MCP clients (Claude Code CLI, Gemini), prompts are available as convenient slash commands:
- Type `/` in the chat to see all available commands and prompts
- You may see the prompts prefixed by `swipl-mcp-server-`
- Example: `/swipl-mcp-server-expert` to initialize expert Prolog assistance

**Client Compatibility (at the time of this writing):**
- ✅ **Claude Code CLI** - Full prompt support with slash commands
- ✅ **Gemini CLI** - Full prompt support with slash commands
- ❌ **Codex CLI** - Does not currently support prompts

Available prompts:
- **`genealogy`** - Build and query family trees using relational logic
  - `family_info` (required): Family members and relationships to model
- **`scheduling`** - Schedule tasks with dependencies using CLP(FD)
  - `tasks` (required): Tasks to schedule with durations and dependencies
- **`puzzle`** - Solve logic puzzles using constraint programming
  - `puzzle` (optional): The logic puzzle to solve with numbered clues. If empty, suggests interesting puzzles
- **`grammar`** - Parse natural language using Definite Clause Grammars (DCGs)
  - `sentence` (optional): Sentence to parse. If empty, uses a default example

### MCP Resources
Dynamic and static resources for workspace access:

- **`mcp://workspace/symbols`** - List all user-defined predicates in the workspace
- **`mcp://workspace/snapshot`** - Export workspace with original source text and preserved formatting
- **`mcp://server/branding/logo`** - Server logo (SVG)
- **`mcp://server/capabilities`** - Machine-readable server capabilities (JSON)

### Tools

- **`query`** - Unified tool to start queries, get next solutions, and close query sessions
- **`capabilities`** - Get machine-readable capabilities summary
- **`clauses`** - Assert or retract facts/rules (with source preservation)
- **`files`** - Import/unimport Prolog files with provenance tracking
- **`workspace`** - Manage workspace snapshots and list predicates
- **`explain_error`** - Analyze and explain Prolog errors using domain expertise

## Available Predicates

All standard SWI-Prolog predicates are available (lists, arithmetic, meta-predicates, etc.).

`library(clpfd)` is available for constraint programming.

## Examples

### Loading and Querying Knowledge Base

Load a Prolog file (must be in a configured root directory):
```json
files { "operation": "import", "filename": "/Users/you/prolog/family.pl" }
```

Start a query and iterate through solutions:
```json
query { "operation": "start", "query": "parent(X, mary)" }
query { "operation": "next" }  // Returns {solution: "X=john", status: "success"}
query { "operation": "next" }  // Returns {solution: "X=bob", status: "success"}
query { "operation": "next" }  // Returns {solution: null, status: "done"}
query { "operation": "close" } // Close when done
```

Use the standard iterator pattern - call `query` with `operation: "next"` repeatedly until `status === "done"`.

### Engine Mode (True Backtracking)

For queries requiring all solutions or complex backtracking (required for CLP(FD)):
```json
query { "operation": "start", "query": "member(X, [1,2,3])", "use_engine": true }
query { "operation": "next" }  // Returns {solution: "X=1", status: "success"}
query { "operation": "next" }  // Returns {solution: "X=2", status: "success"}
query { "operation": "next" }  // Returns {solution: "X=3", status: "success"}
query { "operation": "next" }  // Returns {solution: null, status: "done"}
query { "operation": "close" }
```

### Database Operations

**Add facts:**
```json
// Single fact
clauses { "operation": "assert", "clauses": "parent(john, mary)" }

// Multiple facts
clauses {
  "operation": "assert",
  "clauses": ["parent(john, mary)", "parent(mary, alice)"]
}
```

**Remove facts:**
```json
// Single fact
clauses { "operation": "retract", "clauses": "parent(john, mary)" }

// Multiple facts
clauses {
  "operation": "retract",
  "clauses": ["parent(john, mary)", "parent(mary, alice)"]
}

// Clear entire workspace
workspace { "operation": "reset" }
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
- **Secure by Default**: File operations disabled without explicit root configuration
- **Configuration Required**: Configure roots via MCP client or `SWI_MCP_ALLOWED_ROOTS` environment variable
- **Blocked Directories**: System directories (`/etc`, `/usr`, `/bin`, `/var`, etc.) are automatically blocked
- **Example**:
  ```json
  files { "operation": "import", "filename": "/etc/passwd" }
  ```
  → `Security Error: Access to system directories is blocked`

### Dangerous Predicate Detection
- **Pre-execution Blocking**: Dangerous operations are caught before execution
- **Blocked Predicates**: `shell()`, `system()`, `call()`, `assert()`, `halt()`, etc.
- **Example**:
  ```json
  clauses { "operation": "assert", "clauses": "malware :- shell('rm -rf /')" }
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

**Learn More:** [Our Security Policy](../../SECURITY.md#npm-publishing-security) | [npm Provenance Docs](https://docs.npmjs.com/generating-provenance-statements)

See [SECURITY.md](../../SECURITY.md) for complete security documentation.

## Troubleshooting

### Common Issues

**SWI-Prolog not found:**
```bash
# Verify SWI-Prolog installation
swipl --version

# On macOS with Homebrew
brew install swi-prolog

# On Ubuntu/Debian
sudo apt-get install swi-prolog

# On Windows
# Download from https://www.swi-prolog.org/download/stable
```

**Server startup timeout:**
- Increase `SWI_MCP_READY_TIMEOUT_MS` to `10000` or higher
- Check SWI-Prolog is accessible in PATH
- Enable debug logging with `DEBUG=swipl-mcp-server`

**Query timeout:**
- Increase `SWI_MCP_QUERY_TIMEOUT_MS` for complex queries
- Use `query_close` to terminate long-running queries

**Configuration not loading:**
- Verify JSON syntax in configuration files
- Restart the MCP client after configuration changes
- Check file permissions on configuration directories

**File access errors:**
- Ensure roots are configured via MCP client or `SWI_MCP_ALLOWED_ROOTS`
- Verify files are within configured root directories
- Check file permissions for read access
- Use `roots_list` tool to verify configured roots

**Session conflicts:**
- Close current session before starting a different mode
- Query sessions: after exhausting solutions, `query_next` returns "No more solutions available" until explicitly closed

**Security errors:**
- File access blocked or dangerous predicates detected; see [Security](#security)

### Debug Mode

Enable comprehensive debugging:

```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "DEBUG": "swipl-mcp-server",
    "SWI_MCP_TRACE": "1"
  }
}
```

This provides detailed logs of:
- Server startup and initialization
- Tool calls and responses
- Prolog process communication
- Error conditions and stack traces

### Getting Help

- Check existing [issues](https://github.com/vpursuit/model-context-lab/issues)
- Review [examples](./docs/examples.md) for usage patterns
- Test with MCP Inspector for isolated debugging
- Include debug logs when reporting issues

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

For security practices, reporting, and hardening guidance, see [SECURITY.md](../../SECURITY.md).

## Documentation

**[Complete Documentation Index](./docs/README.md)** - Full navigation guide

**Quick Links:**
- **[Installation](#installation)** — Complete setup for all MCP clients
- **[Configuration](./docs/configuration.md)** — Comprehensive configuration reference
- **[Troubleshooting](#troubleshooting)** — Common issues and solutions
- **[Features Reference](./docs/features.md)** — Prompts, resources, and tools
- **[Examples](./docs/examples.md)** — Practical usage examples
- **[Architecture](./docs/architecture.md)** — System design and internals
- **[Logging & Debugging](./docs/logging.md)** — Comprehensive debugging guide
- **[Deployment](./docs/deployment.md)** — Release quick reference


## License

BSD‑3‑Clause. See [LICENSE](./LICENSE) for details.

## Links

- [GitHub Repository](https://github.com/vpursuit/model-context-lab)
- [NPM Package](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server)
- [Model Context Protocol](https://modelcontextprotocol.io)
- [SWI-Prolog](https://www.swi-prolog.org)
