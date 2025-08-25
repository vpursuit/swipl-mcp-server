# SWI-Prolog MCP Server

A Model Context Protocol (MCP) server for integrating SWI-Prolog with Large Language Model applications.

## Overview

This MCP server enables Large Language Models with tool calling capabilities to interact directly with SWI-Prolog. It provides tools for loading Prolog files, executing queries with step-by-step solution retrieval using two different backtracking modes, and managing facts and rules.

## Quick Start

### Option 1: NPM Package

The fastest way to use the SWI-Prolog MCP Server:

```bash
# Run directly with npx (no installation required)
npx @vpursuit/swipl-mcp-server
```

### Option 2: Download from GitHub (More Control)

For users who prefer not to use package managers or want control over updates:

```bash
# Download latest release
curl -L -o swipl-mcp-server.tar.gz https://github.com/vpursuit/swipl-mcp-server/archive/refs/heads/main.tar.gz

# Extract and install
tar -xzf swipl-mcp-server.tar.gz
cd swipl-mcp-server-main
npm install --production
npm run build

# Use the local installation
node build/index.js
```

**Benefits of GitHub Download:**
- **Version Control**: No automatic updates without your knowledge
- **Security**: Review source code before installation
- **Customization**: Modify configuration if needed

**Requirements:**
- Node.js ≥ 18.0.0
- SWI-Prolog installed and available in PATH

**Package Details:**
- **Package**: `@vpursuit/swipl-mcp-server`
- **Size**: ~21KB compressed, 85KB installed (NPM) / ~95KB (GitHub download)
- **Zero configuration**: Works out of the box

## Configuration

### Environment Variables

The server can be configured via environment variables for timeout control and logging:

| Variable | Description | Default | Range |
|----------|-------------|---------|-------|
| `SWI_MCP_READY_TIMEOUT_MS` | Server startup timeout (ms) | `5000` | 1000-30000 |
| `SWI_MCP_QUERY_TIMEOUT_MS` | Query execution timeout (ms) | `30000` | 5000-300000 |
| `MCP_LOG_LEVEL` | Logging level | `warn` | `debug`, `info`, `warn`, `error`, `silent` |
| `DEBUG` | Enable debug logging | - | `swipl-mcp-server` |
| `SWI_MCP_TRACE` | Enable detailed send/receive traces | - | `true`, `1`, `yes` |

### Claude Desktop Configuration

**Important:** For GitHub installations, replace `/path/to/swipl-mcp-server-main/` with your actual installation directory path in all configurations below.

#### Basic Configuration (NPM)
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

#### Basic Configuration (GitHub Download)
```json
{
  "mcpServers": {
    "swipl": {
      "command": "node",
      "args": ["/path/to/swipl-mcp-server-main/build/index.js"],
      "cwd": "/path/to/swipl-mcp-server-main"
    }
  }
}
```

#### Development Configuration (Extended Timeouts + Debug Logging)
```json
{
  "mcpServers": {
    "swipl": {
      "command": "node",
      "args": ["/path/to/swipl-mcp-server-main/build/index.js"],
      "cwd": "/path/to/swipl-mcp-server-main",
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

#### Production Configuration (Conservative Timeouts)
```json
{
  "mcpServers": {
    "swipl": {
      "command": "node",
      "args": ["/path/to/swipl-mcp-server-main/build/index.js"],
      "cwd": "/path/to/swipl-mcp-server-main",
      "env": {
        "SWI_MCP_READY_TIMEOUT_MS": "8000",
        "SWI_MCP_QUERY_TIMEOUT_MS": "60000",
        "MCP_LOG_LEVEL": "warn"
      }
    }
  }
}
```

**Configuration File Location:**
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%/Claude/claude_desktop_config.json`

### Timeout Scenarios

**When to adjust timeouts:**

- **Increase `SWI_MCP_READY_TIMEOUT_MS`** if:
  - SWI-Prolog installation is slow to start
  - Running on resource-constrained systems
  - Getting "Prolog server ready timeout" errors

- **Increase `SWI_MCP_QUERY_TIMEOUT_MS`** if:
  - Working with large datasets or complex recursive queries
  - Performing computationally intensive operations
  - Getting "Query timeout" errors on valid queries

- **Decrease timeouts** if:
  - Working in high-security environments
  - Want faster failure detection
  - Preventing potential infinite loops

## Features

### Available Tools

#### Core Tools
1. **`help`** (agent-facing) — Quick usage guidelines and examples for agents
   - Parameter: optional `topic` (overview | standard_mode | engine_mode | safety | security | examples | troubleshooting)
   - Returns concise how‑to info without internal knobs

2. **`license`** - Display ISC license information
   - No parameters required

3. **`capabilities`** (agent-facing) — Machine-readable high-level summary
   - No parameters required
   - Returns server name/version, modes, tool names, and security status

#### Database Tools
4. **`db_load`** - Load a Prolog file into the knowledge base
   - Parameter: `filename` (string) - Path to .pl file
   - Example: Load `family.pl` with family data

5. **`db_assert`** - Add a new clause (fact or rule) to the knowledge base
   - Parameter: `fact` (string) - Prolog clause
   - Example: `"parent(john, susan)"` adds a parent relationship
   - Example: `"grandparent(X,Z) :- parent(X,Y), parent(Y,Z)"` adds a rule

6. **`db_retract`** - Remove a clause (fact or rule) from the knowledge base
   - Parameter: `fact` (string) - Clause to remove
   - Example: `"parent(john, susan)"` removes the relationship

7. **`db_dump`** - Export current knowledge base as Prolog facts
   - No parameters required
   - Returns formatted Prolog clauses with proper syntax

8. **`symbols_list`** - List all available predicates
   - No parameters required
   - Shows available predicates with their arity (name/arity)

#### Query Tools - Standard Mode (call_nth/2)
9. **`query_start`** - Start a new Prolog query session with deterministic pagination
   - Parameter: `query` (string) - Prolog query to start
   - Example: `"member(X, [1,2,3])"` starts a backtracking session
   - Uses `call_nth/2` for deterministic solution retrieval

10. **`query_next`** - Get the next solution from current query (unified for both modes)
    - No parameters required
    - Returns next solution with deterministic backtracking
    - Memory efficient for large solution spaces

11. **`query_close`** - Close the current query session (unified for both modes)
    - No parameters required
    - Ends the query and frees resources

#### Query Tools - Engine Mode (SWI-Prolog Engines)
12. **`query_startEngine`** - Start a new Prolog query session using SWI-Prolog engines
    - Parameter: `query` (string) - Prolog query to start
    - Example: `"member(X, [1,2,3])"` starts an engine-based session
    - Uses true backtracking without recomputation
    - Uses unified `query_next` and `query_close` for iteration and cleanup


### Query Session Workflows

#### Standard Mode (call_nth/2)
Memory-efficient deterministic pagination:
```
1. query_start({query: "member(X, [1,2,3])"})  → "Query started, solutions available"
2. query_next()                                → "Solution: X = 1, more solutions: true"  
3. query_next()                                → "Solution: X = 2, more solutions: true"
4. query_next()                                → "Solution: X = 3, more solutions: false"
5. query_close()                               → "Query session closed"
```

#### Engine Mode (SWI-Prolog Engines)
True backtracking with iterator semantics:
```
1. query_startEngine({query: "member(X, [1,2,3])"}) → "Engine started, engine ready"
2. query_next()                               → "Solution: X = 1, more solutions: true"
3. query_next()                               → "Solution: X = 2, more solutions: true"  
4. query_next()                               → "Solution: X = 3, more solutions: true"
5. query_next()                               → "No more solutions available"
6. query_close()                              → "Engine session closed"

## MCP ↔ Prolog Wire Protocol

- Each line is a Prolog term in UTF‑8. The Node client tags requests and the Prolog server tags replies for correlation.
- Request envelope: `cmd(ID, Term)`; Response envelope: `id(ID, Reply)`.
  - Example: `cmd(1, start_query(member(X,[1,2,3])))` → `id(1, ok)`.
  - Example: `cmd(2, next_solution)` → `id(2, solution(['X'=1]))` … later `no_more_solutions`.
- Backward compatibility: bare terms without `cmd/2` are still accepted; replies to such requests are unwrapped. The Node client always uses envelopes.
- Reply shapes include: `ok`, `done`, `no_more_solutions`, `solution(Bindings)`, and `error(Term)` like `error(no_active_query)` or `error(session_conflict(Current, Requested))`.

See `src/prolog_server.pl` for dispatcher details and `src/PrologInterface.ts` for client-side correlation and parsing.

### Structured Outputs

- Tools generally return a human‑readable text item first, and a JSON item second for clients that prefer structured data.
- JSON is included for: `capabilities`, `query_start`, `query_startEngine`, `query_next`, `query_close`, `symbols_list`, `db_assert`, `db_retract`, `db_load`, `db_dump`, `help`, and `license`.

```

#### Session Mutual Exclusion
- Only one session type can be active at a time
- `query_start`/`query_startEngine` commands check for conflicts
- Must close current session before starting a different type

## Installation

### Production Use (NPM Package)

#### Prerequisites
- Node.js ≥ 18.0.0
- SWI-Prolog (tested with version 9.2.9)

#### Usage

**Direct execution with npx:**
```bash
npx @vpursuit/swipl-mcp-server
```

**Global installation:**
```bash
npm install -g @vpursuit/swipl-mcp-server
swipl-mcp-server
```

**Project dependency:**
```bash
npm install @vpursuit/swipl-mcp-server
npx swipl-mcp-server
```

### Development Setup

#### Prerequisites
- Node.js ≥ 18.0.0
- SWI-Prolog (tested with version 9.2.9)
- TypeScript

#### Setup

1. **Clone and install dependencies:**
   ```bash
   git clone <repository-url>
   cd swipl-mcp-server
   npm install
   ```

2. **Build the project:**
   ```bash
   npm run build
   ```

3. **Start the development server:**
   ```bash
   # Recommended: build and run in one command
   npm run server
   
   # Alternative: separate build and start
   npm start
   ```

## Package Deployment

### Development Build & Install

For development and testing, build and install directly from source:

```bash
# Build from TypeScript source
npm run build

# Create tarball from current directory
npm pack

# Install locally from tarball
npm install -g swipl-mcp-server-1.0.0.tgz

# Verify installation
which swipl-mcp-server
swipl-mcp-server --help
```

**Alternative development installation:**
```bash
# Create symlink for live development (changes reflect immediately)
npm link

# Unlink when done
npm unlink -g swipl-mcp-server
```

### Building Production Package

To create an optimized production package:

```bash
# Build optimized package
npm run build:package

# Test the package locally
npm pack dist/
tar -tzf vpursuit-swipl-mcp-server-*.tgz

# Test locally with npm link
cd dist && npm link
npx @vpursuit/swipl-mcp-server
```

### Publishing to NPM

```bash
# Build and publish
npm run build:package
npm publish dist/
```

### Package Structure

The production package contains only essential runtime files:

```
@vpursuit/swipl-mcp-server/
├── lib/                    # Compiled JS modules (52KB)
│   ├── index.js           # MCP server entry point
│   ├── PrologInterface.js # Process management
│   ├── tools.js          # Tool handlers
│   └── logger.js         # Logging utilities
├── prolog/
│   └── server.pl         # Prolog server script (15KB)
├── package.json          # Production configuration
├── README.md             # Documentation
└── LICENSE               # ISC license
```

**Size Optimization:**
- Development: ~94MB (includes node_modules, tests, docs)
- Production: ~85KB unpacked, ~21KB compressed
- 99.9% size reduction through selective packaging

## Development

### Available Scripts

- `npm test` - Run unit tests (fast, uses mocks)
- `npm run test:coverage` - Run tests with coverage report
- `npm run build` - Compile TypeScript
- `npm run server` - Build and start server
- `npm start` - Start pre-built server

### Publishing Scripts

- `npm run publish:prepare` - Create optimized distribution package in `dist/`
- `npm run publish:dry-run` - Test publishing process without actually publishing
- `npm run publish:npm` - Publish to npm registry (requires 2FA)

### Version Management Scripts

- `npm run version:patch` - Bump patch version (1.0.0 → 1.0.1) for bug fixes
- `npm run version:minor` - Bump minor version (1.0.0 → 1.1.0) for new features
- `npm run version:major` - Bump major version (1.0.0 → 2.0.0) for breaking changes
- `npm run version:prerelease` - Bump prerelease version (1.0.0 → 1.0.1-0) for alpha/beta

### GitHub Release Scripts

- `npm run github:release` - Create GitHub release from current git tag

### Complete Release Scripts (Version + npm + GitHub)

- `npm run release:patch --otp=<2FA-code>` - Patch release + npm publish + GitHub release
- `npm run release:minor --otp=<2FA-code>` - Minor release + npm publish + GitHub release
- `npm run release:major --otp=<2FA-code>` - Major release + npm publish + GitHub release
- `npm run release:prerelease --otp=<2FA-code>` - Prerelease + npm publish + GitHub release

#### Release Workflow

**Option 1: Manual Step-by-Step**
1. **Version bump**: `npm run version:patch` (creates git tag automatically)
2. **Test build**: `npm run publish:dry-run` 
3. **Publish**: `npm run publish:npm --otp=<2FA-code>`

**Option 2: Complete Automated Release**
1. **Full release**: `npm run release:patch --otp=<2FA-code>`
   - Bumps version + creates git tag
   - Builds optimized package in `dist/`
   - Publishes to npm registry
   - Creates GitHub release with automated changelog

#### Version Strategy (Semantic Versioning)
- **Patch** (1.0.1) - Bug fixes, documentation updates
- **Minor** (1.1.0) - New features, backward compatible changes
- **Major** (2.0.0) - Breaking changes, API modifications  
- **Prerelease** (1.0.1-0) - Alpha/beta versions for testing

#### Publishing Details
- Builds TypeScript to `build/`, optimized package to `dist/`
- Uses `README-npm.md` → `README.md` for npm
- Package published as `@vpursuit/swipl-mcp-server`
- Automatic git tagging with `npm version` commands
- GitHub releases with automated changelog generation

#### Prerequisites for Full Release
- **npm login** - Must be logged into npm registry
- **GitHub CLI** - Install with `brew install gh`, authenticate with `gh auth login`
- **Clean git repo** - All changes must be committed first

## Contributing

Please see CONTRIBUTING.md for local setup, workflow, and the PR checklist.

## Usage

### With MCP Inspector

#### Using NPM Package
```bash
# Launch inspector with production package
npx @modelcontextprotocol/inspector --transport stdio npx @vpursuit/swipl-mcp-server
```

#### Using Development Build
1. Start the server:
   ```bash
   npm run server
   ```

2. Launch MCP Inspector:
   ```bash
   npx @modelcontextprotocol/inspector --transport stdio node build/index.js
   ```

3. Open your browser and test the tools interactively

### With Claude Desktop

See the [Configuration](#configuration) section above for complete setup examples including environment variables. 

Basic configuration:
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

### Example Usage
For a larger set of end‑to‑end examples, see the Examples section below.

#### Load Knowledge Base
```json
// Tool: db_load
{
  "filename": "family.pl"
}
```

#### Step-by-Step Query
```json
// Tool: query_start
{
  "query": "parent(X, mary)"
}
// → "Query started, solutions available: true"

// Tool: query_next  
{}
// → "Solution: X = john, more solutions: true"

// Tool: query_next
{}
// → "Solution: X = alice, more solutions: false"

// Tool: query_close
{}
// → "Query session closed"
```

#### Add New Facts
```json
// Tool: db_assert
{
  "fact": "parent(peter, john)"
}
```

## Examples

For many more examples (loading files, list operations, arithmetic, collections, safe string/atom helpers, and engine mode flows), see:

- docs/examples.md — curated, copy‑pasteable MCP tool payloads with expected outputs

A few highlights:

- Filter even numbers with `between/3` and arithmetic:
  - Query: `findall(X, (between(1,10,X), 0 is X mod 2), L)` → `L = [2,4,6,8,10]`
- Collect with `findall/3` and `member/2`:
  - Query: `findall(X, member(X, [a,b,c]), L)` → `L = [a,b,c]`
- String/atom helpers (safe via library(sandbox)):
  - Query: `sub_atom('hello_world', 6, 5, 0, S)` → `S = world`
  - Query: `atom_string(A, "hello")` → `A = hello`
  - Query: `atom_concat("hello", "_world", S)` → `S = hello_world`

Note on formatting
- Depending on the environment, atoms/strings may appear quoted (`A="hello"`) or unquoted (`A=hello`), and whitespace around `=` may vary. Prefer pattern‑based matching (e.g., `/A\s*=\s*hello/`) instead of strict string equality. See docs/examples.md for more.

## Architecture

### Key Components

- **PrologInterface** - Manages SWI-Prolog process communication with session state tracking
- **Tools** - MCP tool handlers (query_start, db_assert, db_retract, engines, etc.)
- **prolog_server.pl** - Prolog server supporting both query modes

### Query Session Management

The server maintains a **single persistent Prolog process** with dual query modes:

#### Standard Mode (call_nth/2)
- Deterministic solution pagination
- Memory efficient for large solution spaces
- Uses `call_nth/2` for N-th solution retrieval
- No recomputation but may be slower for complex queries

#### Engine Mode (SWI-Prolog Engines)  
- True iterator semantics with SWI-Prolog engines
- No recomputation between `query_next` calls in engine mode
- Efficient for complex backtracking scenarios
- True Prolog backtracking preserved

#### State Management
- Mutual exclusion between query modes
- Session conflict detection and prevention
- Proper resource cleanup on session end

### Design Decisions

- **Singleton Pattern**: One Prolog process per MCP server instance
- **Term-based Protocol**: Structured Prolog terms instead of string parsing  
- **Unified Server**: Both query modes in single Prolog server implementation
- **Session State**: Maintained in Prolog using dynamic predicates with conflict detection
- **Error Handling**: Graceful degradation with meaningful error messages

## Security & Sandbox

- **Hybrid Security**: Combines `library(sandbox)` validation with additional safety measures:
  - Most built-ins validated by `library(sandbox)` (`sandbox:safe_goal/1`) 
  - Critical operations explicitly blocked: `call/1`, `assert/1`, `system/1`, `shell/1`, etc.
  - User-defined predicates in `kb` module allowed for recursive definitions
  - Unsafe goals rejected with `error(unsafe_goal(...))`
- **Safe consultation**: files are loaded via a guarded loader that only accepts facts and rules. Directives like `(:- ...)`, `(?- ...)`, `use_module/1`, `initialization/1`, and operator declarations are rejected.
- **Data module**: user content is asserted into `kb`; unknown predicates in `kb` fail by default.
- **Sessions**: mutual exclusion between query and engine modes; resources cleaned on close. Node-side timeouts protect against hung queries.
- **Protocol metadata**: Node wraps requests as `cmd(ID, Term)` and the server replies `id(ID, Reply)` for correlation; security checks apply to the inner `Term`.

## Troubleshooting

### Common Issues

**Server won't start:**
- Check SWI-Prolog installation: `swipl --version`
- Ensure all dependencies are installed: `npm install`
- Verify build completed: `npm run build`

**Query sessions don't work:**
- Integration tests required for full session testing
- Unit tests use mocks and can't test persistent state
- Ensure server stays running between requests

**Tests fail:**
- Unit tests: Check SWI-Prolog installation
- Integration tests: Ensure server is running in separate terminal

## Technical Status

- ✅ **Unified Prolog server supporting both query modes**
- ✅ **Mutual exclusion and session conflict detection**
- ✅ **Engine-based true backtracking implementation**
- ✅ **Term-based protocol with structured parsing**
- ✅ **Comprehensive edge case testing**
- ✅ **Complete tool coverage (12 tools total)**
- ✅ **Memory management and resource cleanup**

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `npm test`
5. Run comprehensive test suite: `npm test`
6. Submit a pull request
7. To see more details run "DEBUG=swipl-mcp-server MCP_LOG_LEVEL=debug npm test"

For security practices, reporting, and hardening guidance, see `SECURITY.md`.

## License

ISC License - see LICENSE file for details.

## Acknowledgments

Built with:
- [TypeScript](https://www.typescriptlang.org/)
- [Model Context Protocol SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [SWI-Prolog](https://www.swi-prolog.org/)
- [Jest](https://jestjs.io/)
