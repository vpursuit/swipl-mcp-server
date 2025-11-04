# MCP Ecosystem by vpursuit

[![Build Status](https://github.com/vpursuit/model-context-lab/actions/workflows/npm-publish.yml/badge.svg)](https://github.com/vpursuit/model-context-lab/actions/workflows/npm-publish.yml)

This monorepo contains Model Context Protocol (MCP) packages and products that enable AI assistants to work with SWI-Prolog, filesystems, and extensible plugin systems.

## 🚀 Products

### SWI-Prolog MCP Server

**Full-featured MCP server with Prolog knowledge base integration**

An MCP server that lets tools-enabled LLMs work directly with SWI‑Prolog. It supports loading Prolog files, adding/removing facts and rules, listing symbols, and running queries with two modes: deterministic pagination and true engine backtracking.

- 📦 **NPM**: [`@vpursuit/swipl-mcp-server`](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server)
- 📖 **Documentation**: [products/swipl-mcp-server](./products/swipl-mcp-server)
- 🎯 **Quick Start**: `npx @vpursuit/swipl-mcp-server`

**Features:**
- Knowledge base management (load, assert, retract, dump)
- Two query modes: standard (`call_nth/2`) and engine (true backtracking)
- Expert Prolog assistance prompts
- Comprehensive security sandboxing
- Dynamic filesystem roots
- Plugin-based architecture

## 🧩 Architecture

This repository follows a **products/plugins** architecture:

- **Products** (`products/`): Published packages that end-users install (e.g., `@vpursuit/swipl-mcp-server`)
- **Plugins** (`plugins/`): Internal, reusable components bundled within products (not published separately)

### Internal Plugin System

The MCP server is built with a modular plugin architecture. These plugins are **internal dependencies** bundled into the main product:

| Plugin | Description | Location |
|--------|-------------|----------|
| `@vpursuit/mcp-server-core` | Plugin system foundation | [plugins/server/core](./plugins/server/core) |
| `@vpursuit/mcp-server-prolog` | SWI-Prolog integration | [plugins/server/prolog](./plugins/server/prolog) |
| `@vpursuit/mcp-server-roots` | Filesystem roots discovery | [plugins/server/roots](./plugins/server/roots) |

**Note:** These plugins are marked as `private` in their `package.json` and are bundled into `@vpursuit/swipl-mcp-server`. They are not published separately to npm.

### Plugin System for Developers

If you're developing within this monorepo, you can use the plugin system directly:

```typescript
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { loadPlugins } from '@vpursuit/mcp-server-core';
import { plugin as prologPlugin } from '@vpursuit/mcp-server-prolog';
import { plugin as rootsPlugin } from '@vpursuit/mcp-server-roots';

const server = new McpServer({
  name: 'my-mcp-server',
  version: '1.0.0',
});

// Load plugins
await loadPlugins(server, [prologPlugin, rootsPlugin]);
```

## 📂 Repository Structure

This is a monorepo managed with **npm workspaces**. Each package can be developed, tested, and published independently.

```
model-context-lab/
├── products/
│   └── swipl-mcp-server/    # Main MCP server (published to npm)
├── plugins/
│   └── server/
│       ├── core/            # Plugin system foundation (internal)
│       ├── prolog/          # SWI-Prolog integration (internal)
│       └── roots/           # Filesystem roots discovery (internal)
├── docs/                    # Monorepo-level documentation
├── .archive/                # Historical strategy documents
└── package.json             # Workspace configuration
```

Each package has:
- Own `package.json` with independent versioning
- Own `README.md` with complete documentation
- Own `LICENSE` (BSD-3-Clause)
- Own test suite

## 🔧 Development

### Prerequisites

- Node.js ≥ 20.0.0
- SWI-Prolog (for testing Prolog integration)
- npm ≥ 9.0.0

### Setup

```bash
# Clone repository
git clone https://github.com/vpursuit/model-context-lab.git
cd model-context-lab

# Install all dependencies
npm install

# Build all packages
npm run build

# Run all tests
npm test
```

### Working with Packages

```bash
# Build specific package
npm run build -w plugins/server/core

# Test specific package
npm test -w plugins/server/prolog

# Clean all build artifacts
npm run clean
```

### Package Development

Each package supports:
- `npm run build` - TypeScript compilation
- `npm run clean` - Remove build artifacts
- `npm test` - Run Vitest tests
- `npm run test:watch` - Watch mode for tests

## Documentation

### Product Documentation (swipl-mcp-server)
- **[Installation & Setup](./products/swipl-mcp-server/README.md#installation)** — Complete setup for all MCP clients
- **[Configuration](./products/swipl-mcp-server/README.md#configuration)** — Filesystem roots, environment variables
- **[Troubleshooting](./products/swipl-mcp-server/README.md#troubleshooting)** — Common issues and debug mode
- **[Features Reference](./products/swipl-mcp-server/docs/features.md)** — Detailed prompts, resources, and tools documentation
- **[Examples](./products/swipl-mcp-server/docs/examples.md)** — Copy-paste usage examples
- **[Architecture](./products/swipl-mcp-server/docs/architecture.md)** — Components, modes, and wire protocol
- **[Lifecycle](./products/swipl-mcp-server/docs/lifecycle.md)** — Server lifecycle, state, and persistence patterns
- **[Deployment](./products/swipl-mcp-server/docs/deployment.md)** — Release, packaging, and install from source

### Monorepo Documentation
- **[Publishing Guide](./PUBLISHING.md)** — How to publish packages to npm
- **[Contributing](./CONTRIBUTING.md)** — Development workflow and guidelines
- **[Security](./SECURITY.md)** — Security policies and vulnerability reporting

## 🤝 Contributing

We welcome contributions! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for:
- Code of conduct
- Development workflow
- Testing requirements
- Pull request process
- Coding standards

For security issues, see [SECURITY.md](./SECURITY.md).

## 📦 Publishing

Only **products** are published to npm under the `@vpursuit` scope:

- **Products** (e.g., `@vpursuit/swipl-mcp-server`) are published to npm for end users
- **Plugins** are internal dependencies bundled within products (not published separately)
- Releases use semantic versioning: `v<version>` (e.g., `v3.0.0`)
- Automated publishing via GitHub Actions
- **Supply chain security**: All packages published with npm provenance attestation
- See [PUBLISHING.md](./PUBLISHING.md) for complete details

## 🔒 Security

All packages implement security best practices:
- File path restrictions
- Dangerous predicate blocking
- Pre-execution validation
- Timeout protection
- Module isolation

**Supply Chain Security:**
- Published with npm provenance attestation for build transparency
- OIDC-based publishing (no long-lived tokens)
- Cryptographically signed packages via Sigstore

See [SECURITY.md](./SECURITY.md) for complete security documentation and reporting.

## 📄 License

All packages in this monorepo are licensed under **BSD-3-Clause**.

See [LICENSE](./LICENSE) file for details.

## 🔗 Links

- **GitHub**: [vpursuit/model-context-lab](https://github.com/vpursuit/model-context-lab)
- **NPM Organization**: [@vpursuit](https://www.npmjs.com/org/vpursuit)
- **Model Context Protocol**: [modelcontextprotocol.io](https://modelcontextprotocol.io)
- **SWI-Prolog**: [swi-prolog.org](https://www.swi-prolog.org)

## 💡 Getting Started

**For end users**: Install the complete MCP server
```bash
npx @vpursuit/swipl-mcp-server
```

**For monorepo developers**: Work with the plugin system
```bash
git clone https://github.com/vpursuit/model-context-lab.git
cd model-context-lab
npm install
npm run build
```

**For contributors**: Set up the development environment
```bash
git clone https://github.com/vpursuit/model-context-lab.git
cd model-context-lab
npm install
npm run build
npm test
```

---

**Questions?** Open an [issue](https://github.com/vpursuit/model-context-lab/issues) or see our [documentation](./products/swipl-mcp-server#readme).
