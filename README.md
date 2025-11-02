# MCP Ecosystem by vpursuit

[![Build Status](https://github.com/vpursuit/swipl-mcp-server/actions/workflows/npm-publish.yml/badge.svg)](https://github.com/vpursuit/swipl-mcp-server/actions/workflows/npm-publish.yml)

This monorepo contains Model Context Protocol (MCP) packages and products that enable AI assistants to work with SWI-Prolog, filesystems, and extensible plugin systems.

## 🚀 Products

### SWI-Prolog MCP Server

**Full-featured MCP server with Prolog knowledge base integration**

An MCP server that lets tools-enabled LLMs work directly with SWI‑Prolog. It supports loading Prolog files, adding/removing facts and rules, listing symbols, and running queries with two modes: deterministic pagination and true engine backtracking.

- 📦 **NPM**: [`@vpursuit/swipl-mcp-server`](https://www.npmjs.com/package/@vpursuit/swipl-mcp-server)
- 📖 **Documentation**: [packages/swipl-mcp-server](./packages/swipl-mcp-server)
- 🎯 **Quick Start**: `npx @vpursuit/swipl-mcp-server`

**Features:**
- Knowledge base management (load, assert, retract, dump)
- Two query modes: standard (`call_nth/2`) and engine (true backtracking)
- Expert Prolog assistance prompts
- Comprehensive security sandboxing
- Dynamic filesystem roots
- Plugin-based architecture

## 🧱 Reusable Libraries

Build your own MCP servers using these composable packages:

| Package | Description | Version | NPM |
|---------|-------------|---------|-----|
| [`@vpursuit/mcp-server-core`](./packages/mcp-core) | Plugin system for MCP servers | 1.0.0 | [npm](https://www.npmjs.com/package/@vpursuit/mcp-server-core) |
| [`@vpursuit/mcp-server-prolog`](./packages/mcp-prolog) | SWI-Prolog integration plugin | 3.0.0 | [npm](https://www.npmjs.com/package/@vpursuit/mcp-server-prolog) |
| [`@vpursuit/mcp-server-roots`](./packages/mcp-roots) | Dynamic filesystem roots management | 1.0.0 | [npm](https://www.npmjs.com/package/@vpursuit/mcp-server-roots) |

### Using the Libraries

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
swipl-mcp-server/
├── packages/
│   ├── swipl-mcp-server/    # Main MCP server product (orchestrator)
│   ├── mcp-core/            # Plugin system for MCP servers
│   ├── mcp-prolog/          # SWI-Prolog integration plugin
│   └── mcp-roots/           # Filesystem roots discovery plugin
├── docs/                    # Monorepo-level documentation
├── .archive/                # Historical strategy documents
└── package.json            # Workspace configuration
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
git clone https://github.com/vpursuit/swipl-mcp-server.git
cd swipl-mcp-server

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
npm run build -w packages/mcp-core

# Test specific package
npm test -w packages/mcp-prolog

# Clean all build artifacts
npm run clean
```

### Package Development

Each package supports:
- `npm run build` - TypeScript compilation
- `npm run clean` - Remove build artifacts
- `npm test` - Run Vitest tests
- `npm run test:watch` - Watch mode for tests

## 📝 Documentation

### Product Documentation (swipl-mcp-server)
- **[Installation Guide](./packages/swipl-mcp-server/docs/installation.md)** — Complete setup for all MCP clients
- **[Features Reference](./packages/swipl-mcp-server/docs/features.md)** — Detailed prompts, resources, and tools documentation
- **[Examples](./packages/swipl-mcp-server/docs/examples.md)** — Copy-paste usage examples
- **[Architecture](./packages/swipl-mcp-server/docs/architecture.md)** — Components, modes, and wire protocol
- **[Lifecycle](./packages/swipl-mcp-server/docs/lifecycle.md)** — Server lifecycle, state, and persistence patterns
- **[Deployment](./packages/swipl-mcp-server/docs/deployment.md)** — Release, packaging, and install from source

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

Each package is published independently to NPM under the `@vpursuit` scope:

- Packages use semantic versioning independently
- Releases are tagged as `v<version>` for main product, `<package-name>-v<version>` for libraries
- Automated publishing via GitHub Actions
- See [PUBLISHING.md](./PUBLISHING.md) for details

## 🔒 Security

All packages implement security best practices:
- File path restrictions
- Dangerous predicate blocking
- Pre-execution validation
- Timeout protection
- Module isolation

See [SECURITY.md](./SECURITY.md) for complete security documentation and reporting.

## 📄 License

All packages in this monorepo are licensed under **BSD-3-Clause**.

See [LICENSE](./LICENSE) file for details.

## 🔗 Links

- **GitHub**: [vpursuit/swipl-mcp-server](https://github.com/vpursuit/swipl-mcp-server)
- **NPM Organization**: [@vpursuit](https://www.npmjs.com/org/vpursuit)
- **Model Context Protocol**: [modelcontextprotocol.io](https://modelcontextprotocol.io)
- **SWI-Prolog**: [swi-prolog.org](https://www.swi-prolog.org)

## 💡 Getting Started

**For end users**: Install the complete MCP server
```bash
npx @vpursuit/swipl-mcp-server
```

**For developers**: Build custom MCP servers using the plugin libraries
```bash
npm install @vpursuit/mcp-server-core @vpursuit/mcp-server-prolog @vpursuit/mcp-server-roots
```

**For contributors**: Set up the development environment
```bash
git clone https://github.com/vpursuit/swipl-mcp-server.git
cd swipl-mcp-server
npm install
npm run build
npm test
```

---

**Questions?** Open an [issue](https://github.com/vpursuit/swipl-mcp-server/issues) or see our [documentation](./packages/swipl-mcp-server#readme).
