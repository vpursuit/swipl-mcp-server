# SWI-Prolog MCP Server Documentation

Complete documentation for the SWI-Prolog MCP Server.

## Quick Start

- **[Installation & Setup](../README.md#installation)** - Getting started with Claude Desktop, Claude Code CLI, and other clients
- **[Available Prompts](../README.md#available-prompts)** - Expert Prolog assistance prompts
- **[Available Tools](../README.md#available-tools)** - Knowledge base management and query tools

## Core Documentation

### [Features Reference](./features.md)
Complete reference for all server capabilities:
- MCP prompts (expert, knowledge, optimize, puzzle)
- MCP resources (knowledge base, help, license, capabilities)
- MCP tools (core, knowledge base management, queries, symbols)
- Session state management
- Security model

### [Configuration](./configuration.md)
Complete configuration reference:
- Filesystem roots setup (MCP client vs environment variable)
- Environment variable reference
- Timeout configuration
- Logging configuration
- Client-specific examples
- Configuration verification and troubleshooting

### [Architecture](./architecture.md)
System architecture and design:
- Two-module design (prolog_server vs knowledge_base)
- Operators vs predicates
- Query processing pipeline
- Safe library loading
- Security architecture
- Wire protocol
- Common questions

## Operational Guides

### [Logging & Debugging](./logging.md)
Comprehensive debugging guide:
- Log levels (silent, error, warn, info, debug)
- Debug namespace configuration
- Wire protocol tracing
- Common debugging scenarios
- Log output examples
- Interpreting trace output

### [Lifecycle](./lifecycle.md)
Server lifecycle and state management:
- Session state machine
- State transitions
- Query mode coordination
- Best practices

### [Examples](./examples.md)
Practical usage examples:
- Arithmetic and comparison
- List operations
- Control structures
- Collection operations
- String and atom manipulation

## Development & Deployment

### [Deployment](./deployment.md)
Quick reference for releases:
- Release commands (patch, minor, major)
- Pre-release checklist
- Version guidelines
- See [PUBLISHING.md](../../../PUBLISHING.md) for complete guide

## Project Documentation

### Root-Level Documentation
- **[PUBLISHING.md](../../../PUBLISHING.md)** - Complete publishing guide (tag-based, scripts, OIDC, troubleshooting)
- **[SECURITY.md](../../../SECURITY.md)** - Security policy, vulnerability reporting, hardening checklist
- **[CONTRIBUTING.md](../../../CONTRIBUTING.md)** - Development workflow, testing, pull requests
- **[README.md](../../../README.md)** - Monorepo overview, architecture, development setup

### Product Documentation
- **[README.md](../README.md)** - Main product README with installation, configuration, and usage

## Documentation by Use Case

### For End Users
1. Start with [Installation](../README.md#installation)
2. Configure [Filesystem Roots](./configuration.md#filesystem-roots)
3. Review [Available Prompts](../README.md#available-prompts) and [Tools](../README.md#available-tools)
4. Explore [Examples](./examples.md)
5. Check [Troubleshooting](../README.md#troubleshooting) if issues arise

### For Developers
1. Read [Architecture](./architecture.md) to understand system design
2. Review [Plugin Implementation](../../../plugins/server/prolog/ARCHITECTURE.md) for internals
3. Follow [Contributing Guidelines](../../../CONTRIBUTING.md)
4. See [Deployment Guide](./deployment.md) for releases

### For Troubleshooting
1. Enable [Debug Logging](./logging.md#debug-namespace)
2. Check [Configuration](./configuration.md#configuration-verification)
3. Review [Common Scenarios](./logging.md#common-debugging-scenarios)
4. See [Troubleshooting Section](../README.md#troubleshooting)

### For Security Auditing
1. Read [Security Policy](../../../SECURITY.md#swi-prolog-mcp-server-security)
2. Review [Threat Model](../../../SECURITY.md#threat-model)
3. Examine [Built-in Features](../../../SECURITY.md#built-in-security-features)
4. Check [Hardening Checklist](../../../SECURITY.md#hardening-checklist-for-prolog-server)

## Documentation Standards

All documentation in this repository follows these principles:
- **DRY (Don't Repeat Yourself)** - Single source of truth for each concept
- **Hierarchical Structure** - Root for policy, product for features, docs for details
- **Cross-Referenced** - Related documents link to each other
- **User-Focused** - Written for end users first, developers second
- **Example-Driven** - Practical examples for all features

## Contributing to Documentation

Found an issue or want to improve documentation?
1. Check [Contributing Guidelines](../../../CONTRIBUTING.md)
2. Open an issue or submit a pull request
3. Follow documentation standards above

## Version Information

This documentation corresponds to:
- **Product**: @vpursuit/swipl-mcp-server
- **Version**: See [package.json](../package.json)
- **License**: BSD-3-Clause

For older versions, see [GitHub Releases](https://github.com/vpursuit/model-context-lab/releases).
