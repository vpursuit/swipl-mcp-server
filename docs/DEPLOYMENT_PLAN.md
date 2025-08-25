# 📦 SWI-Prolog MCP Server - Scoped NPM Package Deployment Plan

## 🎯 Package Identity
- **Scope:** `@vpursuit`
- **Package Name:** `@vpursuit/swipl-mcp-server`
- **NPX Usage:** `npx @vpursuit/swipl-mcp-server`

## 🔍 Current State Analysis

**Package Size Breakdown:**
- Total: 94M (mostly node_modules: 93M)
- **Essential Runtime Files:** ~85K
  - `build/` directory: 60K (compiled JS)
  - `prolog_server.pl`: 15K (428 lines)
  - `package.json`: 1K
- **Excludable Content:** 93.7M
  - `node_modules/`: 93M
  - `test/`: 132K  
  - `coverage/`: 268K
  - `docs/`, `.claude/`, etc.: ~300K

**Dependencies:**
- **Runtime:** `@modelcontextprotocol/sdk`, `zod` (lightweight)
- **Dev:** TypeScript, Jest, types (excluded from distribution)

## 🏗️ Scoped Package Architecture Design

### 1. Package Identity & Structure
```
@vpursuit/swipl-mcp-server/
├── lib/                           # Compiled JS modules  
│   ├── index.js                   # Main entry point
│   ├── PrologInterface.js
│   ├── tools.js
│   └── logger.js
├── prolog/
│   └── server_unified.pl          # Prolog server script
├── package.json                   # Scoped package config
├── README.md                      # Usage documentation
└── LICENSE                        # ISC license
```

### 2. Entry Points & Package Design
```json
{
  "name": "@vpursuit/swipl-mcp-server",
  "main": "lib/index.js",
  "bin": "lib/index.js",
  "exports": {
    ".": "./lib/index.js",
    "./package.json": "./package.json"
  },
  "files": ["lib", "prolog", "README.md", "LICENSE"]
}
```

## 🛠️ Implementation Roadmap

### Phase 1: Build System & Package Configuration

**1.1 Production Build Script**
- Create `scripts/build-package.js` for optimized compilation
- Tree-shaking and minification for smaller bundle
- Copy essential files to `dist/` directory

**1.2 Package.json Enhancement**
```json
{
  "name": "@vpursuit/swipl-mcp-server", 
  "version": "1.0.0",
  "description": "SWI-Prolog MCP Server with dual query modes and security layer",
  "keywords": ["mcp", "prolog", "swi-prolog", "swipl", "model-context-protocol"],
  "files": ["lib", "prolog", "README.md", "LICENSE"],
  "engines": { "node": ">=18.0.0" },
  "publishConfig": { "access": "public" }
}
```

### Phase 2: MCP Server Implementation

**2.1 Direct Entry Point**
- Simple `node lib/index.js` execution
- MCP protocol communication via stdin/stdout
- No CLI argument parsing needed (MCP standard)

**2.2 Runtime Path Resolution**
- Dynamic path resolution for `prolog_server.pl`
- Package installation detection
- Environment variable configuration support

### Phase 3: Package Optimization

**3.1 File Exclusion (.npmignore)**
```
# Development files
src/
test/
coverage/
docs/
.claude/

# Config files
jest.config.js
tsconfig.json
.gitignore

# Build artifacts not needed
*.log
*.tgz
```

**3.2 Dependency Optimization**
- Move TypeScript compilation to `prepublishOnly`
- Ensure only runtime dependencies in production
- Optional peer dependencies for development tools

### Phase 4: Distribution & Testing

**4.1 Local Testing Strategy**
```bash
# Test packaging
npm pack
tar -tzf vpursuit-swipl-mcp-server-*.tgz

# Test npx functionality
npm link
npx @vpursuit/swipl-mcp-server
```

**4.2 Publishing Preparation**
- Automated version bumping
- Pre-publish tests and validation
- CI/CD integration for automated releases

## 🎯 Key Features & Benefits

### For End Users:
- **Simple Installation:** `npx @vpursuit/swipl-mcp-server`
- **Zero Config:** Works out of the box with sensible defaults
- **Lightweight:** <1MB download (vs 94M dev environment)
- **Cross-Platform:** Unix/Windows compatibility

### For Developers:
- **Security Hardened:** Built-in query whitelisting and sandboxing
- **Dual Query Modes:** Standard call_nth/2 and SWI-Prolog engines
- **Full MCP Compliance:** 10 tools, proper error handling
- **Production Ready:** Comprehensive logging and debugging

## 📋 Success Criteria

1. **Package Size:** <1MB distributed package
2. **Installation Time:** <30 seconds via npx
3. **Zero Dependencies Issues:** Clean installation on fresh systems  
4. **MCP Protocol:** Full compatibility with Claude Desktop and MCP clients
5. **Documentation:** Clear usage examples and troubleshooting
6. **Platform Support:** macOS, Linux, Windows (with SWI-Prolog installed)

## 🚀 Execution Status

### Phase 1: Build System & Package Configuration ⏳
- [ ] Create production build script
- [ ] Configure package.json for @vpursuit scope
- [ ] Set up optimized compilation pipeline

### Phase 2: CLI Implementation ⏳  
- [ ] Implement CLI entry point with argument parsing
- [ ] Add runtime path resolution for distributed package
- [ ] Support configuration options and help commands

### Phase 3: Package Optimization ⏳
- [ ] Create .npmignore for minimal package size
- [ ] Optimize dependencies for production
- [ ] Implement prepublish hooks

### Phase 4: Distribution & Testing ⏳
- [ ] Test local packaging and npx functionality
- [ ] Validate cross-platform compatibility
- [ ] Prepare for npm registry publishing

**Estimated Timeline:** 4-6 hours for complete transformation  
**Target Package Size:** <1MB (99% size reduction)
**NPX Command:** `npx @vpursuit/swipl-mcp-server`

## 🔄 Next Steps

Ready to proceed with Phase 1: Build System & Package Configuration implementation.