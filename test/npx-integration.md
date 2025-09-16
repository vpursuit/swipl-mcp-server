# NPX Integration Testing

## Overview

The NPX integration testing provides comprehensive testing for real-world npx usage scenarios. This test catches issues that unit tests miss by simulating the actual npm package installation and execution flow.

**Two ways to run NPX integration tests:**
1. **Standalone script**: `test/npx-integration.test.js` - Used by CI and publishing
2. **Vitest integration**: `test/integration/npx.test.ts` - Part of the unified test suite

## What it Tests

### 1. Package Installation
- Creates a real npm package tarball from `dist/`
- Installs the package in an isolated environment
- Verifies the package can be found via `npx`

### 2. Path Resolution
- Tests that `LICENSE` file is found in installed package
- Verifies `server.pl` (Prolog script) is located correctly
- Ensures `package.json` version resolution works

### 3. MCP Protocol Functionality
- **License Tool**: Verifies license file is readable and contains expected content
- **Help Tool**: Tests basic tool functionality
- **DB Assert Tool**: Confirms Prolog server starts correctly
- **Tools List**: Validates all tools are properly registered

### 4. Environment Isolation
- Uses temporary directories to avoid conflicts
- Sets appropriate environment variables
- Cleans up after each test run

## Key Test Cases

```javascript
const tests = [
  { name: 'capabilities', /* initialize MCP */ },
  { name: 'tools/list', /* list all available tools */ },
  { name: 'license tool', /* test file resolution */ },
  { name: 'help tool', /* basic functionality */ },
  { name: 'knowledge_base_assert test', /* Prolog server startup */ }
];
```

## Running the Tests

### Standalone NPX Test (CI/Publishing)
```bash
# Run standalone npx integration test only (used by CI)
npm run test:npx

# Run all tests including standalone npx (used by CI)
npm run test:all

# Standalone test runs automatically before publish
npm run prepublishOnly
```

### Unified Test Suite
```bash
# Run all unit tests (fast, no SWI-Prolog required)
npm run test:unit

# Run all integration tests including NPX (requires SWI-Prolog)
npm run test:integration

# Run NPX tests only within Vitest framework
npm run test:integration:npx

# Run everything (unit + integration tests)
npm test
```

### Test Structure
```
test/
├── unit/                          # Unit tests (no SWI-Prolog)
├── integration/                   # Integration tests (requires SWI-Prolog)
│   ├── npx.test.ts               # ⭐ NPX tests in Vitest framework
│   └── [...other integration...]
├── npx-integration.test.js        # 🔧 Standalone script for CI
└── setup.js                      # Test configuration
```

## Why This Matters

### Problems This Test Catches

1. **File Path Resolution**: Unit tests run from the source directory, but npx runs from `node_modules`. Path resolution bugs only appear in the real environment.

2. **Package Structure**: The test verifies the built package has the correct structure and all necessary files.

3. **Environment Variables**: Real npx usage may have different environment variables than development.

4. **Process Spawning**: Tests that SWI-Prolog can be spawned correctly from an installed package.

5. **MCP Protocol**: Validates the entire JSON-RPC protocol works end-to-end.

### Real-World Simulation

The test creates a scenario that closely matches what happens when users run:
```bash
npx @modelcontextprotocol/inspector --transport stdio npx @vpursuit/swipl-mcp-server
```

## Architecture

```
NPXIntegrationTest
├── setup()           # Build package, create temp environment
├── installPackage()  # npm install in isolated directory
├── testMCPProtocol() # Run MCP commands via spawned process
├── runMCPCommand()   # JSON-RPC communication with timeout
└── cleanup()         # Remove temp files and uninstall
```

## Error Scenarios Tested

- **Timeout**: Commands that hang or take too long
- **Process Errors**: SWI-Prolog not found, script errors
- **Protocol Errors**: Invalid JSON-RPC responses
- **File Not Found**: Missing LICENSE, server.pl, or package.json
- **Server Startup**: Prolog server initialization failures

## CI Integration

The test is integrated into the build process:
- Runs before every npm publish (`prepublishOnly`)
- Can be run independently for debugging
- Provides detailed error reporting for troubleshooting

This ensures that every published version has been verified to work correctly via npx.