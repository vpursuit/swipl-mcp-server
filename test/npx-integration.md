# NPX Integration Testing

## Overview

The `npx-integration.test.js` provides comprehensive testing for real-world npx usage scenarios. This test catches issues that unit tests miss by simulating the actual npm package installation and execution flow.

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
  { name: 'db_assert test', /* Prolog server startup */ }
];
```

## Running the Tests

```bash
# Run npx integration test only
npm run test:npx

# Run all tests (unit + integration)
npm run test:all

# Integration test runs automatically before publish
npm run prepublishOnly
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