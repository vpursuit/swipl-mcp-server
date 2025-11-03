# swipl-mcp-server Manual Test Scripts

Quick manual testing tools for the swipl-mcp-server package.

## Prerequisites

- Build the package first: `npm run build` (from package directory)
- SWI-Prolog must be installed and available in PATH

## Scripts

### mcp_quickcheck.mjs

Quick health check that verifies the server starts and lists available tools.

**Usage:**
```bash
cd products/swipl-mcp-server
node scripts/mcp_quickcheck.mjs
```

**What it tests:**
- Server startup via stdio transport
- Connection establishment
- Tool listing

**Expected output:**
```
TOOLS: knowledge_base_load, knowledge_base_assert, knowledge_base_assert_many, ...
```

### mcp_prolog_demo.mjs

Comprehensive demo showing MCP client interaction with the server.

**Usage:**
```bash
cd products/swipl-mcp-server
node scripts/mcp_prolog_demo.mjs
```

**What it demonstrates:**
- Asserting facts and rules
- Starting queries
- Fetching multiple solutions
- Closing query sessions
- Complete knowledge base workflow

**Expected output:**
```
=== Connecting to MCP server ===
=== Listing tools ===
knowledge_base_load, knowledge_base_assert, ...

=== tools/call: knowledge_base_assert_many ===
✅ Successfully asserted 5 clauses

=== tools/call: query_start ===
Query started successfully (ID: 1)

=== tools/call: query_next ===
Solution 1: parent(john, mary)
...
```

## When to Use

**Use these scripts for:**
- Quick manual verification after code changes
- Debugging MCP protocol issues
- Demonstrating server capabilities
- Interactive testing during development

**Use E2E tests instead for:**
- Automated CI/CD testing (`npm test`)
- Comprehensive test coverage
- NPX installation validation
- Regression testing

## Troubleshooting

**Server fails to start:**
- Check that `dist/index.js` exists (`npm run build`)
- Verify SWI-Prolog is installed: `swipl --version`
- Check stderr output for error messages

**Connection timeout:**
- Server may still be starting (increase timeout)
- Check for port conflicts or process issues

**Tool calls fail:**
- Review tool names with quickcheck first
- Check tool input schemas match expectations
- Look at server stderr for validation errors

## Alternative: MCP Inspector

For interactive testing with a GUI, use the official MCP Inspector:

```bash
npx @modelcontextprotocol/inspector node dist/index.js
```

This provides a web-based interface for exploring tools, resources, and prompts.
