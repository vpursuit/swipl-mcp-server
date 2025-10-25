# MCP Logging Infrastructure

## Overview

This project implements a comprehensive logging system that uses the Model Context Protocol (MCP) specification for proper client-visible logging. All logs are sent via MCP protocol notifications and appear in the MCP Inspector UI.

## Architecture

### How It Works

```
┌─────────────────────────────────────────┐
│  MCP Server Logging Flow                │
├─────────────────────────────────────────┤
│                                         │
│  Plugin Logs (mcp-core, mcp-prolog,    │
│               mcp-roots)                │
│         ↓                               │
│  createMcpLogger()                      │
│         ↓                               │
│  ┌─────────────────┐                   │
│  │ Server          │                   │
│  │ Connected?      │                   │
│  └────┬────────┬───┘                   │
│       │        │                        │
│      YES       NO                       │
│       │        │                        │
│       ↓        ↓                        │
│  sendLogging   console.error            │
│  Message()     (stderr)                 │
│       │                                 │
│       ↓                                 │
│  notifications/message                  │
│  (MCP Protocol)                         │
│       │                                 │
│       ↓                                 │
│  📊 Inspector Logs Panel                │
│                                         │
└─────────────────────────────────────────┘
```

### Key Components

1. **`packages/mcp-core/src/logger.ts`** - Shared MCP-aware logger
   - Creates loggers that use MCP protocol when connected
   - Falls back to stderr when server not available
   - Supports all 8 MCP logging levels

2. **Plugin Loggers** - Each plugin has its own logger instance
   - `packages/mcp-prolog/src/logger.ts` - Logger for Prolog plugin
   - `packages/mcp-roots/src/logger.ts` - Logger for Roots plugin
   - Both wrap the core MCP logger with package-specific names

3. **Server Configuration** - `packages/swipl-mcp-server/src/index.ts`
   - Declares `capabilities: { logging: {} }` to enable MCP logging
   - Plugins set their serverRef in onInit hooks

## Log Levels

The MCP specification defines 8 standard logging levels (from syslog):

| Level     | Priority | Usage |
|-----------|----------|-------|
| debug     | 0        | Detailed debugging information |
| info      | 1        | General informational messages |
| notice    | 2        | Normal but significant events |
| warning   | 3        | Warning messages |
| error     | 4        | Error conditions |
| critical  | 5        | Critical conditions |
| alert     | 6        | Action must be taken immediately |
| emergency | 7        | System is unusable |

## Usage in Code

### Basic Logging

```typescript
import { logger } from "./logger.js";

// Info level
logger.info("Server started");

// With structured data
logger.info("User connected", { userId: "123", ip: "192.168.1.1" });

// Warning
logger.warn("High memory usage detected", { usage: "85%" });

// Error with Error object
try {
  // ...
} catch (error) {
  logger.error("Failed to process request", error);
}

// Debug (verbose)
logger.debug("Processing query", { query: "member(X, [1,2,3])" });
```

### Creating a Logger for a New Plugin

```typescript
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { createMcpLogger, type McpLogger } from "@vpursuit/mcp-core";

// Shared server reference for MCP logging
export const serverRef: { current: McpServer | null } = { current: null };

// Create MCP-aware logger with package name
const mcpLogger: McpLogger = createMcpLogger("my-plugin", serverRef);

export const logger = {
  debug(msg: string, data?: Record<string, unknown>) {
    mcpLogger.debug(msg, data);
  },
  info(msg: string, data?: Record<string, unknown>) {
    mcpLogger.info(msg, data);
  },
  warn(msg: string, data?: Record<string, unknown>) {
    mcpLogger.warn(msg, data);
  },
  error(msg: string, errorOrData?: Error | Record<string, unknown>) {
    mcpLogger.error(msg, errorOrData);
  },
};
```

### Setting Server Reference in Plugin

```typescript
import { serverRef, logger } from "./logger.js";

export const plugin: Plugin = {
  name: "my-plugin",
  // ...

  async onInit(server) {
    // Set server reference for MCP-aware logging
    serverRef.current = server;

    logger.info("Plugin initialized");
  },
};
```

## Environment Variables (Future Enhancement)

### Planned: `MCP_LOG_LEVEL` Environment Variable

Control log verbosity via environment variable:

```bash
# Show all logs including debug
MCP_LOG_LEVEL=debug npx @vpursuit/swipl-mcp-server

# Show only info and above (default)
MCP_LOG_LEVEL=info npx @vpursuit/swipl-mcp-server

# Show only warnings and errors
MCP_LOG_LEVEL=warning npx @vpursuit/swipl-mcp-server

# Show only critical errors
MCP_LOG_LEVEL=error npx @vpursuit/swipl-mcp-server
```

**Note:** This feature is not yet implemented. All log levels are currently sent to the client.

## Critical Rules

### ⚠️ NEVER Write to stdout

**The MCP protocol requires that ONLY JSON-RPC messages appear on stdout.** Any other output will corrupt the protocol stream and cause JSON parsing errors in the MCP Inspector.

❌ **WRONG:**
```typescript
console.log("[my-plugin] Starting...");  // Writes to stdout - BAD!
console.warn("Warning!");                  // Writes to stdout - BAD!
console.info("Info message");              // Writes to stdout - BAD!
```

✅ **CORRECT:**
```typescript
logger.info("Starting...");                // MCP protocol - GOOD!
console.error("[fallback] Message");       // stderr fallback - OK for pre-init
```

### stdout vs stderr vs MCP Protocol

| Method | Destination | When to Use | MCP Inspector |
|--------|-------------|-------------|---------------|
| `console.log()` | stdout | ❌ NEVER | Corrupts protocol |
| `console.warn()` | stdout | ❌ NEVER | Corrupts protocol |
| `console.info()` | stdout | ❌ NEVER | Corrupts protocol |
| `console.error()` | stderr | ⚠️ Fallback only | Not visible in UI |
| `logger.info()` | MCP protocol | ✅ Always | ✅ Visible in logs panel |

## Testing

### Verify MCP Inspector Works

1. Start the MCP Inspector:
   ```bash
   npx @modelcontextprotocol/inspector node packages/swipl-mcp-server/build/index.js
   ```

2. Check for errors in the Inspector console:
   - ❌ BAD: `SyntaxError: Unexpected token 'm', "[mcp-prolog"... is not valid JSON`
   - ✅ GOOD: `New STDIO connection request`, `Created client transport`

3. Look for logs in the Inspector UI logs panel:
   - Should see logs from `[mcp-core]`, `[mcp-prolog]`, `[mcp-roots]`
   - Logs appear with proper timestamps and levels

### Common Issues

**Problem:** JSON parsing errors in Inspector
```
Error: SyntaxError: Unexpected token 'm', "[mcp-prolog"... is not valid JSON
```

**Solution:** Search for `console.log()` or `console.warn()` calls:
```bash
grep -r "console\.(log|warn|info)" packages/ --include="*.ts"
```
Replace all with logger calls.

**Problem:** Logs not appearing in Inspector

**Solution:**
1. Verify server declares logging capability:
   ```typescript
   const server = new McpServer({
     name: "my-server",
     version: "1.0.0",
     capabilities: {
       logging: {},  // ← Must be present
     },
   });
   ```

2. Verify plugin sets serverRef in onInit:
   ```typescript
   async onInit(server) {
     serverRef.current = server;  // ← Must be set
     logger.info("Initialized");
   }
   ```

## MCP Specification

The logging implementation follows the MCP specification for server-side logging:

- **Specification:** [MCP Logging 2025-03-26](https://modelcontextprotocol.io/specification/2025-03-26/server/utilities/logging)
- **Method:** `notifications/message`
- **Capability:** `logging: {}`
- **Message Format:**
  ```json
  {
    "jsonrpc": "2.0",
    "method": "notifications/message",
    "params": {
      "level": "info",
      "logger": "mcp-prolog",
      "data": {
        "message": "Plugin initialized"
      }
    }
  }
  ```

## Implementation History

### Why This Approach?

**Problem:** Initial implementation used `console.log()` for logging, which writes to stdout. This corrupted the JSON-RPC protocol stream, causing:
```
Error: SyntaxError: Unexpected token 'm', "[mcp-prolog"... is not valid JSON
```

**Solution:** Implemented proper MCP protocol logging via `server.sendLoggingMessage()`, which:
- Sends logs as MCP notifications (not stdout)
- Appears cleanly in Inspector logs panel
- Maintains pure JSON-RPC stream on stdout
- Provides structured logging with levels and metadata

### Migration Checklist

When migrating code to use MCP logging:

- [ ] Replace all `console.log()` with `logger.info()`
- [ ] Replace all `console.warn()` with `logger.warn()`
- [ ] Replace all `console.error()` with `logger.error()`
- [ ] Import logger: `import { logger } from "./logger.js"`
- [ ] Set serverRef in plugin onInit: `serverRef.current = server`
- [ ] Verify no JSON parsing errors in Inspector
- [ ] Verify logs appear in Inspector UI

## References

- [MCP Specification - Logging](https://modelcontextprotocol.io/specification/2025-03-26/server/utilities/logging)
- [MCP SDK Documentation](https://github.com/modelcontextprotocol/typescript-sdk)
- [Inspector Tool](https://github.com/modelcontextprotocol/inspector)
