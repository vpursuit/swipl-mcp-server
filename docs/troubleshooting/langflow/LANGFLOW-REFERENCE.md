# Langflow MCP Client - Complete Reference

Complete guide for fixing and configuring the SWI-Prolog MCP Server with Langflow MCP Client.

## Table of Contents

- [Overview](#overview)
- [Root Cause Analysis](#root-cause-analysis)
- [The Fix](#the-fix)
- [Quick Setup](#quick-setup)
- [Troubleshooting](#troubleshooting)
- [Advanced Configuration](#advanced-configuration)

---

## Overview

### Problem
Langflow MCP Client showed "Connection Closed" error when trying to connect to swipl-mcp-server.

### Solution
Server now **automatically finds SWI-Prolog** in common installation locations. No PATH configuration or wrapper scripts needed.

### Result
Simple, direct configuration: `node dist/index.js` - it just works.

---

## Root Cause Analysis

### Why Did It Fail?

When MCP clients like Langflow spawn the server as a subprocess, they don't inherit the user's shell PATH. This caused a cascade of failures:

1. **Subprocess has minimal PATH**
   - Langflow spawns server with restricted environment
   - Common tools like `swipl` not found in PATH

2. **Plugin initialization fails silently**
   - PrologInterface.ts tried to spawn `swipl` command
   - Command not found → subprocess spawn error
   - Error not properly caught or reported

3. **Connection closes immediately**
   - Initialization failure → server exits
   - Client sees "Connection Closed" with no logs
   - No indication of root cause

### Why Traditional Solutions Didn't Work

**Wrapper scripts** (adding PATH in shell):
- Work for manual testing
- Don't work when Langflow spawns subprocess directly
- Langflow may not execute shell scripts with proper permissions

**Environment variables in MCP config**:
- Some clients don't pass env vars to subprocess
- Inconsistent behavior across different MCP clients
- Not portable across different systems

---

## The Fix

### Permanent Solution: Automatic Executable Discovery

Modified the server to search for SWI-Prolog in common installation locations automatically:

#### Code Changes

**1. Added findSwiplExecutable() function** (`plugins/server/prolog/src/PrologInterface.ts:56-72`)

Searches these locations in order:
- `/opt/homebrew/bin/swipl` (Homebrew on Apple Silicon Mac)
- `/usr/local/bin/swipl` (Homebrew on Intel Mac, standard Linux)
- `/usr/bin/swipl` (System package manager on Linux)
- `/opt/local/bin/swipl` (MacPorts on Mac)
- `C:\Program Files\swipl\bin\swipl.exe` (Windows default)
- `C:\Program Files (x86)\swipl\bin\swipl.exe` (Windows 32-bit)
- Falls back to `swipl` from PATH if found

**2. Blocking Prolog initialization** (`plugins/server/prolog/src/index.ts:112`)
- Changed from `void prologInterface.start()` to `await prologInterface.start()`
- Ensures initialization failures propagate immediately with clear errors
- Added health check validation after startup

**3. Startup guards** (`products/swipl-mcp-server/src/index.ts:104-107`)
- Prevents premature shutdown if stdin closes during initialization
- Logs warning when stdin closes before startup completes
- Added transport lifecycle logging for better diagnostics

### Benefits

- **No wrapper scripts needed** - Server works directly
- **No PATH configuration needed** - Finds swipl automatically
- **Cross-platform** - Works on macOS (Homebrew, MacPorts), Linux, Windows
- **Clear error messages** - Shows which locations were checked if swipl not found
- **Backward compatible** - Still respects PATH if swipl is there

### Files Modified

1. `plugins/server/core/src/executable-finder.ts` - Generic findExecutable() utility
2. `plugins/server/prolog/src/PrologInterface.ts` - Uses findExecutable() for swipl discovery
3. `plugins/server/prolog/src/index.ts` - Blocking initialization with health checks
4. `products/swipl-mcp-server/src/index.ts` - Startup guards and transport logging

---

## Quick Setup

### Step-by-Step Configuration

1. **In Langflow UI**, add an **MCP Tools** component to your flow

2. **Click "Add MCP Server"** button

3. **Select "STDIO Mode"** (not SSE)

4. **Fill in these fields:**

   ```
   Name: swipl-prolog

   Command: node

   Arguments: /path/to/swipl-mcp-server/dist/index.js

   Environment Variables: (optional, for debugging)
     Key: SWI_MCP_READY_TIMEOUT_MS
     Value: 30000
   ```

5. **Click Save** and wait 5-10 seconds for server to initialize

6. **Verify**: You should see 17 tools in the dropdown

### Alternative: Using Full Path to Node

If `node` isn't found, use the full path:

```
Command: /Users/keeper/.nvm/versions/node/v22.18.0/bin/node

Arguments: /path/to/swipl-mcp-server/dist/index.js
```

Find your node path:
```bash
which node
```

### Alternative: JSON Configuration

If Langflow allows JSON config paste:

```json
{
  "swipl-prolog": {
    "command": "node",
    "args": ["/path/to/swipl-mcp-server/dist/index.js"]
  }
}
```

With debugging enabled:

```json
{
  "swipl-prolog": {
    "command": "node",
    "args": ["/path/to/swipl-mcp-server/dist/index.js"],
    "env": {
      "SWI_MCP_READY_TIMEOUT_MS": "30000",
      "MCP_LOG_LEVEL": "debug"
    }
  }
}
```

### How Automatic Discovery Works

The server searches for SWI-Prolog in these locations automatically:
- `/opt/homebrew/bin/swipl` (Homebrew on Apple Silicon)
- `/usr/local/bin/swipl` (Homebrew on Intel Mac)
- `/usr/bin/swipl` (System package manager)
- `/opt/local/bin/swipl` (MacPorts)
- Windows default installation paths
- System PATH

**No manual PATH configuration needed!**

### Expected Behavior

When working correctly:

1. **In Langflow UI:**
   - MCP server shows "Connected" status
   - Tools dropdown populates with 17 tools
   - Tools include: `help`, `capabilities`, `query_start`, `knowledge_base_load`, etc.

2. **Available tools:**
   - help, license, capabilities
   - knowledge_base_load, knowledge_base_assert, knowledge_base_assert_many
   - knowledge_base_retract, knowledge_base_retract_many, knowledge_base_clear
   - knowledge_base_load_library, knowledge_base_dump
   - query_start, query_next, query_close, query_startEngine
   - symbols_list, roots_list

---

## Troubleshooting

### Quick Diagnosis: "Connection Closed" Error

If Langflow shows "Connection Closed" with no logs, use the debug wrapper.

#### Step 1: Use Debug Wrapper

The debug wrapper captures ALL output to a log file, even if Langflow doesn't show logs.

**In Langflow MCP config, replace:**
```json
{
  "command": "node",
  "args": ["/path/to/dist/index.js"]
}
```

**With:**
```json
{
  "command": "/path/to/swipl-mcp-server/scripts/debug-wrapper.sh"
}
```

**Then check the log file:**
```bash
tail -f /tmp/swipl-mcp-server.log
```

#### Step 2: Interpret the Logs

**Success Pattern:**
```
[mcp-core] ✓ All plugins loaded successfully
[swipl-mcp-server] Server started successfully
```

If successful but still getting "Connection Closed":
- Your MCP client is not properly connecting via stdio
- Check your client's MCP configuration
- Ensure client is using stdio transport (not SSE)

**Error: Prolog Script Not Found**
```
ERROR: Prolog server script not found
```

Fix: Set the path to the Prolog script:
```json
{
  "env": {
    "SWI_MCP_PROLOG_PATH": "/path/to/swipl-mcp-server/prolog/prolog_server.pl"
  }
}
```

**Error: Prolog Not Installed**
```
✗ SWI-Prolog NOT FOUND in PATH!
```

Fix: Install SWI-Prolog:
```bash
# macOS
brew install swi-prolog

# Linux (Debian/Ubuntu)
sudo apt-get install swi-prolog

# Linux (Fedora)
sudo dnf install pl

# Windows
# Download from https://www.swi-prolog.org/download/stable
```

**Error: Ready Timeout**
```
Failed to start Prolog process: Prolog server ready timeout
```

Fix: Increase timeout (useful for slow systems):
```json
{
  "env": {
    "SWI_MCP_READY_TIMEOUT_MS": "30000"
  }
}
```

**Error: Health Check Failed**
```
Prolog health check failed: {"isReady":false,...}
```

Fix: Test Prolog script manually:
```bash
cd /path/to/swipl-mcp-server
swipl -q -s prolog/prolog_server.pl -g "server_loop" -t "halt"
```

Should print `@@READY@@` and wait for input.

**No Logs at All**

If `/tmp/swipl-mcp-server.log` is empty or doesn't exist:
1. Check wrapper script permissions:
   ```bash
   chmod +x /path/to/swipl-mcp-server/scripts/debug-wrapper.sh
   ```

2. Check write permissions on log directory:
   ```bash
   touch /tmp/test.log && rm /tmp/test.log
   ```

### Manual Testing

Test the server manually to isolate the issue:

**Test 1: Direct Server Start**
```bash
cd /path/to/swipl-mcp-server
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}' | node dist/index.js
```

Expected output:
```
[mcp-core] Loading 2 plugin(s)
[mcp-prolog] INFO: Prolog interface started successfully
[swipl-mcp-server] Server started successfully
{"result":{"protocolVersion":"2024-11-05",...
```

If this works but Langflow doesn't:
- Issue is with Langflow MCP client configuration
- Check Langflow's stdio implementation
- Verify Langflow is sending proper initialize message

**Test 2: Prolog Script Only**
```bash
cd /path/to/swipl-mcp-server
swipl -q -s prolog/prolog_server.pl -g "server_loop" -t "halt"
```

Expected output:
```
@@READY@@
[waiting for input]
```

If this fails:
- SWI-Prolog installation issue
- Prolog script corrupted
- Missing Prolog libraries

**Test 3: Debug Wrapper with Verbose Logging**
```bash
cd /path/to/swipl-mcp-server
SWI_MCP_TRACE=1 MCP_LOG_LEVEL=debug ./scripts/debug-wrapper.sh <<EOF
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
EOF

# Then check detailed logs:
cat /tmp/swipl-mcp-server.log
```

### Common Issues

**Error: "Command not found" or "node: not found"**

Use the full path to node:
```
Command: /Users/keeper/.nvm/versions/node/v22.18.0/bin/node
```

**No tools appearing after connection**

1. Wait 10-30 seconds (initial startup can be slow)
2. Refresh the Langflow page
3. Check MCP server status in Langflow Settings > MCP Servers
4. Look for errors in browser console (F12)

**Server starts but exits immediately**

1. Check SWI-Prolog is installed: `which swipl`
2. Increase timeout: `SWI_MCP_READY_TIMEOUT_MS = 30000`
3. Test server manually (see Manual Testing above)

### Langflow-Specific Issues

**Langflow may not display stderr logs in UI:**
- Use debug wrapper and check log file
- Check Langflow's terminal output (where you started Langflow)

**Langflow may have stricter initialization timeout:**
- Increase ready timeout to 10-30 seconds
- Use `SWI_MCP_READY_TIMEOUT_MS` environment variable

**Langflow logs:**
```bash
# If running Langflow from terminal, logs appear there
# Look for messages like:
# - "Connecting to MCP server..."
# - "MCP server connection failed..."
# - "Tools loaded from MCP server..."

# Check Python logs
tail -f ~/.langflow/logs/*.log

# Check browser console
# Open Langflow in browser > F12 > Console tab
```

---

## Advanced Configuration

### Environment Variables

Configure the server via environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `SWI_MCP_PROLOG_PATH` | (auto-detect) | Path to prolog_server.pl script |
| `SWI_MCP_READY_TIMEOUT_MS` | 5000 | Timeout waiting for Prolog @@READY@@ (ms) |
| `SWI_MCP_QUERY_TIMEOUT_MS` | 30000 | Default query execution timeout (ms) |
| `SWI_MCP_TRACE` | 0 | Enable trace logging (1=on, 0=off) |
| `MCP_LOG_LEVEL` | info | Log level: debug, info, warn, error |
| `SWI_MCP_LOG_FILE` | /tmp/swipl-mcp-server.log | Debug wrapper log file path |
| `SWI_MCP_ALLOWED_ROOTS` | (from client) | Allowed filesystem roots (comma-separated) |

### Full Debug Configuration

```json
{
  "mcpServers": {
    "swipl": {
      "command": "/path/to/swipl-mcp-server/scripts/debug-wrapper.sh",
      "env": {
        "SWI_MCP_TRACE": "1",
        "MCP_LOG_LEVEL": "debug",
        "SWI_MCP_READY_TIMEOUT_MS": "10000",
        "SWI_MCP_LOG_FILE": "/tmp/swipl-debug.log"
      }
    }
  }
}
```

### Performance Tuning

**For Large Knowledge Bases:**
```json
{
  "env": {
    "SWI_MCP_QUERY_TIMEOUT_MS": "60000"
  }
}
```

**For Slow Systems:**
```json
{
  "env": {
    "SWI_MCP_READY_TIMEOUT_MS": "30000",
    "SWI_MCP_QUERY_TIMEOUT_MS": "60000"
  }
}
```

**For Production (Minimal Logging):**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "warn",
    "SWI_MCP_TRACE": "0"
  }
}
```

### Migration for Users

Users currently using wrapper scripts can now switch to direct node invocation:

1. Update MCP client configuration to use `node dist/index.js` directly
2. Remove `PATH` environment variable (no longer needed)
3. Keep `SWI_MCP_READY_TIMEOUT_MS` if needed for slow systems

The wrapper scripts remain available in `scripts/` folder for users who need extra logging during debugging.

### Health Check

After starting the server, verify it's healthy by calling the `capabilities` tool:

```json
{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"capabilities","arguments":{}}}
```

Expected response includes:
```json
{
  "content": [{
    "type": "text",
    "text": "{\"status\":\"healthy\",\"prolog\":{\"isReady\":true,\"circuitState\":\"closed\",...}"
  }]
}
```

---

## Known Limitations

1. **Stdio transport only**: This server only supports stdio transport. SSE and other transports are not supported.

2. **Single concurrent query**: Only one query session can be active at a time. Close the current query before starting a new one.

3. **File operations require roots**: Knowledge base file loading requires filesystem roots to be configured via the MCP client or `SWI_MCP_ALLOWED_ROOTS` environment variable.

4. **Sandboxed execution**: All Prolog queries run in a sandboxed environment. Dangerous operations (shell, system, call, halt) are blocked for security.

---

## Getting Help

If you still have issues:

1. **Capture full debug log:**
   ```bash
   SWI_MCP_TRACE=1 MCP_LOG_LEVEL=debug /path/to/scripts/debug-wrapper.sh < /path/to/test-examples/test-mcp-init.json
   cat /tmp/swipl-mcp-server.log > issue-log.txt
   ```

2. **Include in your issue report:**
   - Log file contents (`issue-log.txt`)
   - Langflow version: `langflow --version`
   - Operating system and version
   - Node.js version: `node --version`
   - SWI-Prolog version: `swipl --version`
   - Langflow MCP configuration (redact sensitive paths)

3. **Test Prolog manually:**
   ```bash
   swipl -q -s prolog/prolog_server.pl -g "server_loop" -t "halt"
   # Include output in issue report
   ```

4. **Confirm:**
   - Is Langflow running in Docker or locally?
   - Did the manual test work?
   - Did using the debug wrapper produce logs?
