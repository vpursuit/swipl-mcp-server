# Langflow "Connection Closed" Fix - Reference Materials

Complete reference materials for debugging and fixing the Langflow MCP Client "Connection Closed" error with swipl-mcp-server.

## Quick Summary

**Problem:** Langflow MCP Client showed "Connection Closed" error when connecting to swipl-mcp-server.

**Root Cause:** MCP subprocesses don't inherit user's shell PATH, so `swipl` executable was not found.

**Solution:** Server now automatically finds SWI-Prolog in common installation locations. No PATH configuration needed.

**Result:** Simple configuration - `node dist/index.js` - it just works.

---

## Contents

### 📖 [LANGFLOW-REFERENCE.md](./LANGFLOW-REFERENCE.md)
**Complete consolidated reference** - Everything you need in one place:
- Root cause analysis
- Fix implementation details
- Setup instructions
- Troubleshooting guide
- Advanced configuration

**Start here for:**
- Understanding what went wrong
- Learning how the fix works
- Setting up Langflow MCP Client
- Troubleshooting connection issues

### 🔧 [scripts/](./scripts/)
**Debugging and testing scripts:**

- **[debug-wrapper.sh](./scripts/debug-wrapper.sh)** - Comprehensive debug wrapper
  - Captures all output to log file (`/tmp/swipl-mcp-server.log`)
  - Shows environment diagnostics
  - Use when Langflow doesn't show logs
  - Usage: Replace `node dist/index.js` with `/path/to/debug-wrapper.sh` in Langflow config

- **[start-server.sh](./scripts/start-server.sh)** - Simple startup script
  - Sets PATH explicitly (fallback for testing)
  - Logs to `/tmp/swipl-startup.log`
  - Use for manual testing

- **[test-langflow.sh](./scripts/test-langflow.sh)** - Minimal test script
  - Tests if Langflow can execute scripts
  - Logs execution details to `/tmp/langflow-test.log`
  - Use to verify Langflow permissions

### 📝 [test-examples/](./test-examples/)
**Test files and configuration examples:**

- **[langflow-simple-config.md](./test-examples/langflow-simple-config.md)** - Simplified configuration reference
  - Shows new automatic discovery feature
  - Quick setup instructions
  - Expected results

- **[test-mcp-init.json](./test-examples/test-mcp-init.json)** - MCP initialization message
  - Example JSON for manual testing
  - Use with: `node dist/index.js < test-mcp-init.json`

---

## Quick Start

### If You Have "Connection Closed" Error

1. **Check SWI-Prolog is installed:**
   ```bash
   which swipl
   # Should show: /opt/homebrew/bin/swipl or similar
   ```

2. **Configure Langflow MCP Tools:**
   ```
   Command: node
   Arguments: /path/to/swipl-mcp-server/dist/index.js
   ```

3. **If still failing, use debug wrapper:**
   ```
   Command: /path/to/docs/troubleshooting/langflow/scripts/debug-wrapper.sh
   ```

4. **Check logs:**
   ```bash
   tail -f /tmp/swipl-mcp-server.log
   ```

5. **See troubleshooting section in [LANGFLOW-REFERENCE.md](./LANGFLOW-REFERENCE.md#troubleshooting)**

### When to Use Each Script

| Script | When to Use |
|--------|-------------|
| **debug-wrapper.sh** | Langflow shows "Connection Closed" with no logs |
| **start-server.sh** | Manual testing or PATH-based fallback needed |
| **test-langflow.sh** | Testing if Langflow can execute scripts at all |

---

## The Fix in One Line

```javascript
// Before: spawn('swipl', ...) → Command not found
// After:  spawn(findSwiplExecutable(), ...) → Searches common locations automatically
```

The server now searches these locations:
- `/opt/homebrew/bin/swipl` (Homebrew Apple Silicon)
- `/usr/local/bin/swipl` (Homebrew Intel)
- `/usr/bin/swipl` (System package manager)
- `/opt/local/bin/swipl` (MacPorts)
- Windows default paths
- System PATH

**No configuration needed - it just works.**

---

## Files Modified (For Reference)

The permanent fix involved changes to these source files:

1. **`plugins/server/core/src/executable-finder.ts`** - Generic findExecutable() utility
2. **`plugins/server/prolog/src/PrologInterface.ts`** - Uses findExecutable() for swipl
3. **`plugins/server/prolog/src/index.ts`** - Blocking initialization with health checks
4. **`products/swipl-mcp-server/src/index.ts`** - Startup guards and transport logging

See [LANGFLOW-REFERENCE.md - The Fix](./LANGFLOW-REFERENCE.md#the-fix) for implementation details.

---

## Environment Variables Reference

| Variable | Default | Description |
|----------|---------|-------------|
| `SWI_MCP_READY_TIMEOUT_MS` | 5000 | Timeout waiting for Prolog startup (ms) |
| `SWI_MCP_QUERY_TIMEOUT_MS` | 30000 | Default query timeout (ms) |
| `SWI_MCP_TRACE` | 0 | Enable trace logging (1=on) |
| `MCP_LOG_LEVEL` | info | Log level: debug, info, warn, error |
| `SWI_MCP_LOG_FILE` | /tmp/swipl-mcp-server.log | Debug wrapper log file |

---

## Getting Help

If you still have issues after reading the reference:

1. Use debug wrapper and capture logs
2. Test server manually: `echo '...' | node dist/index.js`
3. Check Prolog installation: `swipl --version`
4. See [LANGFLOW-REFERENCE.md - Getting Help](./LANGFLOW-REFERENCE.md#getting-help)

---

## Related Documentation

- [LANGFLOW-SETUP.md](../../../products/swipl-mcp-server/LANGFLOW-SETUP.md) - Original setup guide (still in product folder)
- [FIXES.md](../../../products/swipl-mcp-server/FIXES.md) - Original fix documentation (still in product folder)
- [TROUBLESHOOTING.md](../../../products/swipl-mcp-server/TROUBLESHOOTING.md) - Original troubleshooting guide (still in product folder)

**Note:** The above files are still maintained in the product folder. This reference folder consolidates them for easy access.
