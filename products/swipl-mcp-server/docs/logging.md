# Logging & Debugging

Comprehensive guide to logging and debugging the SWI-Prolog MCP Server.

## Log Levels

Control log verbosity with `MCP_LOG_LEVEL`:

- **`silent`** - No logging output
- **`error`** - Only critical errors
- **`warn`** - Warnings and errors (default)
- **`info`** - General information, warnings, and errors
- **`debug`** - Detailed debugging information

### Setting Log Level

**Claude Desktop:**
```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "env": {
        "MCP_LOG_LEVEL": "debug"
      }
    }
  }
}
```

**Claude Code CLI:**
```bash
MCP_LOG_LEVEL=debug claude mcp run swipl-mcp-server
```

**Direct Execution:**
```bash
MCP_LOG_LEVEL=debug npx @vpursuit/swipl-mcp-server
```

## Debug Namespace

Enable detailed debugging for specific components using the `DEBUG` environment variable:

```bash
DEBUG=swipl-mcp-server npx @vpursuit/swipl-mcp-server
```

This enables:
- Tool execution traces
- Query processing details
- Knowledge base operations
- Session state transitions

## Wire Protocol Tracing

For low-level protocol debugging, enable `SWI_MCP_TRACE`:

```json
{
  "env": {
    "SWI_MCP_TRACE": "1",
    "MCP_LOG_LEVEL": "debug"
  }
}
```

This traces:
- Prolog process stdout/stderr
- Wire protocol frames (cmd/id envelopes)
- Raw term communication
- Process lifecycle events

## Common Debugging Scenarios

### Problem: Server Not Starting

**Symptoms:**
- MCP client shows "Server failed to start"
- No response from server

**Debug Configuration:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "DEBUG": "swipl-mcp-server"
  }
}
```

**What to look for:**
- SWI-Prolog binary not found (check `swipl --version`)
- Permission issues on prolog files
- Syntax errors in prolog_server.pl
- Port conflicts or IPC issues

### Problem: Query Timeouts

**Symptoms:**
- Queries hang or timeout
- "Query timed out" errors

**Debug Configuration:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "info",
    "SWI_MCP_QUERY_TIMEOUT_MS": "60000"
  }
}
```

**What to look for:**
- Long-running or infinite loops in queries
- Complex backtracking scenarios
- Large result sets
- Resource exhaustion

### Problem: File Loading Fails

**Symptoms:**
- `knowledge_base_load` returns errors
- "Access denied" or "File not found"

**Debug Configuration:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "DEBUG": "swipl-mcp-server"
  }
}
```

**What to look for:**
- Root directory not configured (see [Configuration](./configuration.md))
- File path outside allowed roots
- System directory access attempt
- File syntax errors

### Problem: Security Validation Errors

**Symptoms:**
- "Security Error: Operation blocked"
- Dangerous predicate warnings

**Debug Configuration:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "SWI_MCP_TRACE": "1"
  }
}
```

**What to look for:**
- Use of blocked predicates (shell, system, call)
- File operations outside roots
- Directive execution attempts
- Sandbox validation failures

### Problem: Inconsistent Query Results

**Symptoms:**
- Different results across runs
- State persists unexpectedly

**Debug Configuration:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "DEBUG": "swipl-mcp-server",
    "SWI_MCP_TRACE": "1"
  }
}
```

**What to look for:**
- Session state transitions
- Query mode switches (standard vs engine)
- Knowledge base modifications
- Unclosed query sessions

## Log Output Examples

### Normal Operation (warn level)
```
[swipl-mcp-server] Starting server...
[swipl-mcp-server] SWI-Prolog process started
[swipl-mcp-server] Server ready
```

### Debug Level
```
[swipl-mcp-server] Starting server with config: {...}
[swipl-mcp-server] Spawning Prolog process: swipl [args]
[swipl-mcp-server] PID: 12345
[swipl-mcp-server] Waiting for startup banner...
[swipl-mcp-server] Received: prolog_server ready
[swipl-mcp-server] Server ready

[swipl-mcp-server] Tool called: query_start
[swipl-mcp-server] Query: member(X, [1,2,3])
[swipl-mcp-server] Sending: cmd(1, start_engine_string("member(X, [1,2,3])"))
[swipl-mcp-server] Received: id(1, ok)
[swipl-mcp-server] Session state: idle -> engine
```

### Trace Level (SWI_MCP_TRACE=1)
```
[swipl-mcp-server] [TRACE] Prolog stdout: |prolog_server ready|
[swipl-mcp-server] [TRACE] Sending term: |cmd(1, start_engine_string("member(X, [1,2,3])"))|
[swipl-mcp-server] [TRACE] Prolog stdout: |id(1, ok)|
[swipl-mcp-server] [TRACE] Parsed reply: {id: 1, reply: ok}
[swipl-mcp-server] [TRACE] State transition: idle -> engine
```

## Performance Considerations

**Production Recommendations:**

```json
{
  "env": {
    "MCP_LOG_LEVEL": "warn",
    "DEBUG": ""
  }
}
```

- `debug` level can impact performance (verbose output)
- `SWI_MCP_TRACE` significantly increases overhead
- Use `info` or `warn` in production
- Enable `debug` only when investigating issues

## Log File Location

Logs are written to stderr by default. To capture to a file:

**macOS/Linux:**
```bash
npx @vpursuit/swipl-mcp-server 2> server.log
```

**Claude Desktop:** Check client logs:
- **macOS**: `~/Library/Logs/Claude/`
- **Windows**: `%APPDATA%\Claude\logs\`
- **Linux**: `~/.config/Claude/logs/`

## Interpreting Trace Output

### Session State Transitions

```
State: idle -> query           # Standard mode query started
State: query -> query_completed # Query finished
State: query_completed -> idle  # Query closed

State: idle -> engine           # Engine mode query started
State: engine -> engine_completed # Engine exhausted
State: engine_completed -> idle  # Engine closed
```

### Wire Protocol Frames

```
Outgoing: cmd(123, start_engine_string("X = 1"))
  ↳ Request ID 123: Start engine with query

Incoming: id(123, ok)
  ↳ Reply to request 123: Success

Incoming: id(123, solution([variable('X', '1')]))
  ↳ Reply to request 123: Solution found

Incoming: id(123, done)
  ↳ Reply to request 123: No more solutions
```

### Security Validation

```
[swipl-mcp-server] Validating: shell('rm -rf /')
[swipl-mcp-server] Security check: BLOCKED - dangerous predicate 'shell'
[swipl-mcp-server] Error: Security Error: Operation blocked - contains dangerous predicate 'shell'
```

## See Also

- [Configuration](./configuration.md) - Environment variable reference
- [Troubleshooting](../README.md#troubleshooting) - Common issues and solutions
- [Architecture](./architecture.md) - Understanding server internals
