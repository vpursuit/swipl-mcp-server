# Configuration Reference

Complete guide to configuring the SWI-Prolog MCP Server.

## Filesystem Roots

**Required for file operations.** Configure allowed directories for loading Prolog files.

### Option 1: MCP Client Roots (Recommended)

Configure roots directly in your MCP client configuration. The server discovers these via the MCP protocol.

**Claude Desktop:**
```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": [
        "/Users/you/prolog",
        "/Users/you/knowledge"
      ]
    }
  }
}
```

**Claude Code CLI:**
```bash
claude mcp add swipl-mcp-server npx @vpursuit/swipl-mcp-server --roots /Users/you/prolog,/Users/you/knowledge
```

**Advantages:**
- Native MCP protocol support
- Client-managed security
- Easier to update (no environment variables)
- Works with MCP Inspector

### Option 2: Environment Variable

Set `SWI_MCP_ALLOWED_ROOTS` with comma-separated absolute paths:

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "env": {
        "SWI_MCP_ALLOWED_ROOTS": "/Users/you/prolog,/Users/you/knowledge"
      }
    }
  }
}
```

**Use Cases:**
- MCP clients without roots support
- Scripted deployments
- Testing and development

### Security Notes

- **Secure by Default**: Without configuration, file operations are disabled
- **System Directories Blocked**: `/etc`, `/usr`, `/bin`, `/var`, `/sys`, `/proc`, `/boot`, `/dev`, `/root` are always blocked
- **Verification**: Use the `roots_list` tool to verify configured roots

## Environment Variables

### Core Configuration

#### `SWI_MCP_ALLOWED_ROOTS`
**Type:** Comma-separated absolute paths
**Default:** (none)
**Example:** `/Users/you/prolog,/Users/you/knowledge`

Configures allowed directories for file operations. Alternative to MCP client roots.

#### `SWI_MCP_PROLOG_PATH`
**Type:** Absolute file path
**Default:** (auto-detected from package)
**Example:** `/path/to/custom/prolog_server.pl`

Override the Prolog server script location. Rarely needed except for development or custom deployments.

### Timeout Configuration

#### `SWI_MCP_READY_TIMEOUT_MS`
**Type:** Number (milliseconds)
**Default:** `5000` (5 seconds)
**Range:** 1000-60000
**Example:** `10000`

Timeout for server startup. Increase if server fails to start on slow systems.

**When to increase:**
- Slow disk I/O
- Large Prolog initialization
- Resource-constrained systems

**Configuration:**
```json
{
  "env": {
    "SWI_MCP_READY_TIMEOUT_MS": "10000"
  }
}
```

#### `SWI_MCP_QUERY_TIMEOUT_MS`
**Type:** Number (milliseconds)
**Default:** `30000` (30 seconds)
**Range:** 1000-600000
**Example:** `60000`

Timeout for individual query execution. Increase for complex queries or large search spaces.

**When to increase:**
- Complex constraint solving (CLP)
- Large knowledge bases
- Deep backtracking
- Graph algorithms

**When to decrease:**
- Interactive use (want fast feedback)
- Preventing runaway queries
- Resource protection

**Configuration:**
```json
{
  "env": {
    "SWI_MCP_QUERY_TIMEOUT_MS": "60000"
  }
}
```

### Logging Configuration

#### `MCP_LOG_LEVEL`
**Type:** Enum
**Values:** `silent` | `error` | `warn` | `info` | `debug`
**Default:** `warn`

Controls log verbosity. See [Logging](./logging.md) for detailed examples.

**Recommendations:**
- **Production:** `warn` or `error`
- **Development:** `info` or `debug`
- **Testing:** `debug`
- **Performance-critical:** `error` or `silent`

**Configuration:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug"
  }
}
```

#### `DEBUG`
**Type:** String (namespace)
**Default:** (none)
**Example:** `swipl-mcp-server`

Enable detailed debugging for specific components. Uses the `debug` npm package.

**Configuration:**
```json
{
  "env": {
    "DEBUG": "swipl-mcp-server"
  }
}
```

**Namespaces:**
- `swipl-mcp-server` - All server debugging
- `swipl-mcp-server:*` - All with wildcards

#### `SWI_MCP_TRACE`
**Type:** Boolean flag
**Values:** `1` (enabled), `0` or unset (disabled)
**Default:** (disabled)

Enable low-level wire protocol tracing. Very verbose, use only for deep debugging.

**What it traces:**
- Prolog process stdout/stderr
- Wire protocol frames
- Raw term communication
- IPC lifecycle

**Configuration:**
```json
{
  "env": {
    "SWI_MCP_TRACE": "1"
  }
}
```

**Warning:** Significant performance overhead. Only enable when investigating protocol issues.

## Complete Configuration Examples

### Minimal Production Configuration

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": ["/Users/you/prolog"]
    }
  }
}
```

### Development Configuration

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": [
        "/Users/you/prolog",
        "/Users/you/knowledge",
        "/Users/you/projects"
      ],
      "env": {
        "MCP_LOG_LEVEL": "debug",
        "DEBUG": "swipl-mcp-server",
        "SWI_MCP_QUERY_TIMEOUT_MS": "60000"
      }
    }
  }
}
```

### High-Performance Configuration

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": ["/Users/you/prolog"],
      "env": {
        "MCP_LOG_LEVEL": "error",
        "SWI_MCP_QUERY_TIMEOUT_MS": "120000"
      }
    }
  }
}
```

### Deep Debugging Configuration

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": ["/Users/you/prolog"],
      "env": {
        "MCP_LOG_LEVEL": "debug",
        "DEBUG": "swipl-mcp-server",
        "SWI_MCP_TRACE": "1",
        "SWI_MCP_READY_TIMEOUT_MS": "10000",
        "SWI_MCP_QUERY_TIMEOUT_MS": "60000"
      }
    }
  }
}
```

### Testing Configuration

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "roots": ["/tmp/prolog-tests"],
      "env": {
        "MCP_LOG_LEVEL": "info",
        "SWI_MCP_QUERY_TIMEOUT_MS": "5000"
      }
    }
  }
}
```

## Client-Specific Configuration

### Claude Desktop

**macOS:** `~/Library/Application Support/Claude/claude_desktop_config.json`
**Windows:** `%APPDATA%\Claude\claude_desktop_config.json`
**Linux:** `~/.config/claude/claude_desktop_config.json`

**Restart Required:** After modifying configuration, restart Claude Desktop.

### Claude Code CLI

```bash
# List configured servers
claude mcp list

# View specific server config
claude mcp info swipl-mcp-server

# Test server
claude mcp test swipl-mcp-server
```

### Codex CLI

**Configuration File:** `~/.codex/config.toml`

```toml
[mcp_servers.swipl-mcp-server]
transport = "stdio"
enabled = true
command = "npx"
args = ["@vpursuit/swipl-mcp-server"]

[mcp_servers.swipl-mcp-server.env]
MCP_LOG_LEVEL = "warn"
SWI_MCP_ALLOWED_ROOTS = "/Users/you/prolog"
```

### Gemini CLI

**Configuration File:** `.gemini/settings.json`

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "type": "stdio",
      "command": "npx",
      "args": ["-y", "@vpursuit/swipl-mcp-server"],
      "trust": true,
      "env": {
        "SWI_MCP_ALLOWED_ROOTS": "/Users/you/prolog"
      }
    }
  }
}
```

### MCP Inspector

```bash
# Basic
npx @modelcontextprotocol/inspector \
  --transport stdio \
  npx @vpursuit/swipl-mcp-server

# With environment variables
SWI_MCP_ALLOWED_ROOTS=/path/to/prolog \
MCP_LOG_LEVEL=debug \
npx @modelcontextprotocol/inspector \
  --transport stdio \
  npx @vpursuit/swipl-mcp-server
```

## Configuration Verification

### Check Configured Roots

Use the `roots_list` tool to verify filesystem roots:

```json
{
  "tool": "roots_list"
}
```

**Expected Response:**
```json
{
  "roots": [
    "/Users/you/prolog",
    "/Users/you/knowledge"
  ],
  "source": "mcp_client"
}
```

### Check Server Capabilities

Use the `capabilities` tool to verify server configuration:

```json
{
  "tool": "capabilities"
}
```

**Response includes:**
- Server version
- Available tools
- Supported query modes
- Security constraints
- Library whitelist

### Validate Configuration

**Start server with debug logging:**
```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "DEBUG": "swipl-mcp-server"
  }
}
```

**Check logs for:**
- Root discovery results
- Environment variable parsing
- Startup success/failure
- Configuration warnings

## Troubleshooting Configuration Issues

### File Operations Disabled

**Symptoms:**
- `knowledge_base_load` fails with "access denied"
- `roots_list` shows empty array

**Solution:**
1. Configure roots via MCP client `roots` or `SWI_MCP_ALLOWED_ROOTS`
2. Verify paths are absolute
3. Check path permissions
4. Restart client after configuration change

### Server Startup Timeout

**Symptoms:**
- "Server failed to start"
- Timeout errors during initialization

**Solution:**
1. Increase `SWI_MCP_READY_TIMEOUT_MS` to 10000 or higher
2. Check SWI-Prolog is installed: `swipl --version`
3. Enable debug logging to see startup details
4. Check for disk I/O or resource constraints

### Query Timeouts

**Symptoms:**
- Queries hang or timeout
- "Query timed out" errors

**Solution:**
1. Increase `SWI_MCP_QUERY_TIMEOUT_MS` to 60000 or higher
2. Optimize query (see [optimize prompt](../README.md#available-prompts))
3. Check for infinite loops or unbounded search
4. Consider using Engine Mode for complex backtracking

### Logs Not Appearing

**Symptoms:**
- No log output
- Can't debug issues

**Solution:**
1. Set `MCP_LOG_LEVEL` to `debug` or `info`
2. Enable `DEBUG=swipl-mcp-server`
3. Check client log location (see [Logging](./logging.md#log-file-location))
4. Verify configuration was applied (restart client)

## See Also

- [Logging](./logging.md) - Detailed logging and debugging guide
- [Installation](../README.md#installation) - Installation and setup
- [Troubleshooting](../README.md#troubleshooting) - Common issues and solutions
- [Architecture](./architecture.md) - Understanding server internals
