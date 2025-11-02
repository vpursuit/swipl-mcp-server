# Installation Guide

Complete installation instructions for all MCP clients supporting the SWI-Prolog MCP Server.

## Requirements

- Node.js ≥20.0.0
- SWI-Prolog installed and available in PATH
  - Test with: `swipl --version`

## Claude Code CLI

The Claude Code CLI provides the simplest installation method:

```bash
claude mcp add swipl-mcp-server npx @vpursuit/swipl-mcp-server
```

### Verification
After installation, verify the server is available:
```bash
claude mcp list
```

## Claude Desktop

### Configuration File Location
- **macOS:** `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows:** `%APPDATA%/Claude/claude_desktop_config.json`
- **Linux:** `~/.config/claude/claude_desktop_config.json`

### Basic Configuration
```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"]
    }
  }
}
```

### Restart Required
After modifying the configuration file, restart Claude Desktop for changes to take effect.

## Cline (VS Code Extension)

```json
{
  "mcpServers": {
    "swipl-mcp-server": {
      "autoApprove": [],
      "disabled": false,
      "timeout": 60,
      "type": "stdio",
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"]
    }
  }
}
```

Configure via Cline's MCP settings in VS Code.

## Codex

```toml
[mcp_servers.swipl-mcp-server]
transport = "stdio"
enabled = true
command = "npx"
args = ["@vpursuit/swipl-mcp-server"]
```

Add to `~/.codex/config.toml`

## Other MCP Clients

### Generic Configuration
For other MCP clients that support the Model Context Protocol:

```json
{
  "command": "npx",
  "args": ["@vpursuit/swipl-mcp-server"],
  "transport": "stdio"
}
```


## Environment Variables

Configure server behavior with environment variables:

| Variable | Description | Default | Example |
|----------|-------------|---------|---------|
| `SWI_MCP_READY_TIMEOUT_MS` | Server startup timeout (ms) | `5000` | `10000` |
| `SWI_MCP_QUERY_TIMEOUT_MS` | Query execution timeout (ms) | `30000` | `120000` |
| `MCP_LOG_LEVEL` | Logging verbosity | `warn` | `debug` |
| `DEBUG` | Enable debug logging | - | `swipl-mcp-server` |
| `SWI_MCP_TRACE` | Low-level I/O trace | - | `1` |
| `SWI_MCP_PROLOG_PATH` | Custom Prolog server script | - | `/path/to/server.pl` |

## MCP Inspector (Testing)

```bash
npx @modelcontextprotocol/inspector --transport stdio npx @vpursuit/swipl-mcp-server
```

### ... and many others may also work

## Development Setup

If you cloned the repo, you may use this configuration. Note: change `<path to your development directory>` to your local setup.

```json
{
  "mcpServers": {
    "swipl": {
      "command": "node",
      "args": ["<path to your development directory>/swipl-mcp-server/build/index.js"],
      "env": {
        "SWI_MCP_READY_TIMEOUT_MS": "10000",
        "SWI_MCP_QUERY_TIMEOUT_MS": "120000",
        "MCP_LOG_LEVEL": "debug",
        "DEBUG": "swipl-mcp-server"
      }
    }
  }
}
```

## Troubleshooting

### Common Issues

**SWI-Prolog not found:**
```bash
# Verify SWI-Prolog installation
swipl --version

# On macOS with Homebrew
brew install swi-prolog

# On Ubuntu/Debian
sudo apt-get install swi-prolog

# On Windows
# Download from https://www.swi-prolog.org/download/stable
```

**Server startup timeout:**
- Increase `SWI_MCP_READY_TIMEOUT_MS` to `10000` or higher
- Check SWI-Prolog is accessible in PATH
- Enable debug logging with `DEBUG=swipl-mcp-server`

**Query timeout:**
- Increase `SWI_MCP_QUERY_TIMEOUT_MS` for complex queries
- Use `query_close` to terminate long-running queries

**Configuration not loading:**
- Verify JSON syntax in configuration files
- Restart the MCP client after configuration changes
- Check file permissions on configuration directories

**File access errors:**
- Files must be placed in `~/.swipl-mcp-server/` directory
- Create the directory if it doesn't exist: `mkdir -p ~/.swipl-mcp-server`
- Check file permissions for read access

### Debug Mode

Enable comprehensive debugging:

```json
{
  "env": {
    "MCP_LOG_LEVEL": "debug",
    "DEBUG": "swipl-mcp-server",
    "SWI_MCP_TRACE": "1"
  }
}
```

This provides detailed logs of:
- Server startup and initialization
- Tool calls and responses
- Prolog process communication
- Error conditions and stack traces

### Getting Help

- Check existing [issues](https://github.com/vpursuit/swipl-mcp-server/issues)
- Review [examples](examples.md) for usage patterns
- Test with MCP Inspector for isolated debugging
- Include debug logs when reporting issues