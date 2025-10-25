# @vpursuit/mcp-roots

Dynamic filesystem root discovery for Model Context Protocol (MCP) servers.

## Overview

`@vpursuit/mcp-roots` provides secure, dynamic filesystem access control for MCP servers through:
- Client-provided roots via MCP protocol
- Automatic fallback to `~/.swipl-mcp-server`
- Path validation and security checks
- Root change notifications
- System directory blocking

## Installation

```bash
npm install @vpursuit/mcp-roots
```

## Usage

### As a Plugin

```typescript
import { loadPlugins } from '@vpursuit/mcp-core';
import { plugin as rootsPlugin } from '@vpursuit/mcp-roots';

await loadPlugins(server, [rootsPlugin]);
```

### Direct API Usage

```typescript
import { RootsManager } from '@vpursuit/mcp-roots';

const manager = RootsManager.getInstance();
manager.setServerInstance(server);

// Discover roots
await manager.discoverRoots(true);

// Get all roots
const roots = await manager.getRoots();

// Validate a path
const result = await manager.validatePath('/path/to/file.txt');
if (result.allowed) {
  // Path is within allowed roots
}
```

## Features

### Provided Tools

- **`roots_list`**: List all filesystem roots currently known to the server

### Root Discovery

1. **MCP Client Roots**: Discovers roots via `server.listRoots()` (requires MCP SDK 1.18+)
2. **Environment Override**: Set `SWI_MCP_ALLOWED_ROOTS` (comma-separated paths)
3. **Fallback Directory**: `~/.swipl-mcp-server` when no roots available

### Security

- **System Directory Blocking**: Automatically blocks `/etc`, `/usr`, `/bin`, etc.
- **Path Validation**: All paths validated against allowed roots
- **No Parent Traversal**: Blocks `..` in relative paths from fallback directory
- **Strict Mode**: Set `SWI_MCP_STRICT_ROOTS=true` to block fallback directory

### Caching & Notifications

- **5-minute cache TTL**: Reduces excessive MCP protocol calls
- **Notification Support**: Listens for `roots/list_changed` notifications (MCP SDK feature)
- **Automatic Invalidation**: Cache refreshes immediately when roots change

## Configuration

### Environment Variables

- `SWI_MCP_ALLOWED_ROOTS`: Comma-separated list of allowed root paths (overrides client roots)
- `SWI_MCP_STRICT_ROOTS`: Set to `"true"` to disable fallback directory
- `SWI_MCP_USE_LEGACY_DIR`: Set to `"true"` to force use of fallback directory
- `DEBUG`: Set to `"mcp-roots"` for debug logging

## API Reference

### RootsManager (Singleton)

#### Methods

- `getInstance()`: Get singleton instance
- `setServerInstance(server)`: Set MCP server instance
- `discoverRoots(forceRefresh?)`: Discover roots from client
- `getRoots()`: Get all discovered roots
- `validatePath(filePath)`: Validate if path is allowed
- `invalidateCache()`: Force cache refresh
- `getFallbackDir()`: Get fallback directory path

### Types

```typescript
interface RootDirectory {
  uri: string;      // file:// URI
  path: string;     // Absolute filesystem path
  name?: string;    // Optional display name
}

interface PathValidationResult {
  allowed: boolean;
  error?: string;
  matchedRoot?: RootDirectory;
}
```

## License

BSD-3-Clause
