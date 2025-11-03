# @vpursuit/mcp-server-core

**Internal plugin system for Model Context Protocol (MCP) servers.**

> ⚠️ **Internal Package**: This plugin is private to the [@vpursuit/swipl-mcp-server](https://github.com/vpursuit/model-context-lab) monorepo and is bundled into the main product. It is not published separately to npm.

## Overview

`@vpursuit/mcp-server-core` provides a simple, export-based plugin architecture for building modular MCP servers. It defines standard interfaces for plugins and provides utilities for loading plugins into an MCP server.

## Installation

**For monorepo development only:**

```bash
# Clone the monorepo
git clone https://github.com/vpursuit/model-context-lab.git
cd model-context-lab
npm install
npm run build
```

This package is available as a workspace dependency within the monorepo.

## Core Concepts

### Plugin

A plugin is a JavaScript module that exports a `Plugin` object containing:
- **name**: Unique plugin identifier
- **version**: Semantic version
- **description**: Short description of functionality
- **tools**: Tool definitions (optional)
- **resources**: Resource definitions (optional)
- **prompts**: Prompt definitions (optional)
- **onInit**: Initialization hook (optional)
- **onShutdown**: Cleanup hook (optional)

### Plugin Loader

The `loadPlugins()` function registers all features from an array of plugins into an MCP server instance.

## Usage

### Creating a Plugin

```typescript
import type { Plugin } from '@vpursuit/mcp-server-core';
import { z } from 'zod';

export const plugin: Plugin = {
  name: 'my-plugin',
  version: '1.0.0',
  description: 'My MCP plugin',

  tools: {
    myTool: {
      description: 'Does something useful',
      inputSchema: z.object({
        input: z.string(),
      }),
      handler: async (args) => {
        return {
          success: true,
          data: { result: `Processed: ${args.input}` },
        };
      },
    },
  },

  async onInit(server) {
    console.log('Plugin initialized');
  },
};
```

### Loading Plugins

```typescript
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { loadPlugins } from '@vpursuit/mcp-server-core';
import { plugin as myPlugin } from './my-plugin.js';

const server = new McpServer({
  name: 'my-mcp-server',
  version: '1.0.0',
});

// Load plugins
await loadPlugins(server, [myPlugin]);

// Start server
const transport = new StdioServerTransport();
await server.connect(transport);
```

## API Reference

### Types

#### `Plugin`
Main plugin interface with tools, resources, prompts, and lifecycle hooks.

#### `ToolDefinition`
Defines a tool with description, Zod schema, and handler function.

#### `ResourceDefinition`
Defines a resource with URI, name, description, and handler.

#### `PromptDefinition`
Defines a prompt with name, arguments, and handler.

#### `ToolResponse`
Standard response format for tool handlers with success, data, error, and metadata fields.

### Functions

#### `loadPlugin(server, plugin, config?)`
Load a single plugin into the MCP server.

#### `loadPlugins(server, plugins, config?)`
Load multiple plugins into the MCP server.

### Configuration

Both loader functions accept an optional `PluginLoaderConfig`:
- `continueOnError`: Continue loading other plugins if one fails (default: false)
- `logger`: Custom logger for plugin loading events

## Examples

See the [mcp-server-roots](../roots) and [mcp-server-prolog](../prolog) plugins in this monorepo for real-world plugin examples.

## License

BSD-3-Clause
