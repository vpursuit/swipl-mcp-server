# @vpursuit/mcp-server-sampling

Generic MCP sampling utilities for server plugins.

## Overview

This package provides reusable infrastructure for implementing MCP sampling in server plugins. It enables servers to request LLM completions from MCP clients, allowing for intelligent, context-aware features like error explanation, query optimization, and more.

## Features

- **Generic Sampling Client**: Low-level utilities for creating sampling requests
- **Error Explainer Framework**: Abstract base class for domain-specific error explanation
- **Type-Safe**: Full TypeScript support with comprehensive type definitions
- **Flexible**: Supports various sampling modes and model preferences

## Key Components

### `samplingClient.ts`

Core utilities for creating sampling requests:

- `isSamplingSupported()`: Check if client supports sampling
- `createSamplingRequest()`: Create a sampling request with full control
- `requestTextCompletion()`: Simplified text-based sampling

### `errorExplainerBase.ts`

Abstract framework for domain-specific error explainers:

```typescript
class MyErrorExplainer extends ErrorExplainerBase {
  protected getSystemPrompt(): string {
    return "You are an expert in my domain...";
  }

  protected formatError(error: unknown): string {
    // Convert error to structured description
  }

  protected getDomainKnowledge(error: unknown): string {
    // Return domain-specific knowledge for this error type
  }
}
```

## Usage Example

```typescript
import { ErrorExplainerBase, isSamplingSupported } from "@vpursuit/mcp-server-sampling";
import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";

class PrologErrorExplainer extends ErrorExplainerBase {
  protected getSystemPrompt() {
    return "You are an expert SWI-Prolog developer...";
  }

  protected formatError(error: unknown) {
    return `Error: ${error.kind} - ${error.message}`;
  }

  protected getDomainKnowledge(error: unknown) {
    return "Prolog-specific knowledge about this error type...";
  }
}

// In your tool handler:
const explainer = new PrologErrorExplainer();
const result = await explainer.explainError(server, {
  error: errorObject,
  context: "Query that caused the error",
});
```

## Requirements

- MCP SDK 1.20.2 or later
- Client must support MCP sampling capability

## Integration

This package is designed to be used by other MCP server plugins. It is not published standalone but used as a workspace dependency within the monorepo.

### As a Dependency

Add to your plugin's `package.json`:

```json
{
  "dependencies": {
    "@vpursuit/mcp-server-sampling": "^1.0.0"
  }
}
```

## Architecture

The sampling infrastructure is designed with separation of concerns:

1. **Generic Layer** (`@vpursuit/mcp-server-sampling`): Reusable sampling utilities
2. **Domain Layer** (e.g., `@vpursuit/mcp-server-prolog`): Domain-specific implementations
3. **Tool Layer**: Exposes sampling features as MCP tools

This architecture allows:
- Code reuse across different domain plugins
- Consistent sampling patterns
- Easy testing and maintenance
- Clear separation between generic and domain-specific logic

## License

BSD-3-Clause
