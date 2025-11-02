# Security Policy - @vpursuit/mcp-server-core

Plugin system for Model Context Protocol servers.

## Overview

This package provides the foundational plugin architecture for building modular MCP servers. While it doesn't execute untrusted code directly, proper plugin validation and isolation are critical for secure server operation.

## Threat Model

**Primary Risks:**
- Malicious plugins registered with the server
- Invalid input to plugin handlers
- Information disclosure through error messages
- Resource exhaustion from misbehaving plugins
- Plugin interference and namespace collisions

**Trust Boundaries:**
- Server code is trusted (developer-controlled)
- Plugins are partially trusted (reviewed before use)
- Plugin inputs are untrusted (from MCP clients)

## Built-in Security Features

### 1. Schema-Based Validation

All plugin tools use Zod schemas for input validation:

```typescript
import { z } from 'zod';

const plugin: Plugin = {
  tools: {
    myTool: {
      inputSchema: z.object({
        input: z.string().max(1000),  // Length limit
        count: z.number().int().min(1).max(100)  // Range validation
      }),
      handler: async (args) => {
        // args is fully validated
        return { success: true, data: { result: args.input } };
      }
    }
  }
};
```

**Protection Against:**
- Type confusion attacks
- Buffer overflow attempts
- Injection attacks (validated strings)
- Resource exhaustion (length/range limits)

### 2. Plugin Isolation

Each plugin operates independently:
- Separate namespace per plugin
- No cross-plugin state sharing by default
- Plugin failures don't crash the server
- Tools, resources, and prompts are scoped

### 3. Error Handling

Plugin loader includes error boundaries:

```typescript
await loadPlugins(server, [plugin1, plugin2], {
  continueOnError: true,  // Don't fail entire server
  logger: customLogger    // Log security events
});
```

**Protection Against:**
- Cascading failures
- Information disclosure in stack traces
- Denial of service from plugin crashes

### 4. Type Safety

TypeScript strict mode enforcement:
- Compile-time type checking
- No implicit `any` types
- Null safety checks
- Prevents common vulnerabilities

## Usage Security Guidelines

### For Plugin Developers

**1. Validate All Inputs:**
```typescript
// ✅ Good: strict validation
inputSchema: z.object({
  filename: z.string()
    .min(1)
    .max(255)
    .regex(/^[a-zA-Z0-9_.-]+$/), // Whitelist characters
  count: z.number().int().positive().max(1000)
})

// ❌ Bad: loose validation
inputSchema: z.object({
  filename: z.string(),  // No limits, allows injection
  count: z.number()      // No bounds, allows DoS
})
```

**2. Handle Errors Safely:**
```typescript
handler: async (args) => {
  try {
    const result = await dangerousOperation(args.input);
    return { success: true, data: { result } };
  } catch (error) {
    // ✅ Good: Generic error message
    return {
      success: false,
      error: 'Operation failed',
      metadata: { timestamp: Date.now() }
    };

    // ❌ Bad: Exposes internals
    // return { error: error.stack };
  }
}
```

**3. Limit Resource Usage:**
```typescript
handler: async (args) => {
  // Set timeouts
  const timeout = setTimeout(() => {
    throw new Error('Operation timeout');
  }, 30000);

  try {
    const result = await operation(args);
    return { success: true, data: result };
  } finally {
    clearTimeout(timeout);
  }
}
```

**4. Avoid Storing Secrets:**
```typescript
// ❌ Bad: Hardcoded secrets
const plugin: Plugin = {
  onInit: async (server) => {
    const apiKey = 'sk-1234567890abcdef';  // Don't do this!
  }
};

// ✅ Good: Use environment variables
const plugin: Plugin = {
  onInit: async (server) => {
    const apiKey = process.env.MY_API_KEY;
    if (!apiKey) {
      throw new Error('MY_API_KEY not configured');
    }
  }
};
```

### For Server Implementers

**1. Review Plugins Before Use:**
- Read plugin source code
- Check for network requests
- Verify input validation
- Test error handling

**2. Use Plugin Loader Options:**
```typescript
await loadPlugins(server, [plugin1, plugin2], {
  continueOnError: true,  // Isolate plugin failures
  logger: {
    info: (msg) => console.log('[Plugin]', msg),
    error: (msg) => console.error('[Plugin]', msg)
  }
});
```

**3. Monitor Plugin Behavior:**
```typescript
const plugin: Plugin = {
  tools: {
    myTool: {
      handler: async (args) => {
        const startTime = Date.now();
        try {
          const result = await operation(args);
          const duration = Date.now() - startTime;

          // Monitor performance
          if (duration > 5000) {
            console.warn(`Slow tool: ${duration}ms`);
          }

          return result;
        } catch (error) {
          // Log security events
          console.error('Tool error:', error.message);
          throw error;
        }
      }
    }
  }
};
```

**4. Implement Rate Limiting:**
```typescript
// Track tool usage per client
const usageMap = new Map<string, number>();

handler: async (args, context) => {
  const clientId = context.clientId;
  const count = usageMap.get(clientId) || 0;

  if (count > 100) {
    throw new Error('Rate limit exceeded');
  }

  usageMap.set(clientId, count + 1);
  // ... rest of handler
}
```

## Plugin API Security

### ToolResponse Format

Standard response format provides structured error handling:

```typescript
interface ToolResponse {
  success: boolean;
  data?: unknown;
  error?: string;
  metadata?: Record<string, unknown>;
}
```

**Security Benefits:**
- Predictable error handling
- No raw exceptions leaked to clients
- Metadata for auditing without exposing internals

### Plugin Lifecycle Hooks

```typescript
const plugin: Plugin = {
  async onInit(server) {
    // ✅ Validate configuration
    // ✅ Initialize resources
    // ❌ Don't store secrets in memory
  },

  async onShutdown() {
    // ✅ Clean up resources
    // ✅ Close connections
    // ✅ Clear sensitive data
  }
};
```

## Configuration

**Environment Variables:**
- `DEBUG`: Set to `"mcp-core"` for debug logging
- Plugin-specific variables defined by each plugin

**Best Practices:**
- Use environment variables for secrets
- Validate configuration at startup
- Fail fast on misconfiguration
- Log configuration errors (without secrets)

## Reporting Vulnerabilities

If you discover a security vulnerability in @vpursuit/mcp-server-core:

1. **Do not** open a public GitHub issue
2. Use [GitHub Security Advisories](https://github.com/vpursuit/swipl-mcp-server/security/advisories/new) for private reporting
3. Include:
   - Affected version
   - Proof of Concept (TypeScript code)
   - Expected vs. actual behavior
   - Impact assessment

See the [root SECURITY.md](../../SECURITY.md) for ecosystem-wide security policy.

## Testing Security

**Test Input Validation:**
```typescript
// Should reject invalid input
const result = await toolHandler({
  filename: '../../../etc/passwd'  // Path traversal
});
// Expected: Zod validation error
```

**Test Error Handling:**
```typescript
// Should not expose stack traces
const result = await toolHandler({
  input: 'trigger-error'
});
// Expected: Generic error message, no stack trace
```

**Test Plugin Isolation:**
```typescript
// One plugin failure shouldn't affect others
await loadPlugins(server, [brokenPlugin, workingPlugin], {
  continueOnError: true
});
// Expected: workingPlugin still functions
```

## Resources

- [Zod Documentation](https://zod.dev)
- [Root Security Policy](../../SECURITY.md)
- [Package README](./README.md)
- [Model Context Protocol](https://modelcontextprotocol.io)

## License

BSD-3-Clause - See [LICENSE](./LICENSE) file for details.
