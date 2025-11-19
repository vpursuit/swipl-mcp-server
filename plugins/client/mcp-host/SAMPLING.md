# MCP Host Sampling Implementation

This document describes how sampling works in the MCP Host client library and how to integrate it into your application.

## Overview

The MCP Host library implements the Model Context Protocol's sampling feature, which allows MCP servers to request LLM completions from the connected client. This enables servers to use AI capabilities for tasks like error explanation, code generation, and intelligent analysis.

## Architecture

### Flow Diagram

```
┌─────────────┐         ┌──────────────┐         ┌───────────────┐
│ MCP Server  │         │  MCP Host    │         │  Application  │
│  (Prolog)   │         │  (Client)    │         │   (Chatbot)   │
└──────┬──────┘         └──────┬───────┘         └───────┬───────┘
       │                       │                         │
       │ 1. createMessage()    │                         │
       ├──────────────────────>│                         │
       │                       │                         │
       │                       │ 2. Approval Request     │
       │                       ├────────────────────────>│
       │                       │                         │
       │                       │ 3. { approved, llmClient }
       │                       │<────────────────────────┤
       │                       │                         │
       │                       │ 4. LLM Call             │
       │                       ├────────────────────────>│
       │                       │                         │
       │                       │ 5. LLM Response         │
       │                       │<────────────────────────┤
       │                       │                         │
       │ 6. CreateMessageResult│                         │
       │<──────────────────────┤                         │
       │                       │                         │
```

### Components

1. **SamplingHandler** - Core class that processes sampling requests
2. **SamplingApprovalCallback** - User-provided callback for approval
3. **LLM Client** - OpenAI-compatible client for making completions

## Implementation Guide

### 1. Extend SamplingApproval

When your application receives a sampling approval request, return both approval status and an LLM client:

```typescript
import type { SamplingApproval, SamplingRequest } from '@vpursuit/mcp-client-core';
import OpenAI from 'openai';

async function handleSamplingApproval(
  request: SamplingRequest
): Promise<SamplingApproval> {
  // Show user prompt or check permissions
  const userApproved = await askUser(
    `Server "${request.serverName}" wants to use AI. Allow?`
  );

  if (!userApproved) {
    return {
      approved: false,
      reason: "User denied the request",
    };
  }

  // Create OpenAI client (or any OpenAI-compatible client)
  const llmClient = new OpenAI({
    apiKey: process.env.OPENAI_API_KEY,
    // Or use a local LM Studio server:
    // baseURL: "http://localhost:1234/v1",
  });

  return {
    approved: true,
    llmClient,  // Provide the LLM client
  };
}
```

### 2. Register Approval Callback

Pass your approval callback when creating the MCP Host:

```typescript
import { McpHost } from '@vpursuit/mcp-host';

const host = new McpHost({
  servers: [
    {
      name: "prolog-server",
      command: "npx",
      args: ["-y", "@vpursuit/swipl-mcp-server"],
    },
  ],
  samplingApprovalCallback: handleSamplingApproval,  // Register callback
});

await host.connect();
```

### 3. Server-Side Usage

MCP servers can now request sampling:

```typescript
import { McpProtocolSamplingProvider } from '@vpursuit/mcp-server-sampling';

const samplingProvider = new McpProtocolSamplingProvider(mcpServer);

const result = await samplingProvider.requestSampling({
  messages: [
    {
      role: "user",
      content: {
        type: "text",
        text: "Explain this error: undefined variable X",
      },
    },
  ],
  systemPrompt: "You are a Prolog expert",
  maxTokens: 500,
  includeContext: "none",
});

if (result.success) {
  console.log("AI response:", result.text);
}
```

## Configuration

### Timeout

Configure sampling timeout when creating the SamplingHandler:

```typescript
const handler = new SamplingHandler(
  connections,
  approvalCallback,
  60000  // 60 second timeout (default: 30000)
);
```

### Model Selection

The server can provide model hints via `modelPreferences`:

```typescript
const result = await samplingProvider.requestSampling({
  messages: [...],
  modelPreferences: {
    hints: [{ name: "gpt-4" }],  // Prefer GPT-4
    intelligencePriority: 0.9,   // High intelligence priority
    costPriority: 0.3,            // Low cost priority
  },
});
```

The MCP Host will use the first hint as the model name. If no hints are provided, it defaults to `gpt-4`.

## Message Format Conversion

The MCP Host automatically converts between MCP and OpenAI formats:

### MCP → OpenAI

```typescript
// MCP format (input)
{
  role: "user",
  content: {
    type: "text",
    text: "Hello"
  }
}

// Converted to OpenAI format
{
  role: "user",
  content: "Hello"
}
```

### OpenAI → MCP

```typescript
// OpenAI response
{
  choices: [{
    message: {
      role: "assistant",
      content: "Hi there!"
    },
    finish_reason: "stop"
  }],
  model: "gpt-4"
}

// Converted to MCP format
{
  role: "assistant",
  content: {
    type: "text",
    text: "Hi there!"
  },
  model: "gpt-4",
  stopReason: "stop"
}
```

## Error Handling

### Common Errors

1. **No LLM Client Provided**
   ```
   Error: Sampling approved but no LLM client provided in approval
   ```
   **Solution**: Return `llmClient` in your approval response

2. **Timeout**
   ```
   Error: Sampling request timed out after 30000ms
   ```
   **Solution**: Increase timeout or check LLM server connectivity

3. **LLM Call Failed**
   ```
   Error: Sampling request failed: Rate limit exceeded
   ```
   **Solution**: Check LLM API limits or use exponential backoff

### Graceful Degradation

Servers should handle sampling failures gracefully:

```typescript
const result = await samplingProvider.requestSampling({...});

if (!result.success) {
  console.warn("Sampling failed, using fallback:", result.error);
  // Use rule-based fallback
  return getFallbackExplanation(error);
}

return result.text;
```

## Testing

The library includes comprehensive tests. Run them with:

```bash
npm test
```

### Mock LLM Client

For testing, create a mock client:

```typescript
const mockClient = {
  chat: {
    completions: {
      create: async (params) => ({
        id: "test",
        model: "gpt-4",
        choices: [{
          message: {
            role: "assistant",
            content: "Mock response",
          },
          finish_reason: "stop",
        }],
      }),
    },
  },
};
```

## Security Considerations

1. **User Approval**: Always ask for user consent before allowing sampling
2. **Rate Limiting**: Implement rate limits to prevent abuse
3. **Cost Control**: Monitor token usage and set budgets
4. **Data Privacy**: Be aware that prompts are sent to the LLM provider
5. **Audit Logging**: Log sampling requests for security audits

## Examples

### Example 1: Claude Desktop Integration

```typescript
import Anthropic from '@anthropic-ai/sdk';

const approvalCallback = async (request) => {
  const anthropic = new Anthropic({
    apiKey: process.env.ANTHROPIC_API_KEY,
  });

  return {
    approved: true,
    llmClient: {
      chat: {
        completions: {
          create: async (params) => {
            const response = await anthropic.messages.create({
              model: params.model || "claude-3-5-sonnet-20241022",
              max_tokens: params.max_tokens,
              messages: params.messages,
            });
            // Convert to OpenAI format
            return {
              model: response.model,
              choices: [{
                message: {
                  role: "assistant",
                  content: response.content[0].text,
                },
                finish_reason: response.stop_reason,
              }],
            };
          },
        },
      },
    },
  };
};
```

### Example 2: Local LM Studio

```typescript
import OpenAI from 'openai';

const approvalCallback = async (request) => {
  const client = new OpenAI({
    baseURL: "http://localhost:1234/v1",
    apiKey: "lm-studio",  // Not validated by LM Studio
  });

  return {
    approved: true,
    llmClient: client,
  };
};
```

## Troubleshooting

### Debug Logging

Enable debug logging to see sampling flow:

```typescript
// SamplingHandler logs with [SamplingHandler] prefix
// Check console for:
// - "Handling sampling request from {server}"
// - "Converted N messages to OpenAI format"
// - "Calling LLM with model: {model}"
// - "LLM call succeeded" or "Sampling failed"
```

### Common Issues

| Issue | Cause | Solution |
|-------|-------|----------|
| "Server is not connected" | Server disconnected or not started | Check server status with `host.getStatus()` |
| "Sampling request denied" | Approval callback returned false | Check approval logic |
| Timeout errors | LLM too slow or unresponsive | Increase timeout or check LLM server |
| Empty responses | LLM returned no content | Check LLM parameters (temperature, max_tokens) |

## Further Reading

- [MCP Protocol Specification](https://spec.modelcontextprotocol.io/)
- [Server-Side Sampling Guide](../server/sampling/README.md)
- [Prolog Error Explanation](../server/prolog/SAMPLING_INTEGRATION.md)
