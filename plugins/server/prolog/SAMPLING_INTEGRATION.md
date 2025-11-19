# MCP Sampling Integration Guide

## Overview

The Prolog MCP server's `explain_error` tool uses MCP sampling to provide AI-powered error explanations. This document explains how clients should integrate with the sampling infrastructure.

## Architecture

The sampling architecture follows proper MCP protocol with bidirectional communication:

1. **Server Side**: Exposes `explain_error` tool and provides `registerSamplingCallback()` function
2. **Client Side**: Registers a callback that performs actual LLM sampling
3. **Protocol Flow**:
   - Client calls `explain_error` tool
   - Server needs LLM help → calls registered sampling callback
   - Client performs LLM call → returns result to server
   - Server formats and returns explanation

## Fallback Behavior

**IMPORTANT**: The `explain_error` tool now has robust fallback behavior:

- **Tier 1 (Preferred)**: AI-powered explanation using MCP sampling
- **Tier 2 (Fallback)**: Rule-based explanation using domain knowledge
- **Tier 3 (Error case)**: Basic error information

This means the tool **always works**, even without sampling registration.

## Client Integration (Optional)

To enable AI-powered explanations, clients should register a sampling callback after connecting to the server.

### Step 1: Import the registration function

```typescript
import { registerSamplingCallback } from "@vpursuit/swipl-mcp-server";
```

### Step 2: Create a sampling handler

```typescript
import Anthropic from "@anthropic-ai/sdk";

const anthropic = new Anthropic({
  apiKey: process.env.ANTHROPIC_API_KEY,
});

async function samplingHandler(options: SamplingRequestOptions): Promise<SamplingResult> {
  try {
    const response = await anthropic.messages.create({
      model: "claude-3-5-sonnet-20241022",
      max_tokens: options.maxTokens,
      temperature: options.temperature || 0.2,
      system: options.systemPrompt,
      messages: options.messages.map(msg => ({
        role: msg.role,
        content: msg.content.text || "",
      })),
    });

    // Extract text from response
    const text = response.content
      .filter(block => block.type === "text")
      .map(block => block.text)
      .join("\n");

    return {
      success: true,
      text,
      model: response.model,
      stopReason: response.stop_reason || undefined,
    };
  } catch (error) {
    return {
      success: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }
}
```

### Step 3: Register the callback after server connection

```typescript
// After successfully connecting to the Prolog MCP server
registerSamplingCallback(samplingHandler, 30000); // 30s timeout
```

## Types Reference

### SamplingRequestOptions

```typescript
interface SamplingRequestOptions {
  messages: Array<{
    role: "user" | "assistant";
    content: {
      type: "text" | "image" | "resource";
      text?: string;
      data?: string;
      mimeType?: string;
    };
  }>;
  systemPrompt?: string;
  maxTokens: number;
  includeContext?: "none" | "thisServer" | "allServers";
  modelPreferences?: {
    hints?: string[];
    intelligencePriority?: number;  // 0-1
    speedPriority?: number;         // 0-1
    costPriority?: number;          // 0-1
  };
  temperature?: number;
  metadata?: Record<string, unknown>;
}
```

### SamplingResult

```typescript
interface SamplingResult {
  success: boolean;
  text?: string;
  model?: string;
  stopReason?: string;
  error?: string;
}
```

## Testing

**Note on Tool Names**: In these examples, `"prolog:explain_error"` assumes your MCP server is configured with the name `"prolog"`. The actual tool name will be `"{serverName}:explain_error"` where `{serverName}` is the `name` field in your ServerConfig. The separator is a single colon `:`.

### Test Without Sampling (Rule-Based Fallback)

Simply call the `explain_error` tool without registering a callback:

```typescript
const result = await server.callTool("prolog:explain_error", {
  error: {
    kind: "syntax_error",
    message: "Invalid Prolog syntax",
    details: { raw: "error(...)" }
  },
  query: "foo(X) :- bar X.",
  include_kb: true
});

// Will return rule-based explanation from errorKnowledge.ts
// structuredContent.sampling_used === false
```

### Test With Sampling (AI-Powered)

After registering the callback:

```typescript
const result = await server.callTool("prolog:explain_error", {
  error: {
    kind: "instantiation_error",
    message: "Variable not bound",
    details: { predicate: "is/2" }
  },
  query: "X is Y + 1, Y = 5.",
  include_kb: true
});

// Will return AI-generated explanation
// structuredContent.sampling_used === true
```

## Error Handling

The sampling infrastructure has comprehensive error handling:

1. **Sampling Not Available**: Falls back to rule-based explanation
2. **Sampling Timeout**: Falls back after 30s (configurable)
3. **Sampling Failed**: Falls back with error logged
4. **Empty Response**: Falls back to rule-based explanation

All errors are logged for debugging but don't break the tool.

## Logs

Enable debug logging to see sampling flow:

```
[requestTextCompletion] Creating sampling request
[ErrorExplainerBase] Starting error explanation
[ErrorExplainerBase] Built prompt, requesting sampling
[requestTextCompletion] Sampling succeeded { textLength: 1234, model: 'claude-...' }
[ErrorExplainerBase] Parsing explanation from LLM response
```

Or fallback:

```
[requestTextCompletion] Sampling provider not available
[ErrorExplainerBase] Sampling provider not available
[explain_error] Sampling failed, using rule-based fallback: { code: 'SAMPLING_NOT_AVAILABLE', ... }
```

## Implementation Notes

### Why Not Use McpServer for Sampling?

The original implementation tried to call `createMessage()` on the `McpServer` object, but this violates MCP protocol:

- **Servers are passive**: They respond to client requests, don't initiate them
- **Only clients have LLM access**: Servers can't directly call LLMs
- **Proper flow**: Server requests → Client performs → Server receives result

This refactoring implements the correct bidirectional protocol via callbacks.

### Architecture Benefits

1. **Protocol-Compliant**: Follows MCP specification for sampling
2. **Isolated**: Sampling is a reusable, generic component
3. **Testable**: Can be mocked/stubbed for testing
4. **Robust**: Graceful fallback when unavailable
5. **Flexible**: Configurable timeout and model preferences

## Migration from Old Implementation

If you were relying on the broken sampling behavior:

**Before** (didn't work):
```typescript
// Old code tried to use serverRef for sampling - always failed
```

**After** (works):
```typescript
// Option 1: Register callback for AI-powered explanations
registerSamplingCallback(yourSamplingHandler);

// Option 2: Use rule-based fallback (no registration needed)
// explain_error will automatically fall back to domain knowledge
```

No client-side changes needed if you're okay with rule-based explanations!
