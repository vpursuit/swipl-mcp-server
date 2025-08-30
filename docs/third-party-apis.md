# Third-Party API Documentation

## Overview

This document provides comprehensive documentation for the third-party APIs and libraries used in the SWI-Prolog MCP Server. Understanding these APIs is essential for maintaining, extending, and troubleshooting the server.

## Core Dependencies

### 1. Model Context Protocol TypeScript SDK (@modelcontextprotocol/sdk)

**Version**: ^1.17.2  
**Purpose**: Core MCP framework for building servers that integrate with AI applications

#### Key Components Used

- **McpServer**: Main server class for handling MCP protocol
- **StdioServerTransport**: Transport for stdio communication
- **ResourceTemplate**: For dynamic resource definitions
- **Tool Registration**: Methods for registering server tools

#### Example Usage in Codebase
```typescript
// From src/index.ts
import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";

const server = new McpServer({
  name: "swipl-mcp-server", 
  version: resolvePackageVersion()
});

const transport = new StdioServerTransport();
await server.connect(transport);
```

#### Key Methods and Features

- **Server Initialization**: Create MCP server with name/version metadata
- **Tool Registration**: `server.registerTool()` for exposing server capabilities
- **Transport Connection**: Connect to stdio or HTTP transports
- **Error Handling**: Built-in error handling and validation

#### Documentation Links
- [Official MCP TypeScript SDK Documentation](https://github.com/modelcontextprotocol/typescript-sdk)
- [MCP Specification](https://github.com/modelcontextprotocol/specification)

---

### 2. Zod Schema Validation (zod)

**Version**: ^3.25.76  
**Purpose**: TypeScript-first schema declaration and validation library

#### Key Features Used

- **Schema Definition**: Define input/output schemas for tools
- **Type Inference**: Automatic TypeScript type generation from schemas
- **Runtime Validation**: Parse and validate data at runtime
- **Error Handling**: Detailed validation error messages

#### Example Usage in Codebase
```typescript
// From src/schemas.ts
import { z } from "zod";

export const zodSchemas = {
  prolog_query: z.object({
    query: z.string().describe("Prolog query to execute"),
    timeout_ms: z.number().optional().describe("Query timeout in milliseconds")
  }),
  
  prolog_assert: z.object({
    fact: z.string().describe("Prolog fact to assert"),
    functor: z.string().optional().describe("Functor name for organization")
  })
};
```

#### Type Inference Benefits
```typescript
// Automatic type generation
type QueryParams = z.infer<typeof zodSchemas.prolog_query>;
// Results in: { query: string; timeout_ms?: number | undefined }
```

#### Validation Examples
```typescript
// Safe parsing with error handling
const result = zodSchemas.prolog_query.safeParse(inputData);
if (result.success) {
  const { query, timeout_ms } = result.data;
  // Type-safe access to validated data
} else {
  console.error("Validation errors:", result.error.errors);
}
```

#### Documentation Links
- [Zod Documentation](https://github.com/colinhacks/zod)
- [Zod Type Inference Guide](https://github.com/colinhacks/zod#type-inference)

---

### 3. SWI-Prolog (External Process)

**Purpose**: Logic programming engine executed as external process
**Communication**: Stdin/stdout with custom protocol

#### Integration Architecture

The server communicates with SWI-Prolog through:
1. **Process Spawning**: `child_process.spawn()` to launch `swipl`
2. **Custom Protocol**: Line-based communication protocol
3. **Query Management**: Session management and query lifecycle
4. **Error Handling**: Prolog error parsing and reporting

#### Example Integration
```typescript
// From src/PrologInterface.ts
import { spawn, ChildProcess } from "child_process";

class PrologInterface {
  private prologProcess: ChildProcess;
  
  async startProlog(): Promise<void> {
    this.prologProcess = spawn('swipl', ['-q', '-s', scriptPath]);
    // Setup stdin/stdout communication
  }
  
  async query(query: string, timeout?: number): Promise<QueryResult> {
    // Send query to Prolog process and await response
  }
}
```

#### Communication Protocol
- **Query Format**: JSON-based queries sent via stdin
- **Response Format**: Structured JSON responses from stdout
- **Error Handling**: Prolog errors captured and transformed
- **Timeout Management**: Configurable query timeouts

#### Security Considerations
- **Sandboxing**: Limited Prolog predicates available
- **Query Validation**: Input sanitization and validation
- **Resource Limits**: Memory and execution time constraints

---

## Development Dependencies

### TypeScript and Build Tools
- **@types/node**: Node.js type definitions
- **typescript**: TypeScript compiler
- **@typescript-eslint/***: TypeScript ESLint plugins

### Testing Framework
- **vitest**: Modern test runner with TypeScript support
- **@vitest/coverage-v8**: Code coverage reporting

### Code Quality
- **eslint**: JavaScript/TypeScript linting
- **prettier**: Code formatting

---

## API Integration Patterns

### 1. MCP Tool Pattern
```typescript
server.registerTool(
  "tool-name",
  {
    title: "Human Readable Title",
    description: "Tool description",
    inputSchema: zodSchema
  },
  async (params) => {
    // Tool implementation
    return {
      content: [{ type: "text", text: result }]
    };
  }
);
```

### 2. Schema Validation Pattern
```typescript
// Define schema
const schema = z.object({
  param: z.string(),
  optional: z.number().optional()
});

// Validate input
const validated = schema.parse(input);

// Use type-safe data
doSomething(validated.param);
```

### 3. Error Handling Pattern
```typescript
try {
  const result = await prologInterface.query(query);
  return { content: [{ type: "text", text: result }] };
} catch (error) {
  return {
    content: [{ type: "text", text: `Error: ${error.message}` }],
    isError: true
  };
}
```

---

## Best Practices

### Schema Design
1. **Descriptive Schemas**: Use `.describe()` for parameter documentation
2. **Optional Parameters**: Mark optional fields appropriately
3. **Validation**: Leverage Zod's built-in validators

### Error Handling
1. **Graceful Degradation**: Handle API errors without crashing
2. **User-Friendly Messages**: Transform technical errors to readable messages
3. **Logging**: Use structured logging for debugging

### Performance
1. **Connection Reuse**: Maintain persistent connections when possible
2. **Timeouts**: Implement reasonable timeout values
3. **Resource Cleanup**: Properly close connections and processes

---

## Troubleshooting

### Common Issues

#### MCP Connection Issues
- **Symptom**: Server fails to start or connect
- **Solution**: Check transport configuration and MCP client compatibility

#### Zod Validation Errors
- **Symptom**: Runtime validation failures
- **Solution**: Review schema definitions and input data structure

#### SWI-Prolog Process Issues
- **Symptom**: Prolog queries fail or hang
- **Solution**: Verify SWI-Prolog installation and process communication

### Debugging Tools
1. **MCP Inspector**: Use `@modelcontextprotocol/inspector` for protocol debugging
2. **Logging**: Enable debug logging with `SWI_MCP_TRACE=1`
3. **Schema Testing**: Test Zod schemas in isolation

---

## Migration and Updates

### Updating Dependencies
1. **Test Coverage**: Ensure comprehensive tests before updates
2. **Breaking Changes**: Review changelogs for breaking changes
3. **Type Compatibility**: Verify TypeScript compatibility

### API Compatibility
- **MCP Protocol**: Monitor MCP specification changes
- **Zod Versions**: Handle schema migration for major Zod updates
- **Node.js**: Maintain compatibility with supported Node.js versions

---

## Additional Resources

- [Model Context Protocol Specification](https://spec.modelcontextprotocol.io/)
- [Zod Documentation](https://zod.dev/)
- [SWI-Prolog Documentation](https://www.swi-prolog.org/pldoc/)
- [Node.js Child Process Documentation](https://nodejs.org/api/child_process.html)