# SWI-Prolog MCP Server Terminal UI Project

## Project Overview
Interactive terminal/UI for monitoring and controlling the SWI-Prolog MCP Server. Provides real-time visibility into knowledge base operations and allows direct user interaction with Prolog.

**Educational Value**: This terminal UI serves as a powerful learning tool for both Prolog programming and AI agent reasoning:
- **Learn Prolog**: Watch how AI agents construct and query knowledge bases in real-time
- **Understand AI Reasoning**: See the step-by-step logic process AI agents use to solve problems
- **Demystify AI Decision-Making**: Observe how AI agents build facts, create rules, and derive conclusions
- **Interactive Learning**: Experiment alongside the AI by adding your own facts and queries

## Objectives
- **Real-time Monitoring**: Display all MCP operations as they happen
- **Interactive Console**: Allow users to execute Prolog commands directly
- **Visual Feedback**: Show assertions, retractions, queries, and results
- **Session Management**: Track and export session history
- **Educational Transparency**: Make AI agent reasoning visible and understandable
- **Learning Enhancement**: Provide insights into logic programming and AI problem-solving

## Architecture Decision

### Selected Approach: Web-Based Terminal (Lightweight)
We'll implement a lightweight web-based terminal that runs alongside the MCP server, providing a browser-based interface for monitoring and interaction.

### Key Components

#### 1. Event Broadcasting System
- **Location**: `src/ui/EventBroadcaster.ts`
- **Purpose**: Capture and broadcast all Prolog operations
- **Implementation**:
  ```typescript
  interface PrologEvent {
    timestamp: Date;
    type: 'assert' | 'retract' | 'query' | 'result' | 'error';
    command: string;
    result?: any;
    error?: string;
  }
  ```

#### 2. WebSocket Server
- **Location**: `src/ui/WebSocketServer.ts`
- **Port**: 8080 (configurable via `SWI_MCP_UI_PORT`)
- **Features**:
  - Broadcast events to connected clients
  - Accept commands from web terminal
  - Manage client sessions

#### 3. Web Terminal Interface
- **Location**: `src/ui/terminal.html`
- **Libraries**: 
  - xterm.js for terminal emulation
  - WebSocket API for real-time communication
- **Features**:
  - Terminal display with color support
  - Command input with history
  - Syntax highlighting for Prolog

## How to Access the Terminal UI

### Option 1: Via Web Browser (Primary Method)
When the MCP server starts with the UI enabled, users would:

1. **Enable the Terminal UI in Claude Desktop config:**
```json
{
  "mcpServers": {
    "swipl": {
      "command": "npx",
      "args": ["@vpursuit/swipl-mcp-server"],
      "env": {
        "SWI_MCP_UI_ENABLED": "true",
        "SWI_MCP_UI_PORT": "8080"
      }
    }
  }
}
```

2. **Open browser and navigate to:**
   - `http://localhost:8080` (default)
   - The terminal UI automatically connects to the WebSocket server

### Option 2: Auto-Launch Browser (Enhanced Experience)
The server can automatically open the browser when UI is enabled:
- Set `SWI_MCP_UI_AUTO_OPEN=true` in environment
- Browser opens automatically on server start
- URL displayed in console logs

### Option 3: Integrated Express Server
Serve the HTML directly from the MCP server:
- Express serves terminal.html at root path
- WebSocket upgrades on `/ws` path
- Single port for both HTTP and WebSocket

### Terminal Access Flow
```
1. User starts Claude Desktop
2. MCP server launches with SWI_MCP_UI_ENABLED=true
3. WebSocket server starts on port 8080
4. Server logs: "Terminal UI available at http://localhost:8080"
5. User opens browser to localhost:8080
6. Terminal UI connects via WebSocket
7. Real-time monitoring begins
```

## Implementation Phases

### Phase 1: Core Infrastructure (Week 1)
- [ ] Create EventBroadcaster class
- [ ] Integrate event hooks into PrologInterface
- [ ] Add event emissions to tool handlers
- [ ] Create event type definitions

### Phase 2: WebSocket Server (Week 1-2)
- [ ] Install WebSocket dependencies
- [ ] Implement WebSocketServer class
- [ ] Add connection management
- [ ] Create message protocol

### Phase 3: Web Terminal UI (Week 2-3)
- [ ] Create HTML terminal interface
- [ ] Implement xterm.js integration
- [ ] Add WebSocket client
- [ ] Style terminal appearance

### Phase 4: Integration (Week 3-4)
- [ ] Update index.ts for optional UI
- [ ] Add environment configuration
- [ ] Create direct command execution path
- [ ] Ensure backward compatibility

### Phase 5: Enhancements (Week 4-5)
- [ ] Add syntax highlighting
- [ ] Implement command history
- [ ] Add autocomplete suggestions
- [ ] Create export functionality

## Technical Specifications

### Environment Variables
```bash
SWI_MCP_UI_ENABLED=true        # Enable terminal UI
SWI_MCP_UI_PORT=8080          # WebSocket server port
SWI_MCP_UI_HOST=localhost     # WebSocket server host
```

### WebSocket Protocol
```javascript
// Client -> Server
{
  type: 'command',
  command: 'parent(john, mary).',
  id: 'cmd-123'
}

// Server -> Client
{
  type: 'event',
  event: {
    timestamp: '2025-09-07T10:00:00Z',
    type: 'assert',
    command: 'assert(parent(john, mary))',
    result: 'ok'
  }
}

// Server -> Client (broadcast)
{
  type: 'result',
  id: 'cmd-123',
  success: true,
  result: 'ok'
}
```

### File Structure
```
swipl-mcp-server/
├── src/
│   ├── ui/
│   │   ├── EventBroadcaster.ts
│   │   ├── WebSocketServer.ts
│   │   ├── terminal.html
│   │   ├── terminal.css
│   │   └── terminal.js
│   ├── index.ts (modified)
│   ├── PrologInterface.ts (modified)
│   └── tools.ts (modified)
├── test/
│   └── ui/
│       ├── EventBroadcaster.test.ts
│       └── WebSocketServer.test.ts
└── package.json (updated dependencies)
```

## Dependencies
```json
{
  "dependencies": {
    "ws": "^8.x",
    "express": "^4.x"
  },
  "devDependencies": {
    "@types/ws": "^8.x",
    "@types/express": "^4.x"
  }
}
```

## Testing Strategy

### Unit Tests
- EventBroadcaster event emission
- WebSocketServer message handling
- Protocol serialization/deserialization

### Integration Tests
- WebSocket connection lifecycle
- Command execution flow
- Event broadcasting

### E2E Tests
- Terminal UI interaction
- Real Prolog operations
- Session management

## UI Mockup

```
┌─────────────────────────────────────────────────────┐
│ SWI-Prolog MCP Server Terminal        [◉] Connected │
├─────────────────────────────────────────────────────┤
│ [10:00:01] 🤖 AI: Asserting family relationships   │
│ [10:00:01] ✓ Asserted: parent(john, mary).         │
│ [10:00:05] 🤖 AI: Checking who are parents         │
│ [10:00:05] → Query: parent(X, Y)                   │
│ [10:00:05] ← Result: X=john, Y=mary                │
│ [10:00:10] ✓ Asserted: parent(jane, bob).          │
│ [10:00:15] 🤖 AI: Finding all parent relationships │
│ [10:00:15] → Query: parent(X, Y)                   │
│ [10:00:15] ← Result: X=john, Y=mary                │
│ [10:00:15] ← Result: X=jane, Y=bob                 │
│ [10:00:18] 👤 User: Testing query                  │
│ [10:00:18] → Query: parent(john, X)                │
│ [10:00:18] ← Result: X=mary                        │
│ [10:00:20] ✗ Error: Unknown predicate: foo/1       │
│                                                     │
├─────────────────────────────────────────────────────┤
│ ?- parent(john, X).                            [↵] │
└─────────────────────────────────────────────────────┘
[Learning Mode: ON] [Show AI Reasoning: ON]
```

### Enhanced UI Features for Learning

```
┌─────────────────────────────────────────────────────────────┐
│ SWI-Prolog Learning Terminal    [◉] AI Agent: Claude       │
├─────────────────────────────────────────────────────────────┤
│ 📚 Learning Context: Building a Family Tree Knowledge Base  │
├─────────────────────────────────────────────────────────────┤
│ [10:00:01] 🤖 AI Reasoning:                                │
│   "I need to establish parent-child relationships.         │
│    Starting with basic facts about John and Mary."         │
│                                                             │
│ [10:00:01] ✓ assert(parent(john, mary))                    │
│   → Knowledge Base Updated:                                │
│     • parent(john, mary) - NEW FACT                        │
│                                                             │
│ [10:00:05] 🤖 AI Reasoning:                                │
│   "Let me verify the fact was added correctly by           │
│    querying all parent relationships."                     │
│                                                             │
│ [10:00:05] → query: parent(X, Y)                          │
│   ← Solution 1: X=john, Y=mary                            │
│   ← No more solutions                                     │
│                                                             │
│ [10:00:10] 🤖 AI Reasoning:                                │
│   "Adding a rule to derive grandparent relationships       │
│    from parent facts."                                     │
│                                                             │
│ [10:00:10] ✓ assert((grandparent(X,Z) :-                 │
│                      parent(X,Y), parent(Y,Z)))           │
│   → Knowledge Base Updated:                                │
│     • grandparent/2 - NEW RULE (uses: parent/2)           │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ 💡 Try it yourself: grandparent(john, Who)?           [↵] │
└─────────────────────────────────────────────────────────────┤
[Tutorial] [Explain This] [Save Session] [Knowledge Graph]
```

## Success Criteria
- [ ] Real-time display of all MCP operations
- [ ] Direct Prolog command execution
- [ ] No performance impact on MCP server
- [ ] Works with existing MCP clients
- [ ] Clean separation of concerns
- [ ] Comprehensive test coverage

## Risks and Mitigations
| Risk | Mitigation |
|------|------------|
| Performance overhead | Make UI optional, use efficient event system |
| Security concerns | Localhost only by default, add auth option |
| Compatibility issues | Maintain backward compatibility, feature flag |
| Complex debugging | Comprehensive logging, clear error messages |

## References
- [xterm.js Documentation](https://xtermjs.org/)
- [WebSocket API](https://developer.mozilla.org/en-US/docs/Web/API/WebSocket)
- [MCP Protocol Specification](https://modelcontextprotocol.org/)
- [SWI-Prolog Documentation](https://www.swi-prolog.org/)