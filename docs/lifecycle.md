# Lifecycle and State

This server runs over the MCP `stdio` transport. The client owns the connection: it starts the process, speaks JSON‑RPC over stdin/stdout, and eventually disconnects. This page explains how lifecycle events interact with in‑memory state, and how to make state durable when needed.

## Process Model

- One Node.js process per MCP connection.
- One SWI‑Prolog child process inside the Node process.
- Tools (e.g., `db_assert`, `query_start`) operate against the single in‑memory knowledge base.

## Shutdown Behavior

- The server exits when:
  - It receives `SIGINT` or `SIGTERM` (typical client shutdown), or
  - The MCP client closes stdio (stdin stream emits `close`).
- On stdio close the server waits a very small grace (~25 ms) before exiting to allow any final responses to flush.
- The Prolog child is stopped cleanly before the Node process exits.

Why close on stdio? It prevents orphaned processes if a client crashes or terminates without sending signals. This aligns with typical MCP `stdio` server behavior.

## State Persistence

- State is **per connection**: asserted facts and rules live in memory for the duration of the connection only.
- When the connection ends (signals or stdio close), the in‑memory KB is discarded, and a fresh KB is created on next start.

This is the expected behavior for `stdio` MCP servers. Clients that rely on shared state should keep a single connection open for the duration of their workflow.

## Client Guidance

- Maintain a single long‑lived `stdio` connection while coordinating multiple tool calls.
- Do not close stdin immediately after sending a request; doing so signals disconnect and will end the server.

## Durable Knowledge Base (optional)

If you want state to survive restarts, use an explicit dump/restore pattern under the allowed directory `~/.swipl-mcp-server/`:

1. Save snapshot:
   - Call `db_dump` and write the result into a file in `~/.swipl-mcp-server/`.
2. Restore on startup:
   - Either script your client to call `db_load` for the snapshot file, or
   - Replay facts via `db_assert_many` after connection initialization.

Notes:
- The server enforces file path restrictions; only `~/.swipl-mcp-server/` is permitted for file operations.
- Avoid secrets or sensitive paths; data under the allowed directory is considered non‑sensitive for the purpose of this server.

## Troubleshooting

- “Facts disappeared between calls”: ensure the MCP client kept the same `stdio` connection open and did not restart the server between calls.
- “Server exited before replying”: confirm the client did not close stdin immediately after sending a request. Keep the stream open until the response is received.

