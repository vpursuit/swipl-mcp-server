# Logging & Tracing

## Log Levels

- `MCP_LOG_LEVEL`: `debug` | `info` | `warn` | `error` | `silent` (default `warn`).
- `DEBUG`: enable debug logs by namespace; set to `swipl-mcp-server` (package name unchanged).

## Wire/Child Tracing

- `SWI_MCP_TRACE`: optional low‑level tracing of the spawned Prolog process (stdout/stderr) and protocol frames.

## Notes

- Logs initialize early to help diagnose startup issues.
- Prefer structured logs (`MCP_LOG_LEVEL`) for normal operation; enable `SWI_MCP_TRACE` only for deep debugging.

