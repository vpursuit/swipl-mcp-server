# Session State Machine

This document illustrates the session state machine used by `PrologInterface` to coordinate query and engine sessions.

## Diagram (Mermaid)

```mermaid
stateDiagram-v2
    [*] --> idle

    idle --> query: startQuery
    query --> query_completed: nextSolution → eof
    query --> idle: error
    query_completed --> closing_query: closeQuery
    closing_query --> idle

    idle --> engine: startEngine
    engine --> engine_completed: nextEngine → eof
    engine --> idle: error
    engine_completed --> closing_engine: closeEngine
    closing_engine --> idle

    note right of query_completed: Retains context to return
      "no more solutions" until closed
    note right of engine_completed: Retains context to return
      "no more solutions" until closed
```

## ASCII Overview

```
        startQuery                   nextSolution (eof)
 idle  ─────────────▶  query  ─────────────────────────▶  query_completed
   ▲                         │                 closeQuery │
   │       error             │                           ▼
   └─────────────────────────┘                    closing_query ──▶ idle

        startEngine                   nextEngine (eof)
 idle  ─────────────▶  engine ─────────────────────────▶  engine_completed
   ▲                         │                 closeEngine │
   │       error             │                           ▼
   └─────────────────────────┘                    closing_engine ─▶ idle
```

## Notes

- Exactly one session type can be active at a time (query or engine).
- The `*_completed` states keep context so that subsequent `next` calls respond with "no more solutions" until explicitly closed.
- Transient `closing_*` states serialize shutdown before new sessions begin.
- Invalid transitions are logged when `SWI_MCP_TRACE=1`.

