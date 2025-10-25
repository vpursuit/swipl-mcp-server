# Architecture

## Components

- PrologInterface (Node): manages the SWI‑Prolog process, request/response correlation, and session state.
- Tools (Node): MCP tool handlers (`knowledge_base_load`, `query_start`, etc.).
- prolog_server.pl (SWI‑Prolog): single server supporting standard and engine modes.

## Query Modes

- Standard mode: deterministic pagination via `call_nth/2`.
- Engine mode: true iterator semantics using SWI engines.
- Mutual exclusion: only one mode active at a time; close before switching.

## Wire Protocol

- UTF‑8 terms, one per line.
- Requests: `cmd(ID, Term)`; Replies: `id(ID, Reply)`.
- Backward compatible: bare terms accepted; client always envelopes.
- Reply shapes: `ok`, `done`, `no_more_solutions`, `solution(Bindings)`, `error(Term)`.

## Security

- Hybrid model: `library(sandbox)` validation + explicit blacklist (I/O, OS, network, directives).
- Guarded consultation: accepts facts/rules only; directives rejected.
- User predicates in `knowledge_base` module; unknown predicates fail by default.
- Timeouts on startup and queries for protection.

