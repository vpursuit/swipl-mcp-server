# Security Policy

## ⚠️ Important Warning

**Use this tool at your own risk.** This SWI-Prolog MCP Server provides controlled access to a Prolog environment and executes user-provided queries and code. While comprehensive security measures have been implemented including sandboxing and blacklisting, **no security system is perfect**. 

**Security Layer:** This server includes a hybrid security framework that:
- Validates predicates using SWI-Prolog's `library(sandbox)`
- Maintains an explicit blacklist of dangerous operations
- Isolates user code in a dedicated `kb` module
- Rejects dangerous directives during file consultation

**All usage is at your own risk.** Review and understand any Prolog code before execution, especially from untrusted sources.

## Supported Versions
- Runtime: Node.js ≥ 18, SWI‑Prolog ≥ 9.2.x
- Project: `main` branch is maintained. Use the latest release for fixes.

## Reporting a Vulnerability
- Prefer a private report first to avoid exposure.
- If this repository is on GitHub, use Security Advisories (private) or open a minimal issue and mark it as security‑related.
- Include: affected version/commit, environment, minimal PoC, expected vs. actual behavior, and any logs (redact secrets, paths, and PII).

## Threat Model (Scope)
- Untrusted Prolog content provided via file consultation and query execution.
- Potential risks: arbitrary file/OS access, network calls, long‑running or looping queries, information leakage through logs.

## Built‑in Protections
- **Hybrid Security**: Combines `library(sandbox)` validation with explicit blacklist for critical operations.
  - `library(sandbox)` validates most built-ins as safe/unsafe
  - Additional blacklist prevents dangerous operations: `call/1`, `assert/1`, `system/1`, `shell/1`, etc.
  - User-defined predicates in `kb` module are allowed for recursive definitions
- **Safe consult**: Only facts/rules are accepted; directives and module‑altering terms are rejected.
- **Isolation**: User data lives in `kb`; `unknown=fail` to avoid accidental calls.
- **Timeouts**: Node side enforces query timeouts to prevent hangs.
- **Logging hygiene**: Logs go to `stderr`, default to `warn`, and redact absolute paths/PIDs.

## Hardening Checklist
- Run as a non‑privileged user; avoid filesystem write access beyond the working directory.
- Keep Node.js, SWI‑Prolog, and dependencies up to date.
- Set conservative logging in production: `MCP_LOG_LEVEL=warn` or `silent`.
- Do not ingest untrusted `.pl` files from unknown sources without review.
- Monitor resource usage; consider external CPU/time limits if high‑risk.

## Installation Security

For enhanced security and stability, consider **downloading from GitHub** instead of using NPM:

### Benefits of GitHub Installation:
- **Version Pinning**: No automatic updates without your explicit consent
- **Source Verification**: Review complete source code before installation
- **Supply Chain Security**: Avoid potential NPM package compromise
- **Offline Operation**: No runtime dependencies on package registries
- **Audit Trail**: Know exactly which version you're running

### GitHub Installation:
```bash
# Download and verify source
curl -L -o swipl-mcp-server.tar.gz https://github.com/vpursuit/swipl-mcp-server/archive/refs/heads/main.tar.gz
tar -xzf swipl-mcp-server.tar.gz
cd swipl-mcp-server-main
# Note: Remember this directory path for Claude Desktop configuration

# Review source code before building
less README.md SECURITY.md src/

# Build and install
npm install --production
npm run build
```

**Recommended for:**
- Production environments requiring stability
- High-security environments
- Auditing and compliance requirements
- Air-gapped or restricted networks

## Configuration
- `MCP_LOG_LEVEL`: `debug|info|warn|error|silent` (default `warn`).
- `DEBUG`: include `swipl-mcp-server` to enable debug logging.
- `SWI_MCP_PROLOG_PATH`: override path to `prolog_server.pl` if not in src folder.
- Security is always enabled and cannot be disabled.

## Security Testing
- Verify hybrid security rejects dangerous goals:
  - Example: `call(true)` should return `error(unsafe_goal(...))`
  - Example: `assert(malicious)` should return `error(unsafe_goal(...))`
  - Example: `system('rm -rf /')` should return `error(unsafe_goal(...))`
- Verify safe operations work:
  - Example: `X is 2 + 3` should succeed
  - Example: `append([1,2], [3], L)` should succeed
- Verify recursive user predicates work:
  - Example: `assert((ancestor(X,Y) :- parent(X,Y)))` should succeed
- Confirm consult rejects directives (e.g., `:- initialization(...)`).
- Run comprehensive test suite: `npm test` (includes end-to-end integration testing).
