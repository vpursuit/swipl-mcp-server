# Security Policy

## ⚠️ Important Warning

**Use this tool at your own risk.** This SWI-Prolog MCP Server provides controlled access to a Prolog environment and executes user-provided queries and code. While comprehensive security measures have been implemented including sandboxing and blacklisting, **no security system is perfect**. 

**Security Layer:** This server includes an enhanced security framework that:
- **Secure by Default**: No file access without explicit root configuration
- **File Path Restrictions**: Only allows file access within explicitly configured roots (MCP client or `SWI_MCP_ALLOWED_ROOTS`)
- **Dangerous Predicate Blocking**: Pre-execution detection and blocking of dangerous operations
- Validates predicates using SWI-Prolog's `library(sandbox)`
- Maintains an explicit blacklist of dangerous operations
- Isolates user code in a dedicated `knowledge_base` module
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

### File Path Security
- **Secure by Default**: File operations disabled without explicit root configuration
- **Configuration Required**: Configure roots via MCP client or `SWI_MCP_ALLOWED_ROOTS` environment variable
- **Environment Variable**: `SWI_MCP_ALLOWED_ROOTS=/path/one,/path/two` (comma-separated absolute paths)
- **System Directory Blocking**: Automatically blocks access to `/etc`, `/usr`, `/bin`, `/var`, `/sys`, `/proc`, `/boot`, `/dev`, `/root`
- **Pre-validation**: File paths are strictly validated against configured roots before any Prolog interaction
- **Clear Error Messages**: Helpful messages guide users to configure roots when file access is attempted without configuration

### Dangerous Predicate Detection
- **Pre-execution Blocking**: Dangerous operations caught before execution, not during timeout
- **Blocked Predicates**: `shell()`, `system()`, `call()`, `assert()`, `halt()`, `retract()`, `abolish()`
- **Pattern Detection**: Scans fact/query content for dangerous predicate calls
- **Clear Error Messages**: `Security Error: Operation blocked - contains dangerous predicate 'X'`

### Additional Protections
- **Enhanced Security Model**: Combines `library(sandbox)` validation with explicit blacklist and path restrictions
  - `library(sandbox)` validates most built-ins as safe/unsafe
  - Additional blacklist prevents dangerous operations even if sandbox allows them
  - User-defined predicates in `knowledge_base` module are allowed for recursive definitions
- **Safe consult**: Only facts/rules are accepted; directives and module‑altering terms are rejected
- **Isolation**: User data lives in `knowledge_base`; `unknown=fail` to avoid accidental calls
- **Timeouts**: Node side enforces query timeouts to prevent hangs
- **Logging hygiene**: Logs go to `stderr`, default to `warn`, and redact absolute paths/PIDs

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
# Clone the repository
git clone https://github.com/vpursuit/model-context-lab.git
cd swipl-mcp-server

# Optional: checkout a specific release tag for stability
# git checkout v2.0.1

# Review source code before building
less README.md SECURITY.md src/

# Build from source
npm install
npm run build

# The built server is now at: dist/index.js
# Note this full path for Claude Desktop configuration
pwd  # Shows your current directory path
```

### Configure Claude Desktop with Local Build:
```json
{
  "mcpServers": {
    "swipl": {
      "command": "node",
      "args": ["/full/path/to/model-context-lab/products/swipl-mcp-server/dist/index.js"]
    }
  }
}
```
Replace `/full/path/to/` with your actual directory path from the `pwd` command above.

**Recommended for:**
- Production environments requiring stability
- High-security environments
- Auditing and compliance requirements
- Air-gapped or restricted networks

## NPM Publishing Security

This project uses **OIDC (OpenID Connect) Trusted Publishing** for NPM package distribution, representing the most secure authentication method currently available for NPM packages.

### Security Benefits:
- **No Long-lived Tokens**: No NPM authentication tokens are stored in GitHub secrets or anywhere else
- **Temporary Credentials**: Publishing uses short-lived, automatically expiring tokens generated by GitHub's OIDC provider
- **Scoped Access**: Tokens are automatically scoped to only this specific package and organization
- **Provenance Enabled**: All published packages include cryptographic proof of their build environment

### Why This Matters:
Recent security incidents in the NPM ecosystem have highlighted vulnerabilities in traditional token-based authentication. Our OIDC approach eliminates these risks by:
- Making token theft ineffective (tokens expire within minutes)
- Requiring authenticated GitHub Actions workflow execution
- Providing full audit trail of all publishing activities

Publishing is exclusively handled through our [GitHub Actions workflow](../../.github/workflows/npm-publish.yml) with automatic security validation.

### Verifying Package Provenance

All packages published from this repository include **npm provenance attestations** - cryptographic proof linking published packages to source code and build environment.

**How to Verify:**
```bash
npm audit signatures
npm view @vpursuit/swipl-mcp-server --json | jq .dist.attestations
```

**What to Look For:**
- ✅ Registry signature verified
- ✅ Provenance attestation verified
- 📦 Source: github.com/vpursuit/model-context-lab
- 🔨 Build: GitHub Actions

If verification fails, **do not use the package** and report via [security advisory](#reporting-a-vulnerability).

**Learn More:** [npm Provenance Documentation](https://docs.npmjs.com/generating-provenance-statements)

## Configuration
- `MCP_LOG_LEVEL`: `debug|info|warn|error|silent` (default `warn`).
- `DEBUG`: include `swipl-mcp-server` to enable debug logging.
- `SWI_MCP_PROLOG_PATH`: override path to the Prolog server script.
- Security is always enabled and cannot be disabled.

## Security Testing

### File Path Security Testing
- Verify system directory blocking:
  - Example: `knowledge_base_load({ filename: "/etc/passwd" })` → `Security Error: Access to system directories is blocked`
  - Example: `knowledge_base_load({ filename: "/usr/bin/ls" })` → `Security Error: Access to system directories is blocked`
- Verify allowed directory works:
  - Example: `knowledge_base_load({ filename: "~/.model-context-lab/test.pl" })` → should work (if file exists)

### Dangerous Predicate Testing
- Verify pre-execution blocking:
  - Example: `knowledge_base_assert({ fact: "malware :- shell('rm -rf /')" })` → `Security Error: Operation blocked - contains dangerous predicate 'shell'`
  - Example: `knowledge_base_assert({ fact: "bad :- system('cat /etc/passwd')" })` → `Security Error: Operation blocked - contains dangerous predicate 'system'`
- Verify safe operations work:
  - Example: `X is 2 + 3` should succeed
  - Example: `append([1,2], [3], L)` should succeed
- Verify recursive user predicates work:
  - Example: `knowledge_base_assert({ fact: "ancestor(X,Y) :- parent(X,Y)" })` should succeed

### Legacy Security Testing
- Verify sandbox rejects remaining dangerous goals during execution:
  - Example: `call(true)` should return `error(unsafe_goal(...))`
  - Example: Direct `assert(malicious)` in query should return `error(unsafe_goal(...))`
- Confirm consult rejects directives (e.g., `:- initialization(...)`).
- Run comprehensive test suite: `npm test` (includes end-to-end integration testing).
