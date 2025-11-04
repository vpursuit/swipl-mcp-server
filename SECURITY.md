# Security Policy

This monorepo contains multiple Model Context Protocol (MCP) packages with varying security considerations. Each package may have different security requirements based on its functionality.

## Package-Specific Security Policies

### Products
- **@vpursuit/swipl-mcp-server** - See [SWI-Prolog MCP Server Security](#swi-prolog-mcp-server-security) section below for comprehensive security policy

### Plugin Libraries (For Developers)
For plugin implementation details, see:
- **[@vpursuit/mcp-server-prolog](./plugins/server/prolog/SECURITY.md)** - Prolog integration plugin implementation security
- **[@vpursuit/mcp-server-core](./plugins/server/core/SECURITY.md)** - Plugin system implementation security
- **[@vpursuit/mcp-server-roots](./plugins/server/roots/SECURITY.md)** - Filesystem access control implementation security

## Reporting a Vulnerability

We take security vulnerabilities seriously. If you discover a security issue in any package within this monorepo:

### Preferred Method: Private Disclosure
1. **GitHub Security Advisories** (Recommended): Use [GitHub's private vulnerability reporting](https://github.com/vpursuit/model-context-lab/security/advisories/new) to report security issues privately
2. **Issue Tracker**: If you cannot use Security Advisories, open an issue and mark it as security-related (we will move discussion to a private channel)

### What to Include
Please provide the following information in your report:
- **Affected package(s)** and version(s)
- **Environment details** (Node.js version, SWI-Prolog version if applicable, OS)
- **Minimal reproduction steps** (Proof of Concept)
- **Expected vs. actual behavior**
- **Impact assessment** (what can an attacker achieve?)
- **Relevant logs** (please redact secrets, absolute paths, and PII)

## Supported Versions

| Package | Version | Supported |
|---------|---------|-----------|
| @vpursuit/swipl-mcp-server | 3.x | Yes |
| @vpursuit/mcp-server-prolog | 3.x | Yes |
| @vpursuit/mcp-server-core | 1.x | Yes |
| @vpursuit/mcp-server-roots | 1.x | Yes |

- **Main branch**: Actively maintained with latest security fixes
- **Older versions**: Security patches may be backported for critical vulnerabilities
- **Runtime requirements**: Node.js ≥ 20.0.0, SWI-Prolog ≥ 8.4.0 (for Prolog packages)

## General Security Considerations

### All Packages
- Keep dependencies up to date
- Review release notes for security updates
- Use officially published versions from npm
- Enable GitHub Dependabot alerts

### MCP Servers (Products)
- Run as non-privileged users in production
- Implement network isolation where appropriate
- Monitor resource usage (CPU, memory, file handles)
- Set conservative logging levels (`warn` or `silent` in production)
- Review any code before execution, especially from untrusted sources

### Plugin Libraries
- Validate all inputs from untrusted sources
- Implement proper error handling
- Avoid exposing internal implementation details in error messages
- Follow principle of least privilege

## SWI-Prolog MCP Server Security

The `@vpursuit/swipl-mcp-server` package provides Prolog execution capabilities and implements comprehensive security measures to protect against malicious code execution.

### Important Warning

**Use this tool at your own risk.** This server executes user-provided Prolog queries and code. While comprehensive security measures are implemented including sandboxing and blacklisting, **no security system is perfect**.

**All usage is at your own risk.** Review and understand any Prolog code before execution, especially from untrusted sources.

### Threat Model

**Primary Risks:**
- Untrusted Prolog content provided via file consultation and query execution
- Potential for arbitrary file/OS access attempts
- Long-running or looping queries
- Resource exhaustion (CPU, memory)
- Information leakage through logs

**Trust Boundaries:**
- MCP clients are partially trusted (provide queries)
- Prolog files are untrusted (must be validated)
- SWI-Prolog process is sandboxed and isolated

### Built-in Security Features

#### File Path Security
- **Secure by Default**: File operations disabled without explicit root configuration
- **Configuration Required**: Configure roots via MCP client or `SWI_MCP_ALLOWED_ROOTS` environment variable
- **Environment Variable**: `SWI_MCP_ALLOWED_ROOTS=/path/one,/path/two` (comma-separated absolute paths)
- **System Directory Blocking**: Automatically blocks access to `/etc`, `/usr`, `/bin`, `/var`, `/sys`, `/proc`, `/boot`, `/dev`, `/root`
- **Pre-validation**: File paths are strictly validated against configured roots before any Prolog interaction
- **Clear Error Messages**: Helpful messages guide users to configure roots when file access is attempted without configuration

#### Dangerous Predicate Detection
- **Pre-execution Blocking**: Dangerous operations caught before execution, not during timeout
- **Blocked Predicates**: `shell()`, `system()`, `call()`, `assert()`, `halt()`, `retract()`, `abolish()`
- **Pattern Detection**: Scans fact/query content for dangerous predicate calls
- **Clear Error Messages**: `Security Error: Operation blocked - contains dangerous predicate 'X'`

**Example:**
```prolog
% This is blocked before execution
knowledge_base_assert({ "fact": "malware :- shell('rm -rf /')" })
% Returns: Security Error: Operation blocked - contains dangerous predicate 'shell'
```

#### Additional Protections
- **Enhanced Security Model**: Combines `library(sandbox)` validation with explicit blacklist and path restrictions
  - `library(sandbox)` validates most built-ins as safe/unsafe
  - Additional blacklist prevents dangerous operations even if sandbox allows them
  - User-defined predicates in `knowledge_base` module are allowed for recursive definitions
- **Safe consult**: Only facts/rules are accepted; directives and module-altering terms are rejected
- **Isolation**: User data lives in `knowledge_base` module; `unknown=fail` to avoid accidental calls
- **Timeouts**: Node side enforces query timeouts to prevent hangs
- **Logging hygiene**: Logs go to `stderr`, default to `warn`, and redact absolute paths/PIDs

### Hardening Checklist for Prolog Server
- Run as a non-privileged user; avoid filesystem write access beyond the working directory
- Keep Node.js, SWI-Prolog, and dependencies up to date
- Configure roots to limit file access to specific directories only
- Set conservative logging in production: `MCP_LOG_LEVEL=warn` or `silent`
- Do not ingest untrusted `.pl` files from unknown sources without review
- Monitor resource usage; consider external CPU/time limits if high-risk
- Set conservative query timeout limits via `SWI_MCP_QUERY_TIMEOUT_MS`

### Security Testing

**File Path Security Testing:**
```bash
# Verify system directory blocking
knowledge_base_load({ "filename": "/etc/passwd" })
# Expected: Security Error: Access to system directories is blocked

# Verify configured root works (after setting up roots)
knowledge_base_load({ "filename": "/your/configured/root/test.pl" })
# Expected: Success (if file exists and root is configured)
```

**Dangerous Predicate Testing:**
```bash
# Verify pre-execution blocking
knowledge_base_assert({ "fact": "malware :- shell('rm -rf /')" })
# Expected: Security Error: Operation blocked - contains dangerous predicate 'shell'

# Verify safe operations work
query_start({ "query": "X is 2 + 3" })
# Expected: Success with X = 5
```

**Configuration:**
- `MCP_LOG_LEVEL`: `debug|info|warn|error|silent` (default `warn`)
- `DEBUG`: include `swipl-mcp-server` to enable debug logging
- `SWI_MCP_PROLOG_PATH`: override path to the Prolog server script
- Security is always enabled and cannot be disabled

## NPM Publishing Security

All packages in this monorepo use **OIDC (OpenID Connect) Trusted Publishing** for NPM package distribution, representing the most secure authentication method currently available.

### Security Benefits
- **No Long-lived Tokens**: No NPM authentication tokens stored in GitHub secrets
- **Temporary Credentials**: Short-lived tokens that automatically expire
- **Scoped Access**: Tokens limited to specific packages and organizations
- **Provenance Enabled**: Cryptographic proof of build environment included in all packages
- **Audit Trail**: Full audit log of all publishing activities

### Why This Matters
Recent security incidents in the NPM ecosystem have highlighted vulnerabilities in traditional token-based authentication. Our OIDC approach eliminates these risks by:
- Making token theft ineffective (tokens expire within minutes)
- Requiring authenticated GitHub Actions workflow execution
- Preventing unauthorized package publishing

Publishing is exclusively handled through our [GitHub Actions workflow](./.github/workflows/npm-publish.yml) with automatic security validation.

### Verifying Package Provenance

All packages published from this repository include **npm provenance attestations** - cryptographic proof linking published packages to source code and build environment.

**How to Verify:**
```bash
npm audit signatures
npm view @vpursuit/swipl-mcp-server --json | jq .dist.attestations
```

**What to Look For:**
- Registry signature verified
- Provenance attestation verified
- Source: github.com/vpursuit/model-context-lab
- Build: GitHub Actions

If verification fails, **do not use the package** and report via [security advisory](#reporting-a-vulnerability).

**Learn More:** [npm Provenance Documentation](https://docs.npmjs.com/generating-provenance-statements)

## Installation Security

### NPM Installation (Standard)
```bash
npm install @vpursuit/swipl-mcp-server
# or
npx @vpursuit/swipl-mcp-server
```

Benefits: Simple, automatic updates, standard workflow

### GitHub Installation (Enhanced Security)
For enhanced security and stability, consider installing from source:

```bash
# Clone the repository
git clone https://github.com/vpursuit/model-context-lab.git
cd model-context-lab

# Optional: checkout a specific release tag
# git checkout swipl-mcp-server@3.0.0

# Review source code before building
cat README.md
cat SECURITY.md

# Install and build
npm install
npm run build

# Built packages are in packages/*/dist/
```

**Benefits of GitHub Installation:**
- Version pinning (no automatic updates)
- Source code verification before installation
- Supply chain security (avoid potential NPM compromise)
- Offline operation capability
- Complete audit trail

**Recommended for:**
- Production environments requiring stability
- High-security environments
- Compliance and auditing requirements
- Air-gapped or restricted networks

## Hardening Best Practices

### For All Packages
1. **Keep Software Updated**: Regularly update Node.js, dependencies, and OS packages
2. **Principle of Least Privilege**: Run with minimum required permissions
3. **Network Isolation**: Isolate MCP servers from untrusted networks when possible
4. **Resource Limits**: Set memory and CPU limits to prevent DoS
5. **Logging**: Configure appropriate log levels and ensure logs don't contain secrets

### For Prolog-Related Packages
6. **File Access**: Restrict file operations to designated directories only
7. **Code Review**: Never execute untrusted Prolog code without review
8. **Timeout Configuration**: Set conservative query timeout limits
9. **Sandboxing**: Leverage built-in security features (enabled by default)

## Security Testing

Each package includes its own test suite. To run security-related tests:

```bash
# Run all tests
npm test

# Run tests for specific package
npm test -w products/swipl-mcp-server
```

See individual package SECURITY.md files for package-specific security test examples.

## Acknowledgments

We appreciate responsible disclosure of security vulnerabilities. Contributors who report valid security issues will be acknowledged in:
- Security advisories
- Release notes (with permission)
- CONTRIBUTORS.md (with permission)

## Resources

- [Model Context Protocol Specification](https://modelcontextprotocol.io)
- [GitHub Security Advisories](https://github.com/vpursuit/model-context-lab/security/advisories)
- [NPM Security Best Practices](https://docs.npmjs.com/security)
- [Node.js Security Best Practices](https://nodejs.org/en/docs/guides/security/)
