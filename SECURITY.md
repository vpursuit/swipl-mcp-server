# Security Policy

This monorepo contains multiple Model Context Protocol (MCP) packages with varying security considerations. Each package may have different security requirements based on its functionality.

## Package-Specific Security Policies

For detailed security information specific to each package, please refer to the individual security documentation:

### Products
- **[@vpursuit/swipl-mcp-server](./products/swipl-mcp-server/SECURITY.md)** - Comprehensive security policy for Prolog execution, sandboxing, file path restrictions, and dangerous predicate blocking

### Libraries
- **[@vpursuit/mcp-server-prolog](./plugins/server/prolog/)** - Prolog integration plugin security considerations
- **[@vpursuit/mcp-server-core](./plugins/server/core/)** - Plugin system security considerations
- **[@vpursuit/mcp-server-roots](./plugins/server/roots/)** - Filesystem access control and path validation security

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

### Response Timeline
- **Acknowledgment**: Within 48 hours
- **Initial Assessment**: Within 7 days
- **Fix Timeline**: Varies by severity (critical: immediate, high: 1-2 weeks, medium: 2-4 weeks, low: next release)

## Supported Versions

| Package | Version | Supported |
|---------|---------|-----------|
| @vpursuit/swipl-mcp-server | 3.x | ✅ Yes |
| @vpursuit/mcp-server-prolog | 3.x | ✅ Yes |
| @vpursuit/mcp-server-core | 1.x | ✅ Yes |
| @vpursuit/mcp-server-roots | 1.x | ✅ Yes |

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

## NPM Publishing Security

All packages in this monorepo use **OIDC (OpenID Connect) Trusted Publishing** for NPM package distribution, representing the most secure authentication method currently available.

### Security Benefits
- ✅ **No Long-lived Tokens**: No NPM authentication tokens stored in GitHub secrets
- ✅ **Temporary Credentials**: Short-lived tokens that automatically expire
- ✅ **Scoped Access**: Tokens limited to specific packages and organizations
- ✅ **Provenance Enabled**: Cryptographic proof of build environment included in all packages
- ✅ **Audit Trail**: Full audit log of all publishing activities

### Why This Matters
Recent security incidents in the NPM ecosystem have highlighted vulnerabilities in traditional token-based authentication. Our OIDC approach eliminates these risks by:
- Making token theft ineffective (tokens expire within minutes)
- Requiring authenticated GitHub Actions workflow execution
- Preventing unauthorized package publishing

Publishing is exclusively handled through our [GitHub Actions workflow](./.github/workflows/npm-publish.yml) with automatic security validation.

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
cat products/swipl-mcp-server/SECURITY.md

# Install and build
npm install
npm run build

# Built packages are in packages/*/dist/
```

**Benefits of GitHub Installation:**
- ✅ Version pinning (no automatic updates)
- ✅ Source code verification before installation
- ✅ Supply chain security (avoid potential NPM compromise)
- ✅ Offline operation capability
- ✅ Complete audit trail

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
