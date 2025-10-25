# Publishing Guide

This document explains how to publish packages from this monorepo to npm.

## Overview

This monorepo contains 4 independently versioned npm packages:

| Package | Description | Current Version |
|---------|-------------|----------------|
| `@vpursuit/swipl-mcp-server` | Main MCP server (orchestrator) | 3.0.0 |
| `@vpursuit/mcp-prolog` | SWI-Prolog integration plugin | 3.0.0 |
| `@vpursuit/mcp-core` | Plugin system for MCP servers | 1.0.0 |
| `@vpursuit/mcp-roots` | Filesystem root discovery | 1.0.0 |

Each package can be published independently using automated GitHub Actions workflows.

## Prerequisites

- Write access to the GitHub repository
- npm publishing rights for `@vpursuit` scope (handled via OIDC trusted publishing)
- Packages must pass all tests before publishing

## Publishing Methods

### Method 1: Tag-Based Publishing (Recommended)

This is the primary method for production releases. Push a git tag and GitHub Actions automatically publishes the corresponding package.

#### Tag Format

Each package has its own tag format:

```bash
# Main product (swipl-mcp-server)
v3.0.0          # Stable release
v3.0.1-beta.1   # Pre-release

# Library packages
mcp-prolog-v3.0.1
mcp-core-v1.0.1
mcp-roots-v1.0.1
```

#### Publishing Steps

**1. Update Package Version**

Edit the `package.json` in the package you want to publish:

```bash
# Example: Publishing swipl-mcp-server
cd packages/swipl-mcp-server
# Edit package.json: "version": "3.0.1"
```

**2. Commit Changes**

```bash
git add packages/swipl-mcp-server/package.json
git commit -m "chore: bump swipl-mcp-server to v3.0.1"
```

**3. Create and Push Tag**

```bash
# For main product
git tag v3.0.1
git push origin v3.0.1

# For library packages
git tag mcp-prolog-v3.0.2
git push origin mcp-prolog-v3.0.2
```

**4. Monitor GitHub Actions**

- Go to [GitHub Actions](https://github.com/vpursuit/swipl-mcp-server/actions)
- Watch "Publish to NPM" workflow
- Workflow runs:
  1. Security audit
  2. Build all packages
  3. Run tests
  4. Publish to npm (if version doesn't exist)
  5. Create GitHub release

**5. Verify Publication**

Check npm registry:
```bash
npm view @vpursuit/swipl-mcp-server
npm view @vpursuit/mcp-prolog
npm view @vpursuit/mcp-core
npm view @vpursuit/mcp-roots
```

### Method 2: Manual Workflow Dispatch

Use this for testing or publishing without creating a git tag.

#### Steps

**1. Go to GitHub Actions**

- Navigate to: https://github.com/vpursuit/swipl-mcp-server/actions
- Click "Publish to NPM" workflow
- Click "Run workflow" button

**2. Configure Workflow**

- **Branch**: Select `main` (or your branch)
- **Package**: Choose which package to publish
  - `all` - Publish all 4 packages
  - `swipl-mcp-server` - Main product only
  - `mcp-prolog` - Prolog plugin only
  - `mcp-core` - Core plugin system only
  - `mcp-roots` - Roots plugin only
- **Dry run**:
  - `true` - Test without publishing (shows what would be published)
  - `false` - Actually publish to npm

**3. Run Workflow**

Click "Run workflow" to start the process.

**4. Review Results**

- Dry run mode shows package contents without publishing
- Live mode publishes to npm and creates GitHub releases

## Versioning Strategy

### Semantic Versioning

All packages follow [semantic versioning](https://semver.org/):

- **MAJOR** (X.0.0): Breaking changes
- **MINOR** (1.X.0): New features (backward compatible)
- **PATCH** (1.0.X): Bug fixes (backward compatible)

### Version Independence

Each package maintains its own version number:

```
@vpursuit/swipl-mcp-server: 3.0.0  (orchestrator, high-level)
@vpursuit/mcp-prolog:       3.0.0  (implementation, matches server)
@vpursuit/mcp-core:         1.0.0  (stable plugin API)
@vpursuit/mcp-roots:        1.0.0  (stable filesystem API)
```

### When to Bump Versions

**swipl-mcp-server (Main Product):**
- Bump for new features, breaking changes, or significant updates
- Usually changes with mcp-prolog (they're tightly coupled)

**mcp-prolog:**
- Bump when Prolog interface changes
- Usually matches swipl-mcp-server version

**mcp-core:**
- Bump only when plugin API changes
- Rare - this is a stable foundation

**mcp-roots:**
- Bump when filesystem security or root discovery changes
- Rare - this is a stable utility

## Publishing Checklist

Before publishing any package:

- [ ] All tests pass locally (`npm test`)
- [ ] Version bumped in `package.json`
- [ ] CHANGELOG updated (if exists)
- [ ] Breaking changes documented
- [ ] Dependencies up to date
- [ ] Security audit clean (`npm audit`)
- [ ] README accurate
- [ ] SECURITY.md reflects current security model

## Troubleshooting

### "Version already exists on NPM"

**Cause:** The version in `package.json` already published to npm.

**Solution:**
```bash
# Check what's published
npm view @vpursuit/PACKAGE_NAME versions

# Bump version in package.json
# packages/PACKAGE_NAME/package.json
# "version": "3.0.2"  (increment)

# Create new tag
git tag v3.0.2
git push origin v3.0.2
```

### Workflow Fails at Security Audit

**Cause:** High/critical vulnerabilities found.

**Solution:**
```bash
# Review vulnerabilities
npm audit

# Update dependencies
npm update

# Run security fix
npm run security:update

# Test everything still works
npm test
```

### Workflow Fails at Tests

**Cause:** Tests don't pass on CI environment.

**Solution:**
```bash
# Run tests locally
npm test

# Check SWI-Prolog availability (for mcp-prolog tests)
swipl --version

# Fix tests, commit, and retry
```

### OIDC Publishing Error (ENEEDAUTH)

**Cause:** npm authentication issue with OIDC trusted publishing.

**Solution:** This is handled automatically by the workflow:
- Workflow upgrades to npm@11
- Uses `--provenance` flag
- No manual tokens needed

If still fails, check:
- Repository settings → Secrets → Actions
- npm organization settings for `@vpursuit` scope

### Package Too Large

**Warning:** npm has size limits for packages.

**Current sizes:**
- swipl-mcp-server: 164.9 KB ✅
- mcp-prolog: 64.1 KB ✅
- mcp-core: 9.5 KB ✅
- mcp-roots: 16.2 KB ✅

All well under limits.

**To check size:**
```bash
cd packages/PACKAGE_NAME
npm pack --dry-run
```

## Testing Before Publishing

### Dry Run (Recommended)

Test the entire publishing workflow without actually publishing:

**Via GitHub Actions:**
1. Go to Actions → Publish to NPM → Run workflow
2. Set "Dry run" to `true`
3. Select package
4. Review output

**Locally:**
```bash
# Check what files will be published
cd packages/PACKAGE_NAME
npm pack --dry-run

# Create actual tarball for inspection
npm pack
tar -tzf *.tgz
rm *.tgz
```

### Local npm link Testing

Test the package locally before publishing:

```bash
# In package directory
cd packages/swipl-mcp-server
npm link

# In another project
npm link @vpursuit/swipl-mcp-server
npx @vpursuit/swipl-mcp-server
```

## Advanced: Publishing All Packages

If you need to publish all 4 packages at once (rare):

**Via GitHub Actions:**
1. Go to Actions → Publish to NPM → Run workflow
2. Select package: `all`
3. Dry run: `false`
4. Run workflow

**Manually:**
```bash
# Bump all versions in respective package.json files

# Create tags for each
git tag v3.0.1
git tag mcp-prolog-v3.0.1
git tag mcp-core-v1.0.1
git tag mcp-roots-v1.0.1

# Push all tags
git push origin --tags
```

This triggers 4 separate workflow runs (one per tag).

## Rollback / Unpublishing

**Warning:** npm strongly discourages unpublishing packages.

If you must unpublish (within 72 hours):
```bash
npm unpublish @vpursuit/PACKAGE_NAME@VERSION
```

**Better approach:** Publish a new patch version with fixes:
```bash
# Example: v3.0.1 has bug, publish v3.0.2 with fix
git tag v3.0.2
git push origin v3.0.2
```

## Security

### OIDC Trusted Publishing

This repository uses npm's [OIDC trusted publishing](https://docs.npmjs.com/trusted-publishers):

- ✅ No long-lived npm tokens stored in GitHub
- ✅ GitHub Actions authenticates via OpenID Connect
- ✅ Provenance attestation for supply chain security
- ✅ Automatic token rotation

### Package Provenance

All published packages include provenance attestation:
- Build environment details
- Source repository link
- Commit SHA verification
- GitHub Actions workflow transparency

Verify provenance:
```bash
npm view @vpursuit/swipl-mcp-server --json | jq .dist.attestations
```

## Resources

- [npm CLI Documentation](https://docs.npmjs.com/cli)
- [npm Workspaces](https://docs.npmjs.com/cli/v10/using-npm/workspaces)
- [Semantic Versioning](https://semver.org/)
- [npm Trusted Publishing](https://docs.npmjs.com/trusted-publishers)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)

## Quick Reference

```bash
# Check current versions
npm view @vpursuit/swipl-mcp-server version
npm view @vpursuit/mcp-prolog version
npm view @vpursuit/mcp-core version
npm view @vpursuit/mcp-roots version

# Publish main product
git tag v3.0.X
git push origin v3.0.X

# Publish library package
git tag mcp-PACKAGE-v3.0.X
git push origin mcp-PACKAGE-v3.0.X

# Test package contents
cd packages/PACKAGE_NAME
npm pack --dry-run
```
