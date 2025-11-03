# Publishing Guide

This document explains how to publish packages from this monorepo to npm.

## Overview

This monorepo uses a **products vs plugins architecture**:

### Publishable Products (Public npm packages)

| Package | Description | Current Version | Location |
|---------|-------------|----------------|----------|
| `@vpursuit/swipl-mcp-server` | Main MCP server product | 3.0.0 | `products/swipl-mcp-server/` |

### Internal Plugins (Private, not published)

| Package | Description | Current Version | Location |
|---------|-------------|----------------|----------|
| `@vpursuit/mcp-server-prolog` | SWI-Prolog integration (private) | 3.0.0 | `plugins/server/prolog/` |
| `@vpursuit/mcp-server-core` | Plugin system (private) | 1.0.0 | `plugins/server/core/` |
| `@vpursuit/mcp-server-roots` | Filesystem root discovery (private) | 1.0.0 | `plugins/server/roots/` |

**Note:** Only products are published to npm. Plugins are internal dependencies bundled with the products.

## Prerequisites

- Write access to the GitHub repository
- npm publishing rights for `@vpursuit` scope (handled via OIDC trusted publishing)
- Packages must pass all tests before publishing

## Publishing Methods

### Method 1: Tag-Based Publishing (Recommended)

This is the primary method for production releases. Push a git tag and GitHub Actions automatically publishes the corresponding package.

#### Tag Format

Products use simple version tags:

```bash
# Server product (swipl-mcp-server)
v3.0.0          # Stable release
v3.0.1-beta.1   # Pre-release

# Future client product
client-v1.0.0   # Client product release
```

**Note:** Plugins are not published independently, so they don't have their own tags.

#### Publishing Steps

**1. Update Package Version**

Edit the `package.json` in the product you want to publish:

```bash
# Example: Publishing swipl-mcp-server
cd products/swipl-mcp-server
# Edit package.json: "version": "3.0.1"
```

**2. Commit Changes**

```bash
git add products/swipl-mcp-server/package.json
git commit -m "chore: bump swipl-mcp-server to v3.0.1"
```

**3. Create and Push Tag**

```bash
# For server product
git tag v3.0.1
git push origin v3.0.1

# For future client product
git tag client-v1.0.0
git push origin client-v1.0.0
```

**4. Monitor GitHub Actions**

- Go to [GitHub Actions](https://github.com/vpursuit/model-context-lab/actions)
- Watch "Publish to NPM" workflow
- Workflow runs:
  1. Upgrade npm to v11 (for workspace protocol support)
  2. Security audit
  3. Build all packages
  4. Run tests
  5. Publish to npm (if version doesn't exist)
  6. Create GitHub release

**5. Verify Publication**

Check npm registry:
```bash
npm view @vpursuit/swipl-mcp-server

# Verify plugins are NOT published (should return 404)
npm view @vpursuit/mcp-server-prolog  # Should fail - private package
```

### Method 1b: Automated Release Scripts (Recommended)

For convenience, automated npm scripts are provided that handle version bumping, tag creation, and pushing in one command.

#### Available Scripts

```bash
# Stable releases
npm run release:patch    # Bump patch version (3.0.0 → 3.0.1)
npm run release:minor    # Bump minor version (3.0.0 → 3.1.0)
npm run release:major    # Bump major version (3.0.0 → 4.0.0)

# Pre-releases
npm run release:prerelease  # Bump prerelease version (3.0.0 → 3.0.1-beta.0)
```

#### What These Scripts Do

Each script automatically:
1. Updates version in `products/swipl-mcp-server/package.json`
2. Creates a git commit with the version change
3. **Creates an annotated git tag** (e.g., `v3.0.1-beta.0`)
4. Pushes the commit to `main`
5. Pushes all tags to trigger the publish workflow

#### Example Usage

```bash
# Release a beta version
npm run release:prerelease

# Output:
# ✓ Version bumped to 3.0.1-beta.0
# ✓ Commit created
# ✓ Tag v3.0.1-beta.0 created
# ✓ Pushed to main
# ✓ Tag pushed (workflow triggered)
```

**Important:** These scripts use `--git-tag-version=true` to ensure tags are always created, and `git push origin --tags` to ensure tags are pushed (avoiding the `--follow-tags` limitation).

### Method 2: Manual Workflow Dispatch

Use this for testing or publishing without creating a git tag.

#### Steps

**1. Go to GitHub Actions**

- Navigate to: https://github.com/vpursuit/model-context-lab/actions
- Click "Publish to NPM" workflow
- Click "Run workflow" button

**2. Configure Workflow**

- **Branch**: Select `main` (or your branch)
- **Package**: Choose which product to publish
  - `swipl-mcp-server` - Main server product (default)
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

### Versioning for Products and Plugins

**Products** (published to npm):
```
@vpursuit/swipl-mcp-server: 3.0.0
```

**Plugins** (private, not published):
```
@vpursuit/mcp-server-prolog: 3.0.0  (internal)
@vpursuit/mcp-server-core:   1.0.0  (internal)
@vpursuit/mcp-server-roots:  1.0.0  (internal)
```

### When to Bump Versions

**swipl-mcp-server (Product):**
- Bump for new features, breaking changes, or significant updates
- This is the version users see and install

**Internal Plugins:**
- Bump plugin versions when their APIs change
- Plugin versions are internal only (not published)
- Users don't interact with plugin versions directly

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
# products/PACKAGE_NAME/package.json
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

### Workspace Protocol Error (EUNSUPPORTEDPROTOCOL)

**Cause:** npm doesn't support `workspace:*` protocol (pnpm/Yarn syntax).

**Error Message:**
```
npm error code EUNSUPPORTEDPROTOCOL
npm error Unsupported URL Type "workspace:": workspace:*
```

**Solution:**
```bash
# Use standard npm workspace syntax (*) instead of workspace:*
# In package.json devDependencies:
"@vpursuit/mcp-server-core": "*"  # ✅ Correct
"@vpursuit/mcp-server-core": "workspace:*"  # ❌ Wrong

# After fixing, regenerate lockfile
npm install
```

**Note:** This has been fixed in the repository. If you encounter this, ensure you're on the latest main branch.

### Workflow Didn't Trigger After Version Bump

**Cause:** Tag was not created or not pushed to remote repository.

**Symptoms:**
- You ran `npm run release:*` but workflow didn't start
- Commit was pushed but no GitHub Actions run appears
- `git tag -l` shows tag locally but `git ls-remote --tags origin` doesn't

**Common Reasons:**
1. **Using `--follow-tags` with lightweight tags**: The `--follow-tags` flag only pushes annotated tags, but `npm version` creates lightweight tags by default
2. **Tag not created**: `npm version` might not create tags in some configurations
3. **Push failed silently**: Network issues or permissions problems

**Solution:**

**If using old scripts (before this fix):**
```bash
# Check if tag exists locally
git tag -l "v*"

# If tag exists locally, push it
git push origin v3.0.1-beta.0

# If tag doesn't exist, create and push it
git tag -a v3.0.1-beta.0 -m "Release v3.0.1-beta.0"
git push origin v3.0.1-beta.0
```

**If using current scripts:**
The issue should be fixed. The scripts now:
- Use `--git-tag-version=true` to ensure tag creation
- Use `git push origin --tags` instead of `--follow-tags`

**Verify tag was pushed:**
```bash
# Check remote tags
git ls-remote --tags origin | grep v3.0.1-beta.0

# Should show:
# abc123... refs/tags/v3.0.1-beta.0
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

**Current size (published product):**
- @vpursuit/swipl-mcp-server: 343.2 KB (unpacked: 1.3 MB) ✅

**Note:** Plugins are bundled into the main product and are not published separately.

Well under npm limits (unpublished packages have 10 MB limit).

**To check size:**
```bash
cd products/PACKAGE_NAME
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
cd products/swipl-mcp-server
npm pack --dry-run

# Create actual tarball for inspection
npm pack
tar -tzf *.tgz
rm *.tgz
```

### Local npm link Testing

Test the package locally before publishing:

```bash
# In product directory
cd products/swipl-mcp-server
npm link

# In another project
npm link @vpursuit/swipl-mcp-server
npx @vpursuit/swipl-mcp-server
```

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
# Check current product version
npm view @vpursuit/swipl-mcp-server version

# Verify plugins are not published (should fail with 404)
npm view @vpursuit/mcp-server-prolog version  # Should return 404

# Automated releases (recommended)
npm run release:prerelease  # Beta release
npm run release:patch       # Patch release
npm run release:minor       # Minor release
npm run release:major       # Major release

# Manual tag-based publish (server product)
git tag v3.0.X
git push origin v3.0.X

# Manual tag-based publish (future client product)
git tag client-v1.0.X
git push origin client-v1.0.X

# Test package contents
cd products/swipl-mcp-server
npm pack --dry-run
```
