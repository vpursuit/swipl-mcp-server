# Deployment Quick Reference

Quick reference for deploying new releases of `@vpursuit/swipl-mcp-server`.

For complete publishing documentation including GitHub Actions setup, OIDC trusted publishing, and troubleshooting, see [PUBLISHING.md](../../../PUBLISHING.md).

## Quick Commands

```bash
# Bug fixes (3.0.5 → 3.0.6)
npm run release:patch

# New features (3.0.6 → 3.1.0)
npm run release:minor

# Breaking changes (3.0.6 → 4.0.0)
npm run release:major
```

## What Happens Automatically

1. Version bumped in `package.json`
2. Git commit created with version number
3. Git tag created (e.g., `v3.0.6`)
4. Pushed to GitHub with `--follow-tags`
5. GitHub Actions triggered by tag push
6. Tests run (security audit, build, integration)
7. NPM publish (if version doesn't exist)
8. GitHub Release created with install instructions

## Pre-Release Checklist

- [ ] All changes committed and pushed
- [ ] On `main` branch
- [ ] Tests passing (`npm test`)
- [ ] Build successful (`npm run build`)
- [ ] No uncommitted changes (`git status` clean)

## Checking Release Status

1. **GitHub Actions**: https://github.com/vpursuit/model-context-lab/actions
2. **NPM Package**: https://www.npmjs.com/package/@vpursuit/swipl-mcp-server
3. **GitHub Releases**: https://github.com/vpursuit/model-context-lab/releases

## Version Guidelines

Follow [Semantic Versioning](https://semver.org/):

- **PATCH** (3.0.5 → 3.0.6): Bug fixes, no API changes
- **MINOR** (3.0.6 → 3.1.0): New features, backward compatible
- **MAJOR** (3.0.6 → 4.0.0): Breaking changes

## See Also

- [PUBLISHING.md](../../../PUBLISHING.md) - Complete publishing guide including:
  - Tag-based vs script-based publishing
  - OIDC trusted publishing setup
  - Pre-release versions (alpha, beta, rc)
  - Troubleshooting failed publications
  - Package provenance and security
  - Manual publishing procedures
