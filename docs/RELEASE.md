# Release Process

Quick reference guide for creating new releases.

## Quick Commands

```bash
# For bug fixes (2.0.5 → 2.0.6)
npm run release:patch

# For new features (2.0.6 → 2.1.0)
npm run release:minor

# For breaking changes (2.0.6 → 3.0.0)
npm run release:major
```

## What Happens Automatically

When you run a release command:

1. ✅ **Version bumped** in `package.json`
2. ✅ **Git commit created** with version number as message
3. ✅ **Git tag created** (e.g., `v2.0.6`)
4. ✅ **Pushed to GitHub** with `--follow-tags`
5. ✅ **GitHub Actions triggered** by tag push
6. ✅ **Tests run** (security audit, unit, integration)
7. ✅ **NPM publish** happens automatically (if version doesn't exist)
8. ✅ **GitHub Release created** automatically with install instructions

## Pre-Release Checklist

Before running `release:*`:

- [ ] All changes committed and pushed
- [ ] On `main` branch
- [ ] All tests passing locally (`npm test`)
- [ ] Build successful (`npm run build`)
- [ ] No uncommitted changes (`git status` clean)

## Checking Release Status

After pushing the tag, check:

1. **GitHub Actions**: https://github.com/vpursuit/swipl-mcp-server/actions
   - Verify the workflow ran successfully
   - Check all jobs (security-audit, build-and-test, publish)

2. **NPM Package**: https://www.npmjs.com/package/@vpursuit/swipl-mcp-server
   - Verify new version appears

3. **GitHub Releases**: https://github.com/vpursuit/swipl-mcp-server/releases
   - Release should be created automatically

## Troubleshooting

### Release Not Created on GitHub

If GitHub Actions didn't create the release:

```bash
# Check if tag exists
git tag -l

# Check if tag was pushed
git ls-remote --tags origin

# Manually create release using gh CLI
gh release create v2.0.6 \
  --title "Release v2.0.6" \
  --notes "See CHANGELOG for details"
```

### Version Already Exists on NPM

The workflow will skip publishing if the version already exists. Check GitHub Actions logs for:
```
Version 2.0.6 already exists on NPM
version_exists=true
```

If this happens, you need to bump the version again:
```bash
npm run release:patch  # Creates 2.0.7
```

### Workflow Failed

1. Check the error in GitHub Actions
2. Fix the issue locally
3. Create a new patch release with the fix
4. The workflow will run again

## Manual Process (If Scripts Don't Work)

```bash
# Bump version manually
npm version patch  # or minor/major

# Push with tags
git push origin main --follow-tags
```

## Version Number Guidelines

Follow [Semantic Versioning](https://semver.org/):

- **PATCH** (2.0.5 → 2.0.6): Bug fixes, no API changes
- **MINOR** (2.0.6 → 2.1.0): New features, backward compatible
- **MAJOR** (2.0.6 → 3.0.0): Breaking changes

## Related Files

- `package.json` - Version number and release scripts
- `.github/workflows/npm-publish.yml` - Automated release workflow
- `scripts/build-package.js` - Package build script
