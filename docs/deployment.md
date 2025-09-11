# Deployment & Release Guide

## Quick Release (Automated)

For patch releases, use the automated script:

```bash
npm run release:patch
```

**Note**: This requires 2FA code when publishing to NPM. If it fails, complete manually below.

## Manual Release Process

### 1. Version Bump
```bash
# Choose one:
npm version patch    # 1.0.15 → 1.0.16
npm version minor    # 1.0.15 → 1.1.0  
npm version major    # 1.0.15 → 2.0.0
```

### 2. Push Changes
```bash
git push origin main
git push --tags
```

### 3. Publish to NPM
```bash
npm run publish:npm
```

**If 2FA required:**
```bash
cd dist && npm publish --otp=YOUR_6_DIGIT_CODE
```

### 4. Create GitHub Release
```bash
gh release create v$(node -p "require('./package.json').version") \
  --title "v$(node -p "require('./package.json').version")" \
  --generate-notes
```

## Test Before Release

```bash
# Test package build
npm run publish:dry-run

# Run all tests
npm test

# Test security
npm run security:audit
```

## Build from Source (Development)

```bash
git clone https://github.com/vpursuit/swipl-mcp-server.git
cd swipl-mcp-server
npm install
npm test
npm run build
node build/index.js
```

