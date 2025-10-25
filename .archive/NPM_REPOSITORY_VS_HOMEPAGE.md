# NPM: Repository vs Homepage in Monorepo

**Date**: 2025-10-25
**Context**: Understanding npmjs.org package metadata for monorepo packages

---

## The Distinction on npmjs.org

When you view a package on npmjs.org, you see two separate links:

### 🔗 Repository
- **Purpose**: Where the source code lives
- **Label**: "Repository" or "GitHub"
- **What it does**: Links to the GitHub repository
- **Typical value**: `https://github.com/vpursuit/swipl-mcp-server`

### 🏠 Homepage
- **Purpose**: Where documentation/website lives
- **Label**: "Homepage"
- **What it does**: Links to project documentation or website
- **Typical value**: Can be GitHub README, dedicated docs site, or project website

---

## Why This Matters for Monorepos

In a monorepo, you have **one repository** but **multiple packages**. Each package needs:

1. **Repository link** - Points to the monorepo (same for all packages)
2. **Homepage link** - Can be different for each package

### Example from Your Setup

**Monorepo Repository:**
```
https://github.com/vpursuit/swipl-mcp-server
```

**Package-Specific Homepages:**
- `@vpursuit/swipl-mcp-server` → Main product docs
- `@vpursuit/mcp-prolog` → Prolog plugin docs
- `@vpursuit/mcp-core` → Core library API docs
- `@vpursuit/mcp-roots` → Roots feature docs

---

## Current Configuration

### ✅ Root package.json (workspace)
```json
{
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server#readme"
}
```

### ✅ packages/mcp-prolog/package.json
```json
{
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-prolog"  // ← Important!
  },
  // ❌ NO HOMEPAGE FIELD
}
```

### ✅ packages/swipl-mcp-server/package.json
```json
{
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/swipl-mcp-server"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server#readme"
}
```

### ❌ packages/mcp-core/package.json
```json
{
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-core"
  },
  // ❌ NO HOMEPAGE FIELD
}
```

---

## What the "directory" Field Does

The `"directory"` field in repository config is **critical** for monorepos:

```json
{
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-prolog"  // ← This!
  }
}
```

### GitHub Integration
When someone clicks "Repository" on npmjs.org, GitHub recognizes the `directory` field and:
- ✅ Shows the package-specific folder (not root)
- ✅ Displays the package README (if it exists)
- ✅ GitHub file browser starts in `packages/mcp-prolog/`
- ✅ "View on GitHub" button works correctly

**Without `directory` field:**
- ❌ Links to repo root
- ❌ User has to navigate to find package
- ❌ Confusing experience

---

## Homepage Field Options

You have **three strategies** for homepage:

### Option 1: Point to Package Directory (GitHub)

**Strategy**: Each package homepage points to its specific folder in the monorepo.

```json
{
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/mcp-prolog#readme"
}
```

**Pros:**
- ✅ Direct link to package folder
- ✅ Shows package README
- ✅ Users see package-specific docs
- ✅ No separate site needed

**Cons:**
- ❌ Long URL
- ❌ Still shows GitHub UI (not dedicated docs site)
- ❌ README must be comprehensive

**Best for**: Simple packages with good README documentation

---

### Option 2: Point to Root README with Anchor

**Strategy**: All packages link to root README with package-specific anchor.

```json
{
  "homepage": "https://github.com/vpursuit/swipl-mcp-server#mcp-prolog"
}
```

**Requires**: Root README has sections for each package with IDs:

```markdown
## Packages

### @vpursuit/mcp-prolog {#mcp-prolog}

Prolog integration plugin for MCP servers...
```

**Pros:**
- ✅ Centralized documentation
- ✅ Single source of truth
- ✅ Easy to maintain

**Cons:**
- ❌ Root README becomes very long
- ❌ Mixed audience (product users + library users)
- ❌ Less focused per package

**Best for**: Small number of packages (2-3) with shared context

---

### Option 3: Dedicated Documentation Site

**Strategy**: Host documentation on a separate site, with package-specific pages.

```json
{
  "homepage": "https://vpursuit.github.io/swipl-mcp-server/packages/mcp-prolog"
}
```

**Requires**: GitHub Pages, Docusaurus, VitePress, or similar docs site.

**Pros:**
- ✅ Professional appearance
- ✅ Better navigation
- ✅ Search functionality
- ✅ Versioned docs
- ✅ API documentation tools

**Cons:**
- ❌ Requires setup and maintenance
- ❌ Another deployment to manage
- ❌ Overhead for small projects

**Best for**: Large projects with many users, multiple packages, complex APIs

---

### Option 4: No Homepage (Default to Repo)

**Strategy**: Omit `homepage` field entirely.

```json
{
  // No homepage field
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-prolog"
  }
}
```

**Behavior**: npm.js will use repository URL as homepage.

**Pros:**
- ✅ Simple, no duplication
- ✅ Leverages `directory` field
- ✅ Less maintenance

**Cons:**
- ❌ No dedicated "Homepage" link on npmjs.org
- ❌ Users must use "Repository" link for everything

**Best for**: When repository = homepage (GitHub-centric documentation)

---

## Recommended Configuration for Your Monorepo

### Current Situation
- 4-5 packages in monorepo
- All related to MCP ecosystem
- GitHub is primary documentation location
- No dedicated docs site (yet)

### **Recommended: Option 1 (Package Directory Links)**

Each package should have:

```json
{
  "name": "@vpursuit/mcp-prolog",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-prolog"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/mcp-prolog#readme"
}
```

**Why this works:**
- Users see package-specific docs immediately
- Each package README is the homepage
- Clear separation between packages
- Works with current setup (no infrastructure change)
- Scales as you add more packages

---

## Complete Configuration by Package

### packages/mcp-core/package.json
```json
{
  "name": "@vpursuit/mcp-core",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-core"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/mcp-core#readme",
  "bugs": {
    "url": "https://github.com/vpursuit/swipl-mcp-server/issues"
  }
}
```

### packages/mcp-prolog/package.json
```json
{
  "name": "@vpursuit/mcp-prolog",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-prolog"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/mcp-prolog#readme",
  "bugs": {
    "url": "https://github.com/vpursuit/swipl-mcp-server/issues"
  }
}
```

### packages/mcp-roots/package.json
```json
{
  "name": "@vpursuit/mcp-roots",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-roots"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/mcp-roots#readme",
  "bugs": {
    "url": "https://github.com/vpursuit/swipl-mcp-server/issues"
  }
}
```

### packages/swipl-mcp-server/package.json
```json
{
  "name": "@vpursuit/swipl-mcp-server",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/swipl-mcp-server"
  },
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/swipl-mcp-server#readme",
  "bugs": {
    "url": "https://github.com/vpursuit/swipl-mcp-server/issues"
  }
}
```

---

## What Users See on npmjs.org

### After Correct Configuration

When someone visits `https://npmjs.com/package/@vpursuit/mcp-prolog`:

**Sidebar Shows:**
```
📦 Repository
  → https://github.com/vpursuit/swipl-mcp-server
    (with directory=packages/mcp-prolog)
    Clicking opens: github.com/.../tree/main/packages/mcp-prolog

🏠 Homepage
  → https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/mcp-prolog#readme
    Clicking opens: Package-specific README

🐛 Report Issue
  → https://github.com/vpursuit/swipl-mcp-server/issues
```

### User Journey

1. **Discovery**: User finds `@vpursuit/mcp-prolog` on npmjs.com
2. **Documentation**: Clicks "Homepage" → Sees mcp-prolog README
3. **Source Code**: Clicks "Repository" → GitHub shows packages/mcp-prolog folder
4. **Issue**: Clicks "Report Issue" → GitHub issues (all packages share)

**Clear and intuitive!**

---

## Alternative: Shared Homepage with Root README

If you prefer all packages point to the same homepage:

### All packages use:
```json
{
  "homepage": "https://github.com/vpursuit/swipl-mcp-server#readme"
}
```

### Root README structure:
```markdown
# MCP Ecosystem by vpursuit

This monorepo contains multiple MCP packages...

## 📦 Packages

### [@vpursuit/swipl-mcp-server](packages/swipl-mcp-server)
Complete MCP server with Prolog integration.
- Installation: `npx @vpursuit/swipl-mcp-server`
- [Full Documentation →](packages/swipl-mcp-server/README.md)

### [@vpursuit/mcp-prolog](packages/mcp-prolog)
Prolog integration plugin for MCP servers.
- Installation: `npm install @vpursuit/mcp-prolog`
- [Full Documentation →](packages/mcp-prolog/README.md)

### [@vpursuit/mcp-core](packages/mcp-core)
Plugin system for MCP servers.
- Installation: `npm install @vpursuit/mcp-core`
- [Full Documentation →](packages/mcp-core/README.md)

### [@vpursuit/mcp-roots](packages/mcp-roots)
Filesystem roots management for MCP.
- Installation: `npm install @vpursuit/mcp-roots`
- [Full Documentation →](packages/mcp-roots/README.md)
```

**Pros:**
- ✅ Single landing page shows all packages
- ✅ Users discover related packages
- ✅ Ecosystem overview

**Cons:**
- ❌ Not package-specific
- ❌ Extra click to get to package docs

---

## The "bugs" Field

All packages should share the same issues URL:

```json
{
  "bugs": {
    "url": "https://github.com/vpursuit/swipl-mcp-server/issues"
  }
}
```

**Why:**
- Issues for all packages go to same GitHub Issues
- Use labels to triage: `pkg:mcp-prolog`, `pkg:mcp-core`, etc.
- Centralized issue management
- Users can see related issues across packages

---

## Implementation Checklist

**For each package:**
- [ ] ✅ `repository.url` - Points to monorepo
- [ ] ✅ `repository.directory` - Points to package folder
- [ ] ✅ `homepage` - Points to package docs (or root)
- [ ] ✅ `bugs.url` - Points to shared GitHub issues
- [ ] ✅ Package-specific README.md exists
- [ ] ✅ README.md is comprehensive (install, usage, API)

**For root:**
- [ ] ✅ Root README explains monorepo structure
- [ ] ✅ Root README links to all packages
- [ ] ✅ GitHub topics added (mcp, monorepo, prolog, etc.)

---

## Testing Configuration

### 1. Check package.json fields
```bash
for pkg in packages/*/package.json; do
  echo "=== $(dirname $pkg) ==="
  jq '{name, repository, homepage, bugs}' "$pkg"
done
```

### 2. Test npm pack output
```bash
npm pack packages/mcp-prolog --dry-run | grep -E "(homepage|repository)"
```

### 3. Publish to NPM (or test registry)
After publishing, visit npmjs.com and verify:
- Repository link works and shows correct folder
- Homepage link works and shows correct docs
- Report Issue link works

---

## Future: Dedicated Documentation Site

When your project grows, consider migrating to a dedicated docs site:

### GitHub Pages + VitePress
```
https://vpursuit.github.io/swipl-mcp-server/
├── /                          # Main landing page
├── /guide/getting-started     # Installation guide
├── /packages/
│   ├── /mcp-prolog           # Package-specific docs
│   ├── /mcp-core             # Package-specific docs
│   └── /swipl-mcp-server     # Product docs
└── /api/                      # API reference
```

**Then update all homepage fields:**
```json
{
  "homepage": "https://vpursuit.github.io/swipl-mcp-server/packages/mcp-prolog"
}
```

**Tools to consider:**
- VitePress (Vue-based, excellent for APIs)
- Docusaurus (React-based, popular)
- MkDocs (Python, simple)
- Nextra (Next.js-based)

---

## Summary

### Current Missing Configuration
- ❌ `packages/mcp-prolog` - No homepage field
- ❌ `packages/mcp-core` - No homepage field
- ❌ `packages/mcp-roots` - No homepage field (probably)

### Recommended Immediate Fix

Add to each package:
```json
{
  "homepage": "https://github.com/vpursuit/swipl-mcp-server/tree/main/packages/PACKAGE-NAME#readme",
  "bugs": {
    "url": "https://github.com/vpursuit/swipl-mcp-server/issues"
  }
}
```

### What This Achieves
✅ Clear separation on npmjs.org between Repository and Homepage
✅ Users land on package-specific docs
✅ GitHub integration works correctly via `directory` field
✅ All packages share issue tracker
✅ Professional package presentation

### Effort: 5 minutes
Just add 2 fields to each package.json!

---

**END OF GUIDE**
