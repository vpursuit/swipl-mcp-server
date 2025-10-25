# Monorepo Organization Strategy: Deep Analysis

**Date**: 2025-10-25
**Context**: vpursuit/swipl-mcp-server is now a monorepo with multiple packages, but GitHub repo name suggests single product

---

## Current Situation

### GitHub Repository
- **Name**: `vpursuit/swipl-mcp-server`
- **Current State**: Contains 5 packages (1 product + 4 libraries)
- **Already Published**: `swipl-mcp-server` is known product on NPM
- **Future**: "More products later"

### Current Packages
```
packages/
├── swipl-mcp-server/    # Main product (CLI orchestrator)
├── mcp-core/            # Plugin system library
├── mcp-prolog/          # Prolog integration plugin
├── mcp-roots/           # Filesystem roots plugin
└── mcp-sampling/        # (New component)
```

### NPM Publishing
- **Scope**: `@vpursuit`
- **Published**: `@vpursuit/swipl-mcp-server`
- **Potentially publishable**: All other packages as libraries

---

## Strategic Question

**Should you:**
1. Keep monorepo in `vpursuit/swipl-mcp-server`?
2. Split libraries to new `vpursuit/mcp-building-blocks` repo?
3. Something else entirely?

---

## Architecture Options Analysis

### Option 1: Keep Current Monorepo (Status Quo)

**Structure:**
```
vpursuit/swipl-mcp-server (GitHub)
├── packages/
│   ├── swipl-mcp-server/    → @vpursuit/swipl-mcp-server
│   ├── mcp-core/            → @vpursuit/mcp-core
│   ├── mcp-prolog/          → @vpursuit/mcp-prolog
│   ├── mcp-roots/           → @vpursuit/mcp-roots
│   ├── mcp-sampling/        → @vpursuit/mcp-sampling
│   └── future-product/      → @vpursuit/future-product
```

**Publishing Strategy:**
- Each package published independently to NPM under `@vpursuit`
- Users: `npm install @vpursuit/swipl-mcp-server`
- Users: `npm install @vpursuit/mcp-core` (if building their own)

**Pros:**
- ✅ **Zero migration cost** - already set up
- ✅ **Single dev environment** - one `npm install`, one test run
- ✅ **Atomic cross-package changes** - refactor core + product in one commit
- ✅ **Shared tooling** - ESLint, TypeScript, Prettier, Vitest configs
- ✅ **Faster development** - no need to publish core to test product changes
- ✅ **Version coordination** - Changesets/Lerna can manage dependent releases
- ✅ **Single CI/CD pipeline** - one workflow, selective publishing
- ✅ **Established precedent** - Babel, React, NestJS all use this pattern
- ✅ **GitHub stars/watchers stay** - no loss of visibility
- ✅ **Easy contributor onboarding** - clone once, see everything

**Cons:**
- ❌ **Misleading repo name** - "swipl-mcp-server" doesn't describe full scope
- ❌ **Future product confusion** - Product B users wonder why they're in swipl-mcp-server repo
- ❌ **Mixed issues** - GitHub issues cover all products, harder to triage
- ❌ **Large clone size** - contributors get everything even if working on one package
- ❌ **SEO confusion** - Searching for Product B might not find it easily
- ❌ **Branding dilution** - Each product competes for attention in same repo

**Mitigations:**
- Use comprehensive README explaining monorepo structure
- Tag issues with package labels
- Use GitHub wiki for product-specific docs
- Clear package READMEs (what users see on NPM)

---

### Option 2: Rename Monorepo to Neutral Name

**Structure:**
```
vpursuit/mcp-toolkit (GitHub) ← RENAMED
├── packages/
│   ├── swipl-mcp-server/    → @vpursuit/swipl-mcp-server
│   ├── mcp-core/            → @vpursuit/mcp-core
│   ├── mcp-prolog/          → @vpursuit/mcp-prolog
│   ├── other-product/       → @vpursuit/other-product
```

**Possible Names:**
- `vpursuit/mcp-toolkit`
- `vpursuit/mcp-workspace`
- `vpursuit/mcp-ecosystem`
- `vpursuit/mcp-suite`

**Pros:**
- ✅ All pros of Option 1
- ✅ **Honest naming** - repo name reflects actual scope
- ✅ **Future-proof** - can add products without confusion
- ✅ **GitHub redirects** - old `swipl-mcp-server` URLs auto-redirect

**Cons:**
- ❌ All cons of Option 1 (except misleading name)
- ❌ **Loss of brand recognition** - "swipl-mcp-server" has existing visibility
- ❌ **SEO reset** - Search results for "swipl-mcp-server github" might break
- ❌ **User confusion** - existing users wonder where the repo went
- ❌ **Documentation updates** - all external links need updating

**Mitigations:**
- GitHub auto-redirects old URLs
- Clear announcement in README
- Update all package.json repository fields
- Redirect notice in old bookmarks/stars

---

### Option 3: Split Core Libraries to Separate Repo

**Structure:**
```
vpursuit/mcp-core (GitHub) ← NEW REPO
├── packages/
│   ├── mcp-core/       → @vpursuit/mcp-core
│   ├── mcp-prolog/     → @vpursuit/mcp-prolog
│   ├── mcp-roots/      → @vpursuit/mcp-roots
│   └── mcp-sampling/   → @vpursuit/mcp-sampling

vpursuit/swipl-mcp-server (GitHub) ← KEEP, SLIM DOWN
├── (just the orchestrator product)
├── depends on: @vpursuit/mcp-core, @vpursuit/mcp-prolog
```

**Pros:**
- ✅ **Clear separation** - libraries vs products
- ✅ **Accurate naming** - each repo name matches its purpose
- ✅ **Independent versioning** - core can be v2.0 while product is v3.0
- ✅ **Smaller clones** - work on product without library code
- ✅ **Different audiences** - library consumers vs product users
- ✅ **Focused issues** - core bugs separate from product bugs
- ✅ **Reusability** - other products can use core without swipl-mcp-server context

**Cons:**
- ❌ **Complex development** - changes spanning core + product require two PRs
- ❌ **Publish coordination** - must publish core first, then product
- ❌ **Version drift** - product might lag behind core versions
- ❌ **Duplicate CI/CD** - two workflows to maintain
- ❌ **Testing complexity** - integration tests harder to run locally
- ❌ **Migration cost** - significant work to split and reorganize
- ❌ **Link management** - need to use npm link for local development

**Mitigations:**
- Use npm workspaces or Lerna for local development
- Automated cross-repo testing in CI
- Dependabot to keep product deps updated

---

### Option 4: Product-Per-Repo (Full Split)

**Structure:**
```
vpursuit/mcp-core         → @vpursuit/mcp-core (library)
vpursuit/mcp-prolog       → @vpursuit/mcp-prolog (library)
vpursuit/swipl-mcp-server → @vpursuit/swipl-mcp-server (product)
vpursuit/other-product    → @vpursuit/other-product (product)
```

**Pros:**
- ✅ **Maximum clarity** - one repo = one package
- ✅ **Independent everything** - versions, issues, stars, releases
- ✅ **Smallest clones** - only what you need
- ✅ **Clear ownership** - each repo can have different maintainers
- ✅ **Marketing clarity** - each product has dedicated space

**Cons:**
- ❌ **Extreme fragmentation** - 6+ repos to manage
- ❌ **Duplicate infrastructure** - CI, docs, tooling in every repo
- ❌ **Cross-cutting changes nightmare** - update across all repos manually
- ❌ **Version coordination hell** - ensure compatibility manually
- ❌ **Contributor friction** - multiple repos to clone, set up, test
- ❌ **Highest migration cost** - massive reorganization

**Verdict:** Only for very mature projects with large teams

---

### Option 5: Hybrid - Core Monorepo + Product Repos

**Structure:**
```
vpursuit/mcp-toolkit (GitHub)
├── packages/
│   ├── mcp-core/       → @vpursuit/mcp-core
│   ├── mcp-prolog/     → @vpursuit/mcp-prolog
│   ├── mcp-roots/      → @vpursuit/mcp-roots
│   └── mcp-sampling/   → @vpursuit/mcp-sampling

vpursuit/swipl-mcp-server (GitHub)
└── (product code only, depends on toolkit packages)

vpursuit/other-product (GitHub)
└── (another product, depends on toolkit packages)
```

**Pros:**
- ✅ **Library monorepo benefits** - easy to develop shared components
- ✅ **Product independence** - each product has its own brand/repo
- ✅ **Clear architecture** - libraries separate from applications
- ✅ **Scalable** - add products without cluttering core repo

**Cons:**
- ❌ **Split development** - product changes requiring core changes = two PRs
- ❌ **Two CI/CD systems** - toolkit + each product
- ❌ **Version lag** - products might not use latest toolkit

---

## Industry Precedents

### Successful Monorepos
- **Babel** (`babel/babel`): All packages in one repo, published separately
- **React** (`facebook/react`): React + DevTools + related in one repo
- **NestJS** (`nestjs/nest`): Framework + official plugins in one repo
- **Turborepo** (`vercel/turbo`): Multiple tools in one monorepo

### Successful Multi-Repo
- **Vue 3** (`vuejs/core`, `vuejs/router`, `vuejs/pinia`): Split after maturity
- **Kubernetes**: Many repos, coordinated releases
- **Webpack**: Split core from loaders/plugins

**Pattern:** Monorepo works best when packages are **tightly coupled**. Multi-repo when packages are **loosely coupled**.

---

## Decision Framework

### Key Questions

**Q1: Will future products share significant code with current packages?**
- **Yes** → Monorepo
- **No** → Separate repos

**Q2: Do you expect frequent cross-package refactoring?**
- **Yes** → Monorepo
- **No** → Separate repos

**Q3: Are the products targeting the same audience?**
- **Yes** → Monorepo acceptable
- **No** → Separate repos better for marketing

**Q4: How many maintainers/contributors?**
- **1-3 people** → Monorepo easier
- **5+ people** → Multiple repos for ownership clarity

**Q5: How important is individual product branding?**
- **Not critical** → Monorepo fine
- **Very important** → Separate product repos

---

## Recommended Strategy

Based on your situation:
- Small team (you)
- MCP ecosystem (related domain)
- Shared code (mcp-core used by all)
- Future products likely MCP-related

### **Recommendation: Option 1 with Enhancements**

**KEEP the monorepo** in `vpursuit/swipl-mcp-server`, but make it clear and intentional:

#### Implementation

**1. Update Root README.md**
```markdown
# MCP Ecosystem by vpursuit

This repository contains multiple Model Context Protocol packages and products.

## 🚀 Products

### SWI-Prolog MCP Server
The main MCP server with Prolog knowledge base integration.

📦 NPM: `npm install @vpursuit/swipl-mcp-server`
📖 Docs: [packages/swipl-mcp-server](packages/swipl-mcp-server)

### (Future products listed here)

## 🧱 Reusable Components

| Package | Description | NPM |
|---------|-------------|-----|
| `@vpursuit/mcp-core` | Plugin system for MCP servers | [npm](https://npmjs.com/package/@vpursuit/mcp-core) |
| `@vpursuit/mcp-prolog` | Prolog integration plugin | [npm](https://npmjs.com/package/@vpursuit/mcp-prolog) |
| `@vpursuit/mcp-roots` | Filesystem roots management | [npm](https://npmjs.com/package/@vpursuit/mcp-roots) |

## 📂 Repository Structure

This is a monorepo managed with npm workspaces. Each package can be developed and published independently.
```

**2. Each Package Has Standalone README**
Every `packages/*/README.md` should be complete:
- Installation instructions
- Usage examples
- API documentation
- License
- Links back to main repo

This is what users see on NPM!

**3. GitHub Topics**
Add topics to repo:
- `mcp`
- `model-context-protocol`
- `prolog`
- `monorepo`
- `plugin-system`

**4. Package-Based Versioning**
Use independent versioning for each package:
- `swipl-mcp-server@3.0.0`
- `mcp-core@1.2.0`
- `mcp-prolog@2.5.0`

**NOT** monorepo-wide versions like `v3.0.0`

**5. GitHub Releases Per Package**
Tag format: `<package-name>@<version>`
- `swipl-mcp-server@3.0.1`
- `mcp-core@1.3.0`

Each release describes only that package's changes.

**6. Issue Labels**
Create package-specific labels:
- `pkg:swipl-mcp-server`
- `pkg:mcp-core`
- `pkg:mcp-prolog`
- `type:bug`, `type:feature`, etc.

**7. GitHub Actions for Selective Publishing**
Use Changesets or Lerna to publish only changed packages:

```yaml
name: Publish Packages

on:
  push:
    tags:
      - '*@*'  # Matches swipl-mcp-server@3.0.0

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3

      # Extract package name from tag
      - name: Parse tag
        id: tag
        run: |
          TAG=${GITHUB_REF#refs/tags/}
          PACKAGE=${TAG%@*}
          VERSION=${TAG#*@}
          echo "package=$PACKAGE" >> $GITHUB_OUTPUT
          echo "version=$VERSION" >> $GITHUB_OUTPUT

      # Build and publish specific package
      - run: npm install
      - run: npm run build
      - run: npm publish packages/${{ steps.tag.outputs.package }} --access public
        env:
          NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
```

---

## When to Reconsider and Split

**Move to Option 3 (split core library) if:**
- You have 3+ products that are domain-different (e.g., not all MCP-related)
- External contributors want to use core without product context
- Core library has different release cadence than products
- Products have different maintainers

**Move to Option 5 (hybrid) if:**
- Products become large enough to warrant separate repos
- Marketing requires distinct product identities
- Different products target different user bases

**Signs you should split:**
- Repo has >50k lines of code
- >10 packages
- >5 distinct products
- Different teams per product
- Conflicting dependency requirements

---

## Migration Path (If You Later Split)

If you decide to split in the future:

**Phase 1: Extract Core**
1. Create `vpursuit/mcp-toolkit` repo
2. Move `mcp-core`, `mcp-prolog`, `mcp-roots`, `mcp-sampling` there
3. Publish from new repo
4. Update `swipl-mcp-server` to depend on published packages

**Phase 2: Product Extraction**
1. Keep `swipl-mcp-server` as-is (main product)
2. Future products get their own repos
3. Each depends on toolkit packages

This can be done incrementally without breaking existing users.

---

## NPM Publishing Best Practices (Regardless of Choice)

### 1. Package.json Repository Field
Each package should have correct repository info:

```json
{
  "name": "@vpursuit/mcp-prolog",
  "repository": {
    "type": "git",
    "url": "https://github.com/vpursuit/swipl-mcp-server.git",
    "directory": "packages/mcp-prolog"  // ← Important!
  }
}
```

The `directory` field makes GitHub "View on GitHub" button work correctly.

### 2. Independent Versioning
Use SemVer independently per package:
- Breaking change in `mcp-core`? → `1.0.0` → `2.0.0`
- Patch in `swipl-mcp-server`? → `3.0.0` → `3.0.1`

### 3. Changelog Per Package
Maintain `CHANGELOG.md` in each package directory, not just root.

### 4. Publish Configuration
In each `packages/*/package.json`:

```json
{
  "publishConfig": {
    "access": "public"  // For scoped packages
  },
  "files": [
    "build",
    "README.md",
    "LICENSE"
  ]
}
```

---

## Tooling Recommendations

### Version Management
**Option A: Changesets** (Recommended)
```bash
npm install -D @changesets/cli
npx changeset init
```

Workflow:
1. Developer runs `npx changeset` to document changes
2. CI publishes on merge to main
3. Handles dependent version bumps automatically

**Option B: Lerna**
```bash
npm install -D lerna
npx lerna init
```

Older but proven, more opinionated.

### Workspace Scripts
Already using npm workspaces ✓

```json
{
  "scripts": {
    "build": "npm run build --workspaces",
    "test": "vitest run",
    "publish:all": "changeset publish"
  }
}
```

---

## Final Recommendation Summary

### For vpursuit/swipl-mcp-server:

**DO:**
- ✅ Keep as monorepo with current name
- ✅ Make README extremely clear about structure
- ✅ Use package-based tagging/releases
- ✅ Publish packages independently to NPM
- ✅ Use Changesets for version management
- ✅ Each package README is standalone
- ✅ Use GitHub labels for package filtering

**DON'T:**
- ❌ Rename repo (loses brand recognition)
- ❌ Split now (premature optimization)
- ❌ Use monorepo-wide versioning
- ❌ Mix package issues without labels

### Re-evaluate When:
- You have 3+ major products
- Products serve different markets
- External contributors struggle with monorepo
- Team grows beyond 5 people
- Repo exceeds 100k LOC

---

## Example: How Users See It

### On NPM:
```
@vpursuit/swipl-mcp-server
  → Standalone README
  → Clear installation instructions
  → Links to GitHub repo (with directory param → correct folder)

@vpursuit/mcp-core
  → Standalone README
  → API documentation
  → Links to GitHub repo
```

Users **don't care** about monorepo structure. They see individual packages.

### On GitHub:
```
vpursuit/swipl-mcp-server
  README: "MCP Ecosystem - Multiple products and libraries"

  Releases:
    - swipl-mcp-server@3.0.1
    - mcp-core@1.5.0
    - mcp-prolog@2.3.0

  Issues tagged with pkg:* labels

  Directory structure clear in README
```

---

## Conclusion

**The monorepo is serving you well.** Don't split prematurely.

**Key Insight:** Users consume your **NPM packages**, not your **GitHub repository structure**. As long as each package on NPM is clear and usable, the repo organization is an implementation detail.

**Next Steps:**
1. Enhance README to document monorepo structure
2. Set up Changesets for version management
3. Configure package-based GitHub releases
4. Add package labels to issues
5. Ensure each package README is standalone
6. Update GitHub Actions for selective publishing

**Review this decision** when you add your 3rd major product.
