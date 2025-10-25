# Monorepo Migration Plan: Extract MCP Roots to Reusable Package

**Created**: 2025-10-25
**Status**: PLANNING
**Approach**: Option B - Dual Package Publishing
**Branch**: `feature/monorepo-migration`
**Estimated Duration**: Multiple sessions over several days

---

## 📋 Table of Contents
1. [Overview](#overview)
2. [Goals & Strategy](#goals--strategy)
3. [Target Architecture](#target-architecture)
4. [Implementation Phases](#implementation-phases)
5. [Progress Tracking](#progress-tracking)
6. [Critical Considerations](#critical-considerations)
7. [Rollback Plan](#rollback-plan)
8. [Testing Checklist](#testing-checklist)
9. [Post-Migration Tasks](#post-migration-tasks)

---

## Overview

### What We're Doing
Converting the swipl-mcp-server repository into an npm workspaces monorepo containing:
1. **`@vpursuit/mcp-roots`** - NEW reusable MCP roots management library (v1.0.0)
2. **`@vpursuit/swipl-mcp-server`** - EXISTING Prolog server (v2.0.7)

### Why
- Make roots functionality reusable by other MCP servers in the community
- Enable independent versioning and publishing of both packages
- Cleaner separation of concerns
- Better testing isolation
- Future-proof architecture for additional packages

### How
Using npm workspaces with a single feature branch, implemented in 12 distinct phases.

---

## Goals & Strategy

### Primary Goals
✅ Extract roots functionality to `@vpursuit/mcp-roots` package
✅ Maintain 100% backward compatibility for end users
✅ Enable independent publishing of both packages
✅ Update CI/CD for dual package workflow
✅ Preserve all git history
✅ Keep all 98+ tests passing

### Non-Goals
❌ Change end-user installation process
❌ Break existing MCP client configurations
❌ Modify core Prolog functionality

### Strategy
- **Single feature branch**: `feature/monorepo-migration`
- **Safety first**: Tag main as `pre-monorepo-v2.0.6` before starting
- **Incremental commits**: Small, logical commits after each phase
- **Test continuously**: Run tests after each major change
- **Document everything**: This file tracks all progress

---

## Target Architecture

### Directory Structure
```
swipl-mcp-server/                     # Root (private workspace)
├── package.json                      # Workspace configuration
├── package-lock.json                 # Unified lockfile
├── tsconfig.base.json               # Shared TypeScript config
├── vitest.workspace.ts              # Vitest workspace
├── README.md                         # Monorepo overview
├── MONOREPO_MIGRATION_PLAN.md       # This file!
├── .github/
│   └── workflows/
│       └── npm-publish.yml          # UPDATED: Dual publishing
├── packages/
│   ├── mcp-roots/                   # NEW PACKAGE
│   │   ├── package.json             # @vpursuit/mcp-roots@1.0.0
│   │   ├── tsconfig.json
│   │   ├── vitest.config.ts
│   │   ├── README.md
│   │   ├── CHANGELOG.md
│   │   ├── LICENSE
│   │   ├── .npmignore
│   │   ├── src/
│   │   │   ├── index.ts            # Public API exports
│   │   │   ├── RootsManager.ts     # From src/utils/roots.ts
│   │   │   ├── types.ts            # Interfaces
│   │   │   └── constants.ts        # From src/constants.ts
│   │   ├── dist/                   # Build output (gitignored)
│   │   └── test/
│   │       ├── unit/               # From test/unit/roots.test.ts
│   │       └── integration/
│   │
│   └── swipl-mcp-server/          # MOVED FROM ROOT
│       ├── package.json            # @vpursuit/swipl-mcp-server@2.0.7
│       ├── tsconfig.json
│       ├── vitest.config.ts
│       ├── README.md
│       ├── SECURITY.md
│       ├── CONTRIBUTING.md
│       ├── src/                    # All current src/ files
│       ├── build/                  # Build output
│       ├── test/                   # All tests except roots
│       ├── docs/
│       ├── images/
│       ├── scripts/
│       └── prolog/
```

### Key Relationships
- `swipl-mcp-server` **depends on** `mcp-roots` via `workspace:*`
- During development: symlink in `node_modules/@vpursuit/mcp-roots/`
- After publishing: normal npm dependency resolution
- Build order: `mcp-roots` FIRST, then `swipl-mcp-server`

---

## Implementation Phases

### Phase 0: Preparation & Safety ⏳ NOT STARTED
**Objective**: Create safety nets and feature branch

**Tasks**:
- [ ] Verify clean working tree (`git status`)
- [ ] Create safety tag: `git tag -a pre-monorepo-v2.0.6 -m "Pre-monorepo migration safety tag"`
- [ ] Push tag: `git push origin pre-monorepo-v2.0.6`
- [ ] Create feature branch: `git checkout -b feature/monorepo-migration`
- [ ] Document current state (commit hash, test count, etc.)

**Validation**:
```bash
git status                           # Should be clean
git log --oneline -1                 # Note commit hash
npm test                             # Count passing tests
```

**Commit**: N/A (no code changes)

---

### Phase 1: Root Workspace Configuration ⏳ NOT STARTED
**Objective**: Create monorepo foundation without breaking existing code

**Tasks**:
- [ ] Create root `package.json` (private, with workspaces: ["packages/*"])
- [ ] Create `tsconfig.base.json` (shared TypeScript config)
- [ ] Create `vitest.workspace.ts` (unified testing)
- [ ] Update root `.gitignore` (add monorepo patterns)
- [ ] Create root `README.md` (monorepo overview)

**Key Files Created**:
- `package.json` (root, private)
- `tsconfig.base.json`
- `vitest.workspace.ts`
- `README.md` (root)

**Validation**:
- [ ] JSON files are valid (no syntax errors)
- [ ] Workspace glob pattern matches intended structure

**Commit**: ✅ "Create root workspace configuration"

**Notes**: _[Add session notes here]_

---

### Phase 2: Create mcp-roots Package Structure ⏳ NOT STARTED
**Objective**: Set up the new mcp-roots package scaffolding

**Tasks**:
- [ ] Create directory: `packages/mcp-roots/src/`
- [ ] Create directory: `packages/mcp-roots/test/unit/`
- [ ] Create directory: `packages/mcp-roots/test/integration/`
- [ ] Create `packages/mcp-roots/package.json`
  - name: `@vpursuit/mcp-roots`
  - version: `1.0.0`
  - main: `dist/index.js`
  - types: `dist/index.d.ts`
  - dependencies: `@modelcontextprotocol/sdk`
- [ ] Create `packages/mcp-roots/tsconfig.json` (extends base)
- [ ] Create `packages/mcp-roots/vitest.config.ts`
- [ ] Create `packages/mcp-roots/README.md` (comprehensive docs)
- [ ] Create `packages/mcp-roots/CHANGELOG.md` (v1.0.0 initial)
- [ ] Copy `LICENSE` to `packages/mcp-roots/`
- [ ] Create `packages/mcp-roots/.npmignore`

**Key Files Created**:
- `packages/mcp-roots/package.json`
- `packages/mcp-roots/tsconfig.json`
- `packages/mcp-roots/vitest.config.ts`
- `packages/mcp-roots/README.md`
- `packages/mcp-roots/CHANGELOG.md`
- `packages/mcp-roots/LICENSE`
- `packages/mcp-roots/.npmignore`

**Validation**:
- [ ] All JSON files valid
- [ ] Directory structure matches plan
- [ ] package.json has correct exports field

**Commit**: ✅ "Initialize packages/mcp-roots structure"

**Notes**: _[Add session notes here]_

---

### Phase 3: Extract Roots Code to mcp-roots ⏳ NOT STARTED
**Objective**: Move roots functionality to the new package

**Tasks**:
- [ ] Copy `src/utils/roots.ts` → `packages/mcp-roots/src/RootsManager.ts`
- [ ] Update imports in RootsManager.ts (remove relative paths)
- [ ] Create `packages/mcp-roots/src/types.ts`
  - Extract `RootDirectory` interface
  - Extract `PathValidationResult` interface
- [ ] Create `packages/mcp-roots/src/constants.ts`
  - Extract `ROOT_CACHE_TTL_MS` from src/constants.ts
  - Extract `DEFAULT_FALLBACK_DIR` from src/constants.ts
  - Extract `BLOCKED_SYSTEM_DIRS` from src/constants.ts
- [ ] Create `packages/mcp-roots/src/index.ts` (public API)
  - Export `RootsManager`, `rootsManager`
  - Export types
  - Export constants
- [ ] Move `test/unit/roots.test.ts` → `packages/mcp-roots/test/unit/`
- [ ] Update test imports to use `@vpursuit/mcp-roots`

**Files Modified**:
- NEW: `packages/mcp-roots/src/RootsManager.ts`
- NEW: `packages/mcp-roots/src/types.ts`
- NEW: `packages/mcp-roots/src/constants.ts`
- NEW: `packages/mcp-roots/src/index.ts`
- MOVED: `packages/mcp-roots/test/unit/roots.test.ts`

**Validation**:
- [ ] All source files have correct imports
- [ ] No TypeScript errors in mcp-roots package
- [ ] Test file compiles (may not run yet)

**Commit**: ✅ "Extract roots code to @vpursuit/mcp-roots package"

**Notes**: _[Add session notes here]_

---

### Phase 4: Move Server to packages/ ⏳ NOT STARTED
**Objective**: Relocate all server code to packages/swipl-mcp-server/

**Tasks**:
- [ ] Create `packages/swipl-mcp-server/` directory
- [ ] Move files to packages/swipl-mcp-server/:
  - [ ] `git mv src packages/swipl-mcp-server/`
  - [ ] `git mv test packages/swipl-mcp-server/`
  - [ ] `git mv docs packages/swipl-mcp-server/`
  - [ ] `git mv images packages/swipl-mcp-server/`
  - [ ] `git mv scripts packages/swipl-mcp-server/`
  - [ ] `git mv build packages/swipl-mcp-server/`
  - [ ] `git mv prolog packages/swipl-mcp-server/`
  - [ ] Move README.md, SECURITY.md, CONTRIBUTING.md, etc.
  - [ ] `git mv package.json packages/swipl-mcp-server/`
  - [ ] `git mv tsconfig.json packages/swipl-mcp-server/`
  - [ ] Move vitest.config.ts, .npmignore, etc.
- [ ] Update `packages/swipl-mcp-server/package.json`:
  - [ ] Add dependency: `"@vpursuit/mcp-roots": "workspace:*"`
  - [ ] Bump version to `2.0.7`
  - [ ] Update repository.directory field
- [ ] Remove duplicate constants from `packages/swipl-mcp-server/src/constants.ts`
  - [ ] Remove `ROOT_CACHE_TTL_MS`
  - [ ] Remove `DEFAULT_FALLBACK_DIR`
  - [ ] Remove `BLOCKED_SYSTEM_DIRS`
- [ ] Delete `packages/swipl-mcp-server/src/utils/roots.ts` (now in mcp-roots)

**Files Modified**:
- MOVED: All server files to `packages/swipl-mcp-server/`
- UPDATED: `packages/swipl-mcp-server/package.json`
- UPDATED: `packages/swipl-mcp-server/src/constants.ts`
- DELETED: `packages/swipl-mcp-server/src/utils/roots.ts`

**Validation**:
- [ ] All files moved successfully
- [ ] No files left at root (except workspace config)
- [ ] package.json has mcp-roots dependency

**Commit**: ✅ "Move server code to packages/swipl-mcp-server"

**Notes**: _[Add session notes here]_

---

### Phase 5: Update Server Imports ⏳ NOT STARTED
**Objective**: Change all server imports to use @vpursuit/mcp-roots

**Tasks**:
- [ ] Update `packages/swipl-mcp-server/src/tools.ts`:
  - [ ] Change `import { rootsManager } from './utils/roots.js'`
  - [ ] To: `import { rootsManager } from '@vpursuit/mcp-roots'`
- [ ] Update `packages/swipl-mcp-server/src/index.ts`:
  - [ ] Add `import { rootsManager } from '@vpursuit/mcp-roots'`
- [ ] Update any constant imports:
  - [ ] Change imports from `./constants.js` to `@vpursuit/mcp-roots` where needed
- [ ] Update `packages/swipl-mcp-server/tsconfig.json`:
  - [ ] Extend `../../tsconfig.base.json`
  - [ ] Set correct outDir and rootDir

**Files Modified**:
- `packages/swipl-mcp-server/src/tools.ts`
- `packages/swipl-mcp-server/src/index.ts`
- `packages/swipl-mcp-server/tsconfig.json`
- Any other files importing from roots or constants

**Validation**:
- [ ] No import errors in TypeScript
- [ ] All imports resolve correctly
- [ ] TypeScript compilation succeeds (may have build errors)

**Commit**: ✅ "Update imports to use @vpursuit/mcp-roots"

**Notes**: _[Add session notes here]_

---

### Phase 6: Install Dependencies & Build ⏳ NOT STARTED
**Objective**: Install workspace dependencies and verify builds work

**Tasks**:
- [ ] Run `npm install` at root
  - This creates symlinks for workspace packages
  - Creates unified package-lock.json
- [ ] Build mcp-roots first:
  - [ ] `npm run build -w packages/mcp-roots`
  - [ ] Verify dist/ directory created
  - [ ] Check for TypeScript errors
- [ ] Build server:
  - [ ] `npm run build -w packages/swipl-mcp-server`
  - [ ] Verify build/ directory created
  - [ ] Check for TypeScript errors
- [ ] Test root build script:
  - [ ] `npm run build` (should build both in order)

**Validation**:
- [ ] `node_modules/@vpursuit/mcp-roots` symlink exists
- [ ] `packages/mcp-roots/dist/` contains compiled JS and .d.ts files
- [ ] `packages/swipl-mcp-server/build/` contains compiled code
- [ ] No TypeScript compilation errors
- [ ] No missing module errors

**Commit**: ✅ "Configure monorepo build system"

**Notes**: _[Add session notes here]_

---

### Phase 7: Update Testing Configuration ⏳ NOT STARTED
**Objective**: Ensure all tests work in monorepo structure

**Tasks**:
- [ ] Verify root `vitest.workspace.ts` is correctly configured
- [ ] Update `packages/mcp-roots/vitest.config.ts` if needed
- [ ] Update `packages/swipl-mcp-server/vitest.config.ts` if needed
- [ ] Run mcp-roots tests:
  - [ ] `npm run test -w packages/mcp-roots`
  - [ ] Verify roots tests pass
- [ ] Run server unit tests:
  - [ ] `npm run test:unit -w packages/swipl-mcp-server`
  - [ ] Verify all unit tests pass
- [ ] Run server integration tests:
  - [ ] `npm run test:integration -w packages/swipl-mcp-server`
  - [ ] Verify integration tests pass
- [ ] Run all tests from root:
  - [ ] `npm test`
  - [ ] Count passing tests (should be 98+)

**Validation**:
- [ ] All mcp-roots tests pass
- [ ] All server tests pass
- [ ] Total test count matches or exceeds original (98+)
- [ ] No test failures
- [ ] No import errors in tests

**Commit**: ✅ "Update vitest configuration for monorepo"

**Notes**: _[Add session notes here]_

---

### Phase 8: Update CI/CD for Dual Publishing ⏳ NOT STARTED
**Objective**: Update GitHub Actions workflow for monorepo publishing

**WARNING**: This is the MOST CRITICAL phase. The CI/CD changes are complex!

**Tasks**:
- [ ] Backup current `.github/workflows/npm-publish.yml`
- [ ] Update workflow triggers:
  - [ ] Keep `v*.*.*` for server tags
  - [ ] Add `roots-v*.*.*` for mcp-roots tags
- [ ] Update `security-audit` job:
  - [ ] Keep mostly as-is (npm ci works at root)
- [ ] Update `build-and-test` job:
  - [ ] Change: `npm run lint` → `npm run lint --workspaces --if-present`
  - [ ] Change: Build all packages with `npm run build`
  - [ ] Change: `npm run test:unit --workspaces --if-present`
  - [ ] Keep: `npm run test:integration -w packages/swipl-mcp-server`
  - [ ] Change: `npm run build:package -w packages/swipl-mcp-server`
  - [ ] Change artifact paths to `packages/*/dist/`
  - [ ] Upload artifacts for BOTH packages
- [ ] Create NEW `publish-mcp-roots` job:
  - [ ] Trigger on `roots-v*.*.*` tags
  - [ ] Check version doesn't exist
  - [ ] Publish from `packages/mcp-roots/`
  - [ ] Create GitHub release for mcp-roots
- [ ] Update `publish-server` job (rename from `publish`):
  - [ ] Depend on `publish-mcp-roots` (needs: [..., publish-mcp-roots])
  - [ ] Use conditional: `always()` with success/skipped check
  - [ ] Update all paths to `packages/swipl-mcp-server/`
  - [ ] Update version check path
  - [ ] Update publish path
- [ ] Add workflow_dispatch inputs:
  - [ ] dry_run (already exists)
  - [ ] package choice (both, mcp-roots, server)

**Files Modified**:
- `.github/workflows/npm-publish.yml` (major rewrite)

**Validation**:
- [ ] YAML syntax is valid
- [ ] All path references updated
- [ ] Job dependencies correct
- [ ] Conditionals work as expected
- [ ] Test with workflow_dispatch dry_run

**Commit**: ✅ "Update CI/CD for monorepo dual package publishing"

**Notes**: _[Add session notes here]_

**TESTING**: Before merging, trigger workflow_dispatch with dry_run=true!

---

### Phase 9: Documentation ⏳ NOT STARTED
**Objective**: Update all documentation for monorepo structure

**Tasks**:
- [ ] Create new root `README.md`:
  - [ ] Explain monorepo structure
  - [ ] Link to both packages
  - [ ] Development setup instructions
  - [ ] Contributing guidelines
- [ ] Update `packages/swipl-mcp-server/README.md`:
  - [ ] Add note about monorepo (for contributors)
  - [ ] Keep installation instructions unchanged
  - [ ] Update development setup section
- [ ] Create comprehensive `packages/mcp-roots/README.md`:
  - [ ] Installation instructions
  - [ ] Quick start guide
  - [ ] API reference
  - [ ] Usage examples
  - [ ] Configuration options
  - [ ] MCP client integration examples
- [ ] Update `CLAUDE.md`:
  - [ ] Add monorepo development guidelines
  - [ ] Update file paths
  - [ ] Add mcp-roots milestone
- [ ] Update `packages/swipl-mcp-server/CONTRIBUTING.md`:
  - [ ] Update for monorepo workflow
  - [ ] Explain workspace commands
  - [ ] Update testing instructions

**Files Modified**:
- NEW: Root `README.md`
- NEW: `packages/mcp-roots/README.md`
- UPDATED: `packages/swipl-mcp-server/README.md`
- UPDATED: `CLAUDE.md`
- UPDATED: `packages/swipl-mcp-server/CONTRIBUTING.md`

**Validation**:
- [ ] All links work
- [ ] Installation instructions are clear
- [ ] Code examples are correct
- [ ] No broken references

**Commit**: ✅ "Update documentation for monorepo structure"

**Notes**: _[Add session notes here]_

---

### Phase 10: Update VSCode Workspace ⏳ NOT STARTED
**Objective**: Configure VSCode for optimal monorepo development

**Tasks**:
- [ ] Update `swipl-mcp-server.code-workspace`:
  - [ ] Add folder for `packages/mcp-roots` (name: "🌳 MCP Roots")
  - [ ] Update folder for `packages/swipl-mcp-server` (name: "🧠 SWI-Prolog Server")
  - [ ] Keep other folders (test, llama, etc.)
  - [ ] Add workspace settings:
    - [ ] `typescript.tsdk: "node_modules/typescript/lib"`
    - [ ] `typescript.preferences.includePackageJsonAutoImports: "on"`
- [ ] Test workspace loads correctly in VSCode
- [ ] Verify TypeScript IntelliSense works across packages

**Files Modified**:
- `swipl-mcp-server.code-workspace`

**Validation**:
- [ ] VSCode opens workspace without errors
- [ ] Both packages visible in sidebar
- [ ] TypeScript autocomplete works
- [ ] Can navigate between packages with Cmd+Click

**Commit**: ✅ "Update VSCode workspace for monorepo"

**Notes**: _[Add session notes here]_

---

### Phase 11: Final Validation ⏳ NOT STARTED
**Objective**: Comprehensive testing before pushing

**Tasks**:
- [ ] Clean install from scratch:
  ```bash
  rm -rf node_modules packages/*/node_modules packages/*/dist packages/swipl-mcp-server/build
  rm package-lock.json
  npm install
  ```
- [ ] Full build:
  ```bash
  npm run build
  ```
- [ ] Full test suite:
  ```bash
  npm test
  npm run test:integration
  ```
- [ ] Integration test with MCP inspector:
  ```bash
  npx @modelcontextprotocol/inspector node packages/swipl-mcp-server/build/index.js
  ```
- [ ] Test specific MCP tools:
  - [ ] `roots_list` tool works
  - [ ] `knowledge_base_load` tool works
  - [ ] Query tools work
- [ ] Dry-run publish both packages:
  ```bash
  npm publish --dry-run -w packages/mcp-roots
  npm publish --dry-run -w packages/swipl-mcp-server
  ```
- [ ] Verify package contents:
  ```bash
  cd packages/mcp-roots && npm pack && tar -tzf *.tgz
  cd packages/swipl-mcp-server/dist && npm pack && tar -tzf *.tgz
  ```

**Validation Checklist**:
- [ ] All builds succeed without errors
- [ ] All 98+ tests pass
- [ ] MCP inspector connects successfully
- [ ] All MCP tools function correctly
- [ ] No TypeScript errors
- [ ] No missing dependencies
- [ ] Package contents look correct
- [ ] No sensitive files in packages

**Commit**: ✅ "Verify all tests and builds pass"

**Notes**: _[Add session notes here]_

---

### Phase 12: Push & Create PR ⏳ NOT STARTED
**Objective**: Share work and prepare for merge

**Tasks**:
- [ ] Review all commits on feature branch:
  ```bash
  git log --oneline main..feature/monorepo-migration
  ```
- [ ] Push feature branch:
  ```bash
  git push -u origin feature/monorepo-migration
  ```
- [ ] Create GitHub Pull Request:
  - [ ] Title: "Refactor: Convert to monorepo with @vpursuit/mcp-roots package"
  - [ ] Description: Include link to this plan, summary of changes, testing performed
  - [ ] Add labels: `refactor`, `breaking-change` (internal only)
- [ ] Monitor CI/CD on GitHub:
  - [ ] security-audit job passes
  - [ ] build-and-test job passes
  - [ ] Artifacts uploaded successfully
- [ ] Review PR diff in GitHub UI:
  - [ ] Check for unintended changes
  - [ ] Verify all file moves tracked correctly
  - [ ] Look for TODO comments or debug code
- [ ] OPTIONAL: Test workflow_dispatch with dry_run=true
- [ ] Merge PR to main (when ready)
- [ ] Clean up local branch:
  ```bash
  git checkout main
  git pull
  git branch -d feature/monorepo-migration
  ```

**Validation**:
- [ ] All CI jobs pass
- [ ] PR description is comprehensive
- [ ] No conflicts with main
- [ ] Code review complete (if applicable)

**Commit**: N/A (GitHub PR merge)

**Notes**: _[Add session notes here]_

---

## Progress Tracking

### Overall Status
- **Current Phase**: Phase 0 (Preparation)
- **Phases Complete**: 0/12
- **Estimated Completion**: TBD

### Session Log

#### Session 1: 2025-10-25 (Planning)
- Created comprehensive migration plan
- Analyzed existing CI/CD workflow
- Decided on Option B (dual publishing)
- **Next**: Begin Phase 0

#### Session 2: [Date]
- [Notes from next session]

#### Session 3: [Date]
- [Notes from next session]

---

## Critical Considerations

### ⚠️ Things That Can Break

1. **Import Paths**
   - Problem: Imports from old paths will fail
   - Solution: Phase 5 updates all imports systematically
   - Test: TypeScript compilation after Phase 5

2. **Build Order**
   - Problem: Server depends on mcp-roots being built first
   - Solution: Root build script enforces order
   - Test: Clean build from root

3. **CI/CD Paths**
   - Problem: Hardcoded paths in GitHub Actions
   - Solution: Phase 8 updates all paths
   - Test: Workflow dispatch dry_run before merge

4. **Test Imports**
   - Problem: Tests may import from wrong location
   - Solution: Update test imports in Phase 3 and 7
   - Test: Run all tests after Phase 7

5. **npm Publish Workflow**
   - Problem: Publishing wrong package or from wrong directory
   - Solution: Careful workflow job configuration
   - Test: Dry-run publish before actual release

### 🛡️ Safety Nets

1. **Safety Tag**: `pre-monorepo-v2.0.6`
   - Created before any changes
   - Easy rollback point

2. **Feature Branch**: `feature/monorepo-migration`
   - All work isolated from main
   - Can be abandoned if needed

3. **Incremental Commits**
   - Small, logical commits
   - Easy to identify where things went wrong
   - Can revert individual phases

4. **Continuous Testing**
   - Run tests after major changes
   - Catch breakage early
   - Validate assumptions

5. **Dry Run Publishing**
   - Test publish before actual release
   - Verify package contents
   - Check for sensitive files

### 🔍 Common Pitfalls

1. **Forgetting to Update Imports**
   - Check: grep for old import paths before committing
   - Command: `grep -r "from './utils/roots" packages/swipl-mcp-server/src/`

2. **Build Artifacts in Git**
   - Check: Ensure dist/ and build/ are gitignored
   - Command: `git status` should not show compiled files

3. **Workspace Dependency Not Resolved**
   - Check: `node_modules/@vpursuit/mcp-roots` should be a symlink
   - Command: `ls -la node_modules/@vpursuit/`

4. **Tests Using Wrong Package**
   - Check: Test imports should use `@vpursuit/mcp-roots`
   - Command: `grep -r "from.*roots" packages/*/test/`

5. **CI/CD Using Old Paths**
   - Check: All paths in workflow include `packages/`
   - Command: `grep -n "dist/" .github/workflows/npm-publish.yml`

---

## Rollback Plan

### If Issues Arise During Migration

#### Minor Issues (Build/Test Failures)
1. Review git diff since last commit
2. Fix the specific issue
3. Re-run validation
4. Continue with plan

#### Major Issues (Architecture Problems)
1. Document the issue in this file
2. Commit current state with note
3. Consider alternative approach
4. May need to redesign affected phase

#### Critical Issues (Show Stopper)
1. Checkout safety tag:
   ```bash
   git checkout pre-monorepo-v2.0.6
   ```
2. Create new branch to investigate:
   ```bash
   git checkout -b investigate-monorepo-issue
   ```
3. Document findings
4. Revise plan
5. Start fresh with lessons learned

### If Issues Arise After Merge

#### Package Published with Issues
1. Publish a patch version immediately
2. Update GitHub release with warnings
3. Consider deprecating broken version:
   ```bash
   npm deprecate @vpursuit/package-name@broken-version "Contains critical bug, use X.Y.Z instead"
   ```

#### Rollback Not Possible (Already Published)
1. Fix forward - create patch release
2. Communicate clearly in release notes
3. Update documentation
4. Learn for next time

---

## Testing Checklist

### Before Each Commit
- [ ] Code compiles without errors
- [ ] No TypeScript errors
- [ ] Imports resolve correctly

### After Key Phases (3, 6, 7, 11)
- [ ] Build succeeds: `npm run build`
- [ ] All tests pass: `npm test`
- [ ] No console errors

### Before Pushing (Phase 12)
- [ ] Clean install works
- [ ] Full build works
- [ ] All tests pass (count matches)
- [ ] MCP inspector connects
- [ ] All tools functional
- [ ] Dry-run publish succeeds

### After Merge (Post-Migration)
- [ ] CI/CD pipeline passes
- [ ] Both packages publish successfully
- [ ] Installation works: `npx @vpursuit/swipl-mcp-server@latest`
- [ ] MCP clients can connect
- [ ] No breaking changes for users

---

## Post-Migration Tasks

### Immediate (Day 1)
- [ ] Tag mcp-roots release:
  ```bash
  git tag -a roots-v1.0.0 -m "Release @vpursuit/mcp-roots@1.0.0"
  git push origin roots-v1.0.0
  ```
- [ ] Monitor CI/CD for mcp-roots publish
- [ ] Verify mcp-roots on npm: `npm view @vpursuit/mcp-roots`
- [ ] Tag server release:
  ```bash
  git tag -a v2.0.7 -m "Release @vpursuit/swipl-mcp-server@2.0.7"
  git push origin v2.0.7
  ```
- [ ] Monitor CI/CD for server publish
- [ ] Verify server on npm: `npm view @vpursuit/swipl-mcp-server`
- [ ] Test installation from npm (not local):
  ```bash
  npx @vpursuit/swipl-mcp-server@latest
  ```

### Week 1
- [ ] Monitor GitHub issues for problems
- [ ] Update any external documentation
- [ ] Announce new mcp-roots package (Twitter, Reddit, etc.)
- [ ] Create example projects using mcp-roots
- [ ] Update CLAUDE.md with lessons learned

### Month 1
- [ ] Gather feedback from community
- [ ] Consider mcp-roots enhancements
- [ ] Evaluate if other packages should be extracted
- [ ] Document monorepo best practices

---

## Version & Tagging Strategy

### Tag Naming Convention
- **mcp-roots releases**: `roots-v1.0.0`, `roots-v1.0.1`, etc.
- **server releases**: `v2.0.7`, `v2.0.8`, etc. (existing pattern)

### Publishing Order
1. Tag and publish mcp-roots FIRST
2. Wait for npm publish to complete
3. Tag and publish server SECOND
4. Server depends on published mcp-roots version

### Versioning Guidelines
- **mcp-roots**: Start at 1.0.0 (initial release)
- **server**: Bump patch to 2.0.7 (internal refactor, no API changes)
- **Future**: Independent versioning for each package

### Example Workflow
```bash
# After merging to main
git checkout main
git pull

# Publish mcp-roots
git tag -a roots-v1.0.0 -m "Release @vpursuit/mcp-roots@1.0.0"
git push origin roots-v1.0.0
# Wait for CI/CD...

# Publish server
git tag -a v2.0.7 -m "Release @vpursuit/swipl-mcp-server@2.0.7"
git push origin v2.0.7
# Wait for CI/CD...

# Verify both published
npm view @vpursuit/mcp-roots
npm view @vpursuit/swipl-mcp-server
```

---

## Reference Links

### Documentation
- [npm workspaces](https://docs.npmjs.com/cli/v10/using-npm/workspaces)
- [MCP Specification](https://modelcontextprotocol.io)
- [TypeScript Project References](https://www.typescriptlang.org/docs/handbook/project-references.html)
- [Vitest Workspaces](https://vitest.dev/guide/workspace.html)

### Current Codebase
- Existing roots implementation: `src/utils/roots.ts`
- Existing constants: `src/constants.ts`
- Existing tests: `test/unit/roots.test.ts`
- Current CI/CD: `.github/workflows/npm-publish.yml`

### Migration Resources
- This plan: `MONOREPO_MIGRATION_PLAN.md`
- Roots implementation plan: `ROOTS_IMPLEMENTATION_PLAN.md`
- Code review: `code-review.md`
- Development guidelines: `CLAUDE.md`

---

## Questions & Decisions Log

### Question 1: Should we publish mcp-roots immediately?
**Decision**: Yes (Option B)
**Rationale**: Makes the package available for community use from day one
**Date**: 2025-10-25

### Question 2: How to handle CI/CD for two packages?
**Decision**: Separate jobs with tag-based triggers
**Rationale**: Clean separation, independent publishing
**Date**: 2025-10-25

### Question 3: [Future questions will be logged here]
**Decision**: TBD
**Rationale**: TBD
**Date**: TBD

---

## Success Metrics

### Technical Metrics
- ✅ All 98+ tests passing
- ✅ Both packages build successfully
- ✅ CI/CD pipeline working for both packages
- ✅ Zero breaking changes for end users
- ✅ Clean separation (mcp-roots has no swipl dependencies)

### Quality Metrics
- ✅ Comprehensive documentation for both packages
- ✅ Clear development setup instructions
- ✅ Working examples in mcp-roots README
- ✅ All git history preserved

### Community Metrics (Post-Launch)
- Downloads of `@vpursuit/mcp-roots` on npm
- GitHub stars/forks of mcp-roots
- Issues/PRs from community
- Other projects using mcp-roots

---

## End Notes

This is a **living document**. Update it as you progress through the migration:
- Mark phases as complete ✅
- Add notes from each session
- Document problems encountered and solutions
- Update estimates based on actual time taken
- Add new sections as needed

**Remember**: This is a multi-session project. Don't rush. Test thoroughly at each phase.

**Good luck!** 🚀
