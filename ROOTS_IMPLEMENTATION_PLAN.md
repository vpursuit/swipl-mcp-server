# Dynamic MCP Roots-Based Filesystem Access Implementation Plan

**Created**: 2025-10-15
**Status**: Ready for Implementation
**Goal**: Replace hardcoded `~/.swipl-mcp-server/` restriction with dynamic MCP roots-based filesystem access

## Analysis Summary

### Current State
- **Hardcoded restriction**: Only `~/.swipl-mcp-server/` directory allowed
- **Location**: `src/tools.ts:17` defines `ALLOWED_DIR`
- **Usage**: Path validation in `validateFilePath()` function (tools.ts:19-39)
- **Operations needing access**: `knowledge_base_load` and `knowledge_base_dump`

### MCP Roots Concept
According to MCP specification:
- **Roots are advisory** coordination mechanisms (not strict security)
- **Format**: `file://` URI scheme (e.g., `file:///Users/agent/workspace`)
- **Client provides** roots via `server.listRoots()` API call
- **Optional names**: Each root can have a human-readable name
- **Dynamic updates**: Clients can notify via `roots/list_changed` event
- **Purpose**: Help servers understand workspace boundaries

## Architecture Design

### 1. Root Discovery Module (`src/utils/roots.ts`)

```typescript
interface RootDirectory {
  uri: string;           // file:// URI
  path: string;          // Absolute filesystem path
  name?: string;         // Optional display name
}

class RootsManager {
  // Singleton pattern
  // Cache roots with TTL (5 minutes default)
  // Lazy initialization on first file operation
  // Graceful fallback to ~/.swipl-mcp-server if roots unavailable

  async discoverRoots(): Promise<RootDirectory[]>
  isPathAllowed(filePath: string): boolean
  getAllowedPaths(): string[]
  invalidateCache(): void
}
```

### 2. Integration Points

#### Phase 1: Non-Breaking Addition
- Add `server.listRoots()` call in `index.ts` during initialization
- Store roots in RootsManager singleton
- Keep existing `~/.swipl-mcp-server` as fallback
- Update `validateFilePath()` to check against roots OR fallback

#### Phase 2: Enhanced Validation
- Convert `file://` URIs to filesystem paths
- Validate paths are within ANY discovered root
- Provide clear error messages indicating which roots are allowed
- Add logging for transparency

#### Phase 3: Dynamic Updates (Future Enhancement)
- Listen for `roots/list_changed` notifications
- Invalidate cache and refresh roots list
- Support hot-reloading of allowed directories

### 3. Security Considerations

**Maintain Defense in Depth:**
- Roots are advisory, not security boundaries
- Keep path traversal protection (`..` detection)
- Add explicit blocklist for system directories (`/etc`, `/usr`, `/bin`, `/var`, `/sys`, `/proc`, `/boot`, `/dev`, `/root`)
- Validate `file://` URI format strictly
- Symlink resolution protection
- Consider adding opt-in env var `SWI_MCP_STRICT_ROOTS=true`

### 4. User Experience Improvements

**Better Error Messages:**
```
Before: "Security Error: Files can only be loaded from ~/.swipl-mcp-server/"

After:  "Security Error: File must be within allowed roots:
         - /Users/agent/project1 (My Project)
         - /Users/agent/workspace (Workspace)
         Attempted: /tmp/unsafe.pl"
```

**Capabilities Reporting:**
- Update `getCapabilitiesSummary()` to include discovered roots
- Add to help text dynamically
- Resource metadata about current roots

### 5. Implementation Steps

1. **Create `src/utils/roots.ts`**
   - RootsManager class with caching
   - URI to path conversion utilities
   - Fallback logic to `~/.swipl-mcp-server`
   - Path validation against roots

2. **Update `src/index.ts`**
   - Import RootsManager
   - Call `server.listRoots()` after connection
   - Handle gracefully if not supported by client
   - Pass server instance to RootsManager for future notifications

3. **Modify `src/tools.ts`**
   - Update `validateFilePath()` to use RootsManager
   - Enhanced error messages with available roots
   - Preserve all existing security checks

4. **Update `src/constants.ts`**
   - Add ROOT_CACHE_TTL_MS constant
   - Add BLOCKED_SYSTEM_DIRS array
   - Document roots-related constants

5. **Update Documentation**
   - README.md: Explain roots-based access
   - SECURITY.md: Document new security model
   - docs/features.md: Add roots section
   - Add troubleshooting for roots

6. **Add Tests**
   - `test/unit/roots.test.ts`: Unit tests for RootsManager
   - `test/integration/roots.test.ts`: Integration tests with mock roots
   - Test fallback behavior
   - Test security edge cases
   - Test URI parsing

### 6. Backward Compatibility Strategy

**Guaranteed Compatibility:**
- If client doesn't support roots → use `~/.swipl-mcp-server`
- If `listRoots()` fails → use `~/.swipl-mcp-server`
- If roots list is empty → use `~/.swipl-mcp-server`
- Existing configurations continue working unchanged
- All tests pass without modification

### 7. Environment Variables (Optional Enhancement)

```bash
# Override roots discovery (colon-separated paths)
SWI_MCP_ALLOWED_ROOTS="/path/one:/path/two"

# Strict mode (no fallback to ~/.swipl-mcp-server)
SWI_MCP_STRICT_ROOTS=true

# Disable roots feature (legacy mode)
SWI_MCP_USE_LEGACY_DIR=true

# Cache TTL in milliseconds
SWI_MCP_ROOTS_CACHE_TTL=300000
```

## Implementation Checklist

- [ ] Create `src/utils/roots.ts` with RootsManager class
- [ ] Add URI parsing utilities
- [ ] Update `src/constants.ts` with new constants
- [ ] Modify `src/tools.ts` validateFilePath()
- [ ] Update `src/index.ts` to discover roots on startup
- [ ] Update error messages in tools.ts
- [ ] Update `getCapabilitiesSummary()` to include roots info
- [ ] Create unit tests for RootsManager
- [ ] Create integration tests for roots functionality
- [ ] Update README.md
- [ ] Update SECURITY.md
- [ ] Update docs/features.md
- [ ] Run full test suite
- [ ] Manual testing with MCP inspector
- [ ] Update CLAUDE.md with new feature status

## Key Benefits

✅ **Idiomatic MCP design** - Uses standard roots protocol
✅ **Workspace-aware** - Automatically adapts to user's project structure
✅ **Zero breaking changes** - Graceful fallback ensures compatibility
✅ **Security maintained** - Advisory + validation layers
✅ **Better UX** - Contextual, helpful error messages
✅ **Testable** - Clean separation of concerns
✅ **Future-proof** - Ready for roots/list_changed notifications

## Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Roots may change during operation | Cache invalidation + future notification support |
| Client may not support roots | Fallback to `~/.swipl-mcp-server` |
| Security relaxation concerns | Keep path validation, add explicit blocklists |
| Performance overhead | Caching with TTL, lazy initialization |
| Complex testing scenarios | Comprehensive unit + integration tests |

## MCP SDK API Reference

```typescript
// Server method to request roots from client
server.listRoots(): Promise<{ roots: Root[] }>

// Root interface
interface Root {
  uri: string;      // file:// URI
  name?: string;    // Optional display name
}

// Future: Subscribe to changes
// server.onNotification('notifications/roots/list_changed', handler)
```

## Files to Modify

1. **New Files:**
   - `src/utils/roots.ts` (new)
   - `test/unit/roots.test.ts` (new)
   - `test/integration/roots.test.ts` (new)

2. **Modified Files:**
   - `src/tools.ts` (validateFilePath, error messages)
   - `src/index.ts` (roots discovery on startup)
   - `src/constants.ts` (new constants)
   - README.md (documentation)
   - SECURITY.md (security model)
   - docs/features.md (features documentation)

3. **Documentation Updates:**
   - All references to `~/.swipl-mcp-server` restriction
   - Security model explanation
   - Troubleshooting section

## Notes

- Keep implementation incremental and testable
- Maintain 100% backward compatibility
- Follow existing code patterns (DRY, error handling)
- Use existing utilities (logger, validation)
- Comprehensive error messages
- Document all design decisions
