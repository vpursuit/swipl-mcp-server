# Security Policy - @vpursuit/mcp-server-roots

Dynamic filesystem root discovery for MCP servers.

## Overview

This package manages filesystem access control for MCP servers through dynamic root discovery and path validation. Proper configuration is critical to prevent unauthorized file access.

## Threat Model

**Primary Risks:**
- Path traversal attacks (`../../../etc/passwd`)
- Access to system directories (`/etc`, `/usr`, `/bin`)
- Symlink following to restricted areas
- Information disclosure through path enumeration
- Race conditions in path validation

**Trust Boundaries:**
- MCP clients are partially trusted (provide roots)
- File paths from operations are untrusted
- Environment variables are developer-controlled
- System directories are never trusted

## Built-in Security Features

### 1. System Directory Blocking

Automatic blocking of sensitive system directories:

**Blocked Directories:**
- `/etc` - System configuration
- `/usr` - System binaries and libraries
- `/bin`, `/sbin` - Essential binaries
- `/var` - Variable data
- `/sys`, `/proc` - Kernel interfaces
- `/boot` - Boot files
- `/dev` - Device files
- `/root` - Root user home

**Example:**
```typescript
const result = await manager.validatePath('/etc/passwd');
// Returns: { allowed: false, error: 'Path is in blocked system directory' }
```

### 2. Path Validation

Comprehensive path validation before allowing access:

```typescript
interface PathValidationResult {
  allowed: boolean;
  error?: string;
  matchedRoot?: RootDirectory;
}
```

**Checks:**
- Path must be within allowed roots
- No parent directory traversal (`..`)
- System directories blocked
- Absolute path resolution
- Symlink resolution (follows to actual location)

### 3. Root Discovery Hierarchy

Three-tier root discovery with security implications:

**1. MCP Client Roots** (Highest Priority):
```typescript
// Provided by MCP client via server.listRoots()
const roots = await manager.getRoots();
// Trust boundary: client-provided, must validate
```

**2. Environment Override**:
```typescript
// Developer-controlled override
process.env.SWI_MCP_ALLOWED_ROOTS = '/app/data,/app/uploads';
// Trust boundary: developer-controlled, trusted
```

**3. No Fallback** (Secure by Default):
```typescript
// No automatic fallback directory
// Explicit root configuration required for file operations
// File operations disabled if no roots configured
```

### 4. Caching with Invalidation

5-minute cache with notification support:
- Reduces excessive MCP protocol calls
- Cache invalidated on `roots/list_changed` notification
- Fresh validation on cache miss

**Security Note:** Cache doesn't bypass validation, only reduces discovery frequency.

### 5. Strict Mode

Optional strict mode for high-security environments:

```typescript
process.env.SWI_MCP_STRICT_ROOTS = 'true';
// Disables fallback directory
// Only allows explicitly configured roots
```

## Usage Security Guidelines

### For Server Implementers

**1. Configure Allowed Roots:**
```typescript
import { RootsManager } from '@vpursuit/mcp-server-roots';

const manager = RootsManager.getInstance();
manager.setServerInstance(server);

// Discover roots (respects security hierarchy)
await manager.discoverRoots();

// Validate before file operations
const result = await manager.validatePath(userProvidedPath);
if (!result.allowed) {
  throw new Error(`Access denied: ${result.error}`);
}
```

**2. Enable Strict Mode (Production):**
```typescript
// In production environment
process.env.SWI_MCP_STRICT_ROOTS = 'true';
process.env.SWI_MCP_ALLOWED_ROOTS = '/app/data,/app/cache';

// No fallback directory, explicit control only
```

**3. Validate Paths Before Operations:**
```typescript
async function loadFile(filePath: string) {
  // ALWAYS validate first
  const validation = await manager.validatePath(filePath);

  if (!validation.allowed) {
    console.error(`Security: Blocked access to ${filePath}`);
    throw new Error('Access denied');
  }

  // Safe to proceed
  return fs.readFile(filePath, 'utf8');
}
```

**4. Handle Root Changes:**
```typescript
// Listen for root changes
server.onNotification('roots/list_changed', async () => {
  // Cache automatically invalidated
  await manager.discoverRoots(true);  // Force refresh
  console.log('Roots updated, re-validated paths');
});
```

### For Plugin Developers

**1. Always Use validatePath:**
```typescript
// ✅ Good: Validate before access
const tool = {
  handler: async (args) => {
    const validation = await manager.validatePath(args.filePath);

    if (!validation.allowed) {
      return {
        success: false,
        error: 'File access not allowed'
      };
    }

    return { success: true, data: await readFile(args.filePath) };
  }
};

// ❌ Bad: No validation
const tool = {
  handler: async (args) => {
    return { data: await readFile(args.filePath) };  // Dangerous!
  }
};
```

**2. Sanitize User Paths:**
```typescript
import path from 'path';

function sanitizePath(userPath: string): string {
  // Resolve to absolute path
  const absolute = path.resolve(userPath);

  // Additional sanitization
  const normalized = path.normalize(absolute);

  // Remove any remaining '..'
  if (normalized.includes('..')) {
    throw new Error('Path traversal detected');
  }

  return normalized;
}
```

**3. Check Path Type:**
```typescript
import fs from 'fs/promises';

async function validateFileType(filePath: string) {
  const stats = await fs.stat(filePath);

  if (stats.isSymlink()) {
    // Symlinks require additional validation
    const realPath = await fs.realpath(filePath);
    return manager.validatePath(realPath);
  }

  if (stats.isDirectory()) {
    // Handle directories differently
    return { allowed: false, error: 'Expected file, got directory' };
  }

  return { allowed: true };
}
```

## Configuration

**Environment Variables:**

- **`SWI_MCP_ALLOWED_ROOTS`**: Comma-separated list of allowed root paths
  ```bash
  export SWI_MCP_ALLOWED_ROOTS="/app/data,/app/uploads,/tmp/workspace"
  ```

- **`SWI_MCP_STRICT_ROOTS`**: Disable fallback directory (high security)
  ```bash
  export SWI_MCP_STRICT_ROOTS="true"
  ```

- **`SWI_MCP_USE_LEGACY_DIR`**: Force use of fallback directory (development)
  ```bash
  export SWI_MCP_USE_LEGACY_DIR="true"
  ```

- **`DEBUG`**: Enable debug logging
  ```bash
  export DEBUG="mcp-roots"
  ```

## Security Best Practices

### Path Traversal Prevention

```typescript
// ❌ Vulnerable to path traversal
const filePath = userInput;  // Could be "../../../etc/passwd"
await readFile(filePath);

// ✅ Protected with validation
const validation = await manager.validatePath(userInput);
if (!validation.allowed) {
  throw new Error('Access denied');
}
await readFile(validation.matchedRoot!.path);
```

### Symlink Safety

```typescript
import fs from 'fs/promises';

async function secureReadFile(filePath: string) {
  // Resolve symlinks first
  const realPath = await fs.realpath(filePath);

  // Validate resolved path
  const validation = await manager.validatePath(realPath);

  if (!validation.allowed) {
    throw new Error('Symlink target outside allowed roots');
  }

  return fs.readFile(realPath, 'utf8');
}
```

### Race Condition Prevention

```typescript
// ❌ Time-of-check to time-of-use (TOCTOU) vulnerability
const validation = await manager.validatePath(filePath);
// ... time passes, file could be replaced with symlink ...
await fs.readFile(filePath);

// ✅ Atomic validation and operation
const validation = await manager.validatePath(filePath);
if (validation.allowed) {
  // Immediately perform operation
  const fd = await fs.open(filePath, 'r');
  try {
    return await fd.readFile('utf8');
  } finally {
    await fd.close();
  }
}
```

## Reporting Vulnerabilities

If you discover a security vulnerability in @vpursuit/mcp-server-roots:

1. **Do not** open a public GitHub issue
2. Use [GitHub Security Advisories](https://github.com/vpursuit/model-context-lab/security/advisories/new) for private reporting
3. Include:
   - Affected version
   - Proof of Concept (file path that bypasses validation)
   - Expected vs. actual behavior
   - Impact assessment

See the [root SECURITY.md](../../SECURITY.md) for ecosystem-wide security policy.

## Testing Security

**Test System Directory Blocking:**
```typescript
const result = await manager.validatePath('/etc/passwd');
assert(result.allowed === false);
assert(result.error.includes('blocked system directory'));
```

**Test Path Traversal:**
```typescript
const result = await manager.validatePath('~/.model-context-lab/../../../etc/passwd');
assert(result.allowed === false);
```

**Test Valid Paths:**
```typescript
const result = await manager.validatePath('~/.model-context-lab/data.pl');
assert(result.allowed === true);
assert(result.matchedRoot !== undefined);
```

**Test Strict Mode:**
```typescript
process.env.SWI_MCP_STRICT_ROOTS = 'true';
const result = await manager.validatePath('~/.model-context-lab/data.pl');
// Should fail if no explicit roots configured
assert(result.allowed === false);
```

## Resources

- [OWASP Path Traversal](https://owasp.org/www-community/attacks/Path_Traversal)
- [Root Security Policy](../../SECURITY.md)
- [Package README](./README.md)
- [Model Context Protocol](https://modelcontextprotocol.io)

## License

BSD-3-Clause - See [LICENSE](./LICENSE) file for details.
