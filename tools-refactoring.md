# Tool Consolidation & Source Storage Architecture Plan

## Overview

**Goal**: Reduce API from 15 tools to 7 tools (53% reduction), add source preservation, enable file-scoped operations, improve semantic clarity.

**Motivation**:
- All tools loaded into LLM context - fewer tools = massive token savings
- Source preservation enables dump to match input (variable names kept)
- File-scoped operations allow import/unimport by file
- Semantic naming improves clarity (snapshot vs dump, reset vs clear)
- Operation-based grouping reduces decision complexity for AI agents
- Resource URIs aligned with tool naming for consistency

## Changes Summary

| Change Type | From | To | Impact |
|------------|------|-----|--------|
| **Architecture** | No source storage | TypeScript Map storage | Preserves variable names, enables file provenance |
| **Consolidate** | `knowledge_base_assert` + `knowledge_base_assert_many` | `clauses(operation: 'assert')` | Array support, shared helper |
| **Consolidate** | `knowledge_base_retract` + `knowledge_base_retract_many` | `clauses(operation: 'retract')` | Array support, create shared helper |
| **Replace** | `knowledge_base_load` | `files(operation: 'import')` | File provenance tracking |
| **Add** | N/A | `files(operation: 'unimport')` | File-scoped removal |
| **Add** | N/A | `files(operation: 'list')` | Show imported files |
| **Replace** | `knowledge_base_dump` | `workspace(operation: 'snapshot')` | Source storage output, synced resource |
| **Replace** | `knowledge_base_clear` | `workspace(operation: 'reset')` | Clears source + compiled |
| **Consolidate** | `symbols_list` | `workspace(operation: 'list_symbols')` | Grouped with workspace inspection |
| **Consolidate** | `query_start` + `query_startEngine` | `query_start(use_engine: bool)` | Default engine mode |
| **Consolidate** | `help` + `capabilities` | `help(topic: optional)` | Plain text, token-optimized |
| **Remove** | `license` tool | `reference://license` resource only | Tools for actions, resources for data |
| **Remove** | `knowledge_base_load_library` | Pre-import + CLI config | clpfd default, others via --kb-libraries |
| **Rename Resource** | `prolog://knowledge_base/dump` | `prolog://workspace/snapshot` | Semantic consistency |
| **Rename Resource** | `prolog://knowledge_base/predicates` | `prolog://workspace/symbols` | Semantic consistency |
| **Remove Resource** | `reference://capabilities` | Merged into help tool | Eliminate redundancy |

**Expected Outcome**: 15 tools → 7 tools (53% reduction), ~50% token savings, source preservation, file-scoped operations

---

## Final Tool Inventory (7 Tools)

### KB Tools (3)

**1. `clauses` - Manage inline facts/rules**
```typescript
{
  operation: 'assert' | 'retract',
  clauses: string | string[],
}
```
- **assert**: Add facts/rules to KB, store in source map
- **retract**: Remove facts/rules from KB and source map
- Accepts single string or array for bulk operations

**2. `files` - Manage file imports**
```typescript
{
  operation: 'import' | 'unimport' | 'list',
  filename?: string,  // Required for import/unimport, omit for list
}
```
- **import**: Load .pl file, track provenance, prevent duplicates
- **unimport**: Remove all clauses from specific file
- **list**: Show imported files with metadata (path, clause count, timestamp)

**3. `workspace` - Inspect and manage workspace**
```typescript
{
  operation: 'snapshot' | 'reset' | 'list_symbols',
}
```
- **snapshot**: Export source text (synced with `prolog://workspace/snapshot` resource)
- **reset**: Clear everything (source storage + compiled KB)
- **list_symbols**: List available predicates (replaces `symbols_list`)

### Query Tools (3)

**4. `query_start` - Start query**
```typescript
{
  query: string,
  use_engine?: boolean,  // Default: true
}
```
- **use_engine: true** (default): Engine mode with backtracking
- **use_engine: false**: Standard mode with call_nth/2 pagination

**5. `query_next` - Get next solution**
```typescript
{}  // No parameters
```

**6. `query_close` - Close query**
```typescript
{}  // No parameters
```

### Meta (1)

**7. `help` - Get help**
```typescript
{
  topic?: 'overview' | 'tools' | 'prompts' | 'modes' | 'security' | 'examples',
}
```
- Merged with capabilities tool
- Plain text output, token-optimized for LLMs
- Optional topic for focused queries

---

## Final Resource Inventory (5 Resources)

**Updated URIs for semantic consistency:**

| Resource URI | Description | Synced with Tool |
|-------------|-------------|------------------|
| `prolog://workspace/snapshot` | Current workspace state | `workspace(operation: 'snapshot')` ✓ |
| `prolog://workspace/symbols` | List of predicates | `workspace(operation: 'list_symbols')` ✓ |
| `reference://help` | Usage guidelines | `help` tool ✓ |
| `reference://license` | License text | (no tool, reference only) |
| `reference://logo` | Server logo SVG | (no tool, reference only) |

**Removed:**
- ~~`reference://capabilities`~~ - Merged into `help` tool

---

## Architecture: Source Storage (TypeScript Side)

### Problem Statement

**Current Flow** (source is lost):
```
User inputs: parent(X, Y) :- ancestor(X, Y).
  ↓
Parse & Compile
  ↓
Prolog stores: parent(A, B) :- ancestor(A, C).  ← Variable names changed!
  ↓
Dump outputs: parent(A,B):-ancestor(A,C).  ← Not what user input!
```

**New Flow** (parallel storage):
```
User inputs: parent(X, Y) :- ancestor(X, Y).
  ↓
Store original text → TypeScript Map: "parent(X, Y) :- ancestor(X, Y)."
  ↓
Parse & Compile → Prolog KB: parent(A, B) :- ancestor(A, C).
  ↓
Snapshot reads from Map → "parent(X, Y) :- ancestor(X, Y)."  ← Original preserved!
```

### Data Structure

**Location**: `plugins/server/prolog/src/PrologInterface.ts`

```typescript
interface SourceEntry {
  id: string;              // UUID for unique identification
  sourceText: string;      // Original input text with variable names preserved
  type: 'inline' | 'file'; // Origin: direct assertion or file import
  file?: string;           // File path (if type='file')
  timestamp: number;       // When added (Date.now())
  compiled: boolean;       // Successfully asserted to Prolog?
}

class PrologInterface {
  private kbSourceStorage: Map<string, SourceEntry> = new Map();

  // Shared implementation for tool + resource
  async getSnapshot(): Promise<string> {
    const sources = Array.from(this.kbSourceStorage.values())
      .filter(entry => entry.compiled)
      .sort((a, b) => a.timestamp - b.timestamp)  // Preserve insertion order
      .map(entry => entry.sourceText);
    return sources.join('\n');
  }

  // Track imported files
  getImportedFiles(): Array<{filename: string, clauseCount: number, timestamp: number}> {
    const fileMap = new Map<string, {count: number, timestamp: number}>();

    for (const entry of this.kbSourceStorage.values()) {
      if (entry.type === 'file' && entry.file) {
        const existing = fileMap.get(entry.file);
        if (existing) {
          existing.count++;
          // Keep earliest timestamp
          if (entry.timestamp < existing.timestamp) {
            existing.timestamp = entry.timestamp;
          }
        } else {
          fileMap.set(entry.file, {count: 1, timestamp: entry.timestamp});
        }
      }
    }

    return Array.from(fileMap.entries())
      .map(([filename, data]) => ({
        filename,
        clauseCount: data.count,
        timestamp: data.timestamp
      }))
      .sort((a, b) => a.timestamp - b.timestamp);
  }
}
```

### Key Operations

**Assert with Source**:
```typescript
async assertClauseWithSource(
  clause: string,
  type: 'inline' | 'file',
  file?: string
): Promise<{success: boolean; id?: string; error?: string}> {
  const id = uuidv4();

  try {
    // Normalize clause (remove trailing period for assertion)
    const normalized = clause.trim().endsWith('.')
      ? clause.trim().slice(0, -1)
      : clause.trim();

    // Assert to Prolog
    const result = await this.queryOnce(`assertz(knowledge_base:(${normalized}))`);

    if (result.status === 'success') {
      // Store source on success
      this.kbSourceStorage.set(id, {
        id,
        sourceText: clause.trim().endsWith('.') ? clause.trim() : clause.trim() + '.',
        type,
        file,
        timestamp: Date.now(),
        compiled: true
      });
      return {success: true, id};
    } else {
      return {success: false, error: 'Assertion failed'};
    }
  } catch (error) {
    return {success: false, error: String(error)};
  }
}
```

**Retract with Source**:
```typescript
async retractClauseWithSource(clause: string): Promise<boolean> {
  // Normalize for comparison
  const normalizedInput = clause.trim().endsWith('.')
    ? clause.trim()
    : clause.trim() + '.';

  // Find matching source entries
  const matchingIds: string[] = [];

  for (const [id, entry] of this.kbSourceStorage.entries()) {
    if (entry.sourceText === normalizedInput) {
      matchingIds.push(id);
    }
  }

  // Retract from Prolog (without period)
  const forProlog = normalizedInput.slice(0, -1);
  const result = await this.queryOnce(`retract(knowledge_base:(${forProlog}))`);

  // Remove first matching entry from source storage
  if (result.status === 'success' && matchingIds.length > 0) {
    this.kbSourceStorage.delete(matchingIds[0]);
    return true;
  }

  return false;
}
```

**Clear Workspace**:
```typescript
async clearWorkspaceWithSource(): Promise<void> {
  // Clear Prolog KB (using existing predicate)
  await this.query('abolish_all_user_predicates');

  // Clear source storage
  this.kbSourceStorage.clear();
}
```

**Import File with Source**:
```typescript
async importFileWithSource(filename: string): Promise<{
  success: boolean;
  clausesAdded: number;
  errors: string[];
}> {
  // Check if already imported
  const alreadyImported = Array.from(this.kbSourceStorage.values())
    .some(entry => entry.file === filename);

  if (alreadyImported) {
    return {
      success: false,
      clausesAdded: 0,
      errors: [`File ${filename} already imported. Use unimport first to reload.`]
    };
  }

  // Parse file to clause strings (preserving variable names)
  // This needs special handling - see "File Parsing" section below
  const clauses = await this.parseFileToStringArray(filename);

  let clausesAdded = 0;
  const errors: string[] = [];

  for (const clause of clauses) {
    const result = await this.assertClauseWithSource(clause, 'file', filename);
    if (result.success) {
      clausesAdded++;
    } else {
      errors.push(`Failed to assert: ${clause.substring(0, 50)}... - ${result.error}`);
    }
  }

  return {
    success: errors.length === 0,
    clausesAdded,
    errors
  };
}
```

**Unimport File**:
```typescript
async unimportFile(filename: string): Promise<{
  success: boolean;
  clausesRemoved: number;
}> {
  const toRemove: string[] = [];

  // Find all entries from this file
  for (const [id, entry] of this.kbSourceStorage.entries()) {
    if (entry.type === 'file' && entry.file === filename) {
      toRemove.push(id);
    }
  }

  if (toRemove.length === 0) {
    return {success: false, clausesRemoved: 0};
  }

  let clausesRemoved = 0;

  for (const id of toRemove) {
    const entry = this.kbSourceStorage.get(id);
    if (entry) {
      // Retract from Prolog
      const normalized = entry.sourceText.endsWith('.')
        ? entry.sourceText.slice(0, -1)
        : entry.sourceText;

      await this.queryOnce(`retract(knowledge_base:(${normalized}))`);

      // Remove from source storage
      this.kbSourceStorage.delete(id);
      clausesRemoved++;
    }
  }

  return {success: true, clausesRemoved};
}
```

### File Parsing (Preserving Variable Names)

**Challenge**: SWI-Prolog's `read_term/2` provides `variable_names` option, but we need to convert back to string with original variable names.

**Solution 1: Use write_canonical with variable names**

Add to `prolog_server.pl`:
```prolog
% Helper to read file and produce string representations with variable names preserved
read_file_to_string_clauses(File, Clauses) :-
    read_file_to_terms(File, Terms, [variable_names(VarNames), module(knowledge_base)]),
    maplist(term_to_string_with_vars(VarNames), Terms, Clauses).

term_to_string_with_vars(VarNames, Term, String) :-
    with_output_to(string(String),
        write_term(Term, [
            variable_names(VarNames),
            quoted(true),
            numbervars(false)
        ])
    ).
```

**Solution 2: Simpler approach - just read file as text**

For initial implementation, we can simply read the file as text and split by clauses:

```typescript
async parseFileToStringArray(filename: string): Promise<string[]> {
  const fs = await import('fs/promises');
  const content = await fs.readFile(filename, 'utf-8');

  // Simple splitting: split by period followed by newline or EOF
  // This is naive but works for well-formatted files
  const clauses: string[] = [];
  let current = '';

  for (const line of content.split('\n')) {
    const trimmed = line.trim();

    // Skip comments and empty lines
    if (!trimmed || trimmed.startsWith('%')) {
      continue;
    }

    // Skip directives (for now - may need special handling)
    if (trimmed.startsWith(':-')) {
      continue;
    }

    current += ' ' + trimmed;

    // Check if clause ends
    if (current.trim().endsWith('.')) {
      clauses.push(current.trim());
      current = '';
    }
  }

  return clauses;
}
```

**Note**: This simple approach has limitations with multi-line clauses. For production, use the Prolog-based parsing from Solution 1.

---

## Detailed Implementation

### 1. Add Source Storage to PrologInterface

**File**: `plugins/server/prolog/src/PrologInterface.ts`

**Add imports**:
```typescript
import { v4 as uuidv4 } from 'uuid';
```

**Add to class**:
```typescript
private kbSourceStorage: Map<string, SourceEntry> = new Map();

// [Add all methods from "Architecture: Source Storage" section above]
```

---

### 2. Implement New Tool: `clauses`

**File**: `plugins/server/prolog/src/schemas.ts`

```typescript
export const clausesSchema = {
  operation: z.enum(['assert', 'retract'])
    .describe("Operation to perform: 'assert' adds facts/rules to KB, 'retract' removes matching facts/rules"),
  clauses: z.union([z.string(), z.array(z.string())])
    .describe("Single Prolog clause or array of clauses. Facts: 'parent(john, mary).' Rules: 'ancestor(X,Y) :- parent(X,Y).' Periods optional."),
} as const;

// DELETE old schemas
// - knowledgeBaseAssertSchema
// - knowledgeBaseAssertManySchema
// - knowledgeBaseRetractSchema
// - knowledgeBaseRetractManySchema
```

**File**: `plugins/server/prolog/src/tools.ts`

```typescript
clauses: {
  description: "Manage inline facts and rules in the knowledge base. Use operation 'assert' to add, 'retract' to remove. Accepts single clause or array for bulk operations. Source text is preserved (variable names kept).",
  inputSchema: zodToJsonSchema(z.object(clausesSchema)),
  handler: async (
    { operation, clauses }: { operation: 'assert' | 'retract'; clauses: string | string[] },
    _extra
  ) => {
    const startTime = performance.now();
    await prologInterface.start();

    // Normalize to array
    const clauseArray = Array.isArray(clauses) ? clauses : [clauses];

    if (clauseArray.length === 0) {
      throw new Error("At least one clause required");
    }

    const outcomes: KnowledgeBaseClauseOutcome[] = [];
    let successCount = 0;
    let errorCount = 0;

    if (operation === 'assert') {
      for (const clause of clauseArray) {
        // Validate
        if (typeof clause !== 'string') {
          outcomes.push({
            status: 'error',
            clause: String(clause),
            message: `Expected string, got ${typeof clause}`
          });
          errorCount++;
          continue;
        }

        if (clause.length > MAX_FACT_LENGTH) {
          outcomes.push({
            status: 'error',
            clause: clause.substring(0, 50) + '...',
            message: `Clause exceeds maximum length of ${MAX_FACT_LENGTH}`
          });
          errorCount++;
          continue;
        }

        const result = await prologInterface.assertClauseWithSource(clause, 'inline');
        if (result.success) {
          outcomes.push({ status: 'success', clause });
          successCount++;
        } else {
          outcomes.push({ status: 'error', clause, message: result.error });
          errorCount++;
        }
      }
    } else { // retract
      for (const clause of clauseArray) {
        // Validate
        if (typeof clause !== 'string') {
          outcomes.push({
            status: 'error',
            clause: String(clause),
            message: `Expected string, got ${typeof clause}`
          });
          errorCount++;
          continue;
        }

        const success = await prologInterface.retractClauseWithSource(clause);
        if (success) {
          outcomes.push({ status: 'success', clause });
          successCount++;
        } else {
          outcomes.push({
            status: 'error',
            clause,
            message: 'No matching clause found or retraction failed'
          });
          errorCount++;
        }
      }
    }

    const processingTimeMs = performance.now() - startTime;
    return formatClauseResults(operation, outcomes, processingTimeMs);
  },
},

// DELETE old tool handlers:
// - knowledge_base_assert
// - knowledge_base_assert_many
// - knowledge_base_retract
// - knowledge_base_retract_many
```

---

### 3. Implement New Tool: `files`

**File**: `plugins/server/prolog/src/schemas.ts`

```typescript
export const filesSchema = {
  operation: z.enum(['import', 'unimport', 'list'])
    .describe("File operation: 'import' loads .pl file with provenance tracking, 'unimport' removes file's clauses, 'list' shows imported files"),
  filename: z.string().optional()
    .describe("File path (required for import/unimport, omit for list). Must be within configured root directories."),
} as const;

// DELETE old schemas:
// - knowledgeBaseLoadSchema
```

**File**: `plugins/server/prolog/src/tools.ts`

```typescript
files: {
  description: "Manage Prolog file imports with provenance tracking. Import .pl files (prevents duplicates), unimport to remove file-specific clauses, or list all imported files with metadata. Files are parsed to preserve variable names in source storage.",
  inputSchema: zodToJsonSchema(z.object(filesSchema)),
  handler: async (
    { operation, filename }: { operation: 'import' | 'unimport' | 'list'; filename?: string },
    _extra
  ) => {
    const startTime = performance.now();

    if (operation === 'list') {
      // List imported files
      const importedFiles = prologInterface.getImportedFiles();

      const processingTimeMs = performance.now() - startTime;

      return createToolResponse({
        message: `Found ${importedFiles.length} imported file(s)`,
        structuredContent: {
          type: 'files_list',
          files: importedFiles,
          count: importedFiles.length
        },
        processingTimeMs
      });
    }

    // Import and unimport require filename
    if (!filename) {
      throw new Error(`filename parameter required for ${operation} operation`);
    }

    // Validate file path (roots checking)
    const absolutePath = path.resolve(filename);
    await validateFilePath(absolutePath);

    if (operation === 'import') {
      await prologInterface.start();
      const result = await prologInterface.importFileWithSource(absolutePath);

      const processingTimeMs = performance.now() - startTime;

      if (result.success) {
        return createToolResponse({
          message: `Successfully imported ${result.clausesAdded} clause(s) from ${filename}`,
          structuredContent: {
            type: 'file_import',
            filename: absolutePath,
            clausesAdded: result.clausesAdded,
            status: 'success'
          },
          processingTimeMs
        });
      } else {
        return createToolResponse({
          message: `Import ${result.clausesAdded > 0 ? 'partial' : 'failed'}: ${result.clausesAdded} succeeded, ${result.errors.length} failed`,
          structuredContent: {
            type: 'file_import',
            filename: absolutePath,
            clausesAdded: result.clausesAdded,
            errors: result.errors,
            status: result.clausesAdded > 0 ? 'partial' : 'failed'
          },
          processingTimeMs,
          isError: result.clausesAdded === 0
        });
      }

    } else { // unimport
      await prologInterface.start();
      const result = await prologInterface.unimportFile(absolutePath);

      const processingTimeMs = performance.now() - startTime;

      if (!result.success) {
        return createToolResponse({
          message: `File ${filename} not found in imports`,
          structuredContent: {
            type: 'file_unimport',
            filename: absolutePath,
            clausesRemoved: 0,
            status: 'not_found'
          },
          processingTimeMs,
          isError: true
        });
      }

      return createToolResponse({
        message: `Successfully unimported ${result.clausesRemoved} clause(s) from ${filename}`,
        structuredContent: {
          type: 'file_unimport',
          filename: absolutePath,
          clausesRemoved: result.clausesRemoved,
          status: 'success'
        },
        processingTimeMs
      });
    }
  },
},

// DELETE old tool handler:
// - knowledge_base_load
```

---

### 4. Implement New Tool: `workspace`

**File**: `plugins/server/prolog/src/schemas.ts`

```typescript
export const workspaceSchema = {
  operation: z.enum(['snapshot', 'reset', 'list_symbols'])
    .describe("Workspace operation: 'snapshot' exports current state, 'reset' clears all clauses, 'list_symbols' shows available predicates"),
} as const;

// DELETE old schemas:
// - knowledgeBaseDumpSchema
// - knowledgeBaseClearSchema
// - symbolsListSchema
```

**File**: `plugins/server/prolog/src/tools.ts`

```typescript
workspace: {
  description: "Inspect and manage the knowledge base workspace. Snapshot exports all clauses with preserved source formatting (synced with prolog://workspace/snapshot resource). Reset clears everything. List_symbols shows available predicates.",
  inputSchema: zodToJsonSchema(z.object(workspaceSchema)),
  handler: async (
    { operation }: { operation: 'snapshot' | 'reset' | 'list_symbols' },
    _extra
  ) => {
    const startTime = performance.now();
    await prologInterface.start();

    if (operation === 'snapshot') {
      // Export source text (synced with resource)
      const dump = await prologInterface.getSnapshot();

      const processingTimeMs = performance.now() - startTime;

      const lineCount = dump ? dump.split('\n').filter(line => line.trim()).length : 0;

      return createToolResponse({
        message: lineCount > 0
          ? `Knowledge base snapshot (${lineCount} clause(s))`
          : 'Knowledge base is empty',
        structuredContent: {
          type: 'workspace_snapshot',
          dump,
          clauseCount: lineCount
        },
        processingTimeMs
      });

    } else if (operation === 'reset') {
      // Clear everything
      await prologInterface.clearWorkspaceWithSource();

      const processingTimeMs = performance.now() - startTime;

      return createToolResponse({
        message: 'Knowledge base reset - all clauses and source storage cleared',
        structuredContent: {
          type: 'workspace_reset',
          status: 'success'
        },
        processingTimeMs
      });

    } else { // list_symbols
      // Replaces old symbols_list tool
      const result = await prologInterface.query('list_knowledge_base_predicates');

      const processingTimeMs = performance.now() - startTime;

      // Parse predicates from result (existing logic)
      const lines = result.split('\n');
      const predicates: string[] = [];

      for (const line of lines) {
        const trimmed = line.trim();
        if (trimmed && !trimmed.startsWith('%') && !trimmed.startsWith('?-')) {
          predicates.push(trimmed);
        }
      }

      return createToolResponse({
        message: `Found ${predicates.length} predicate(s) in knowledge base`,
        structuredContent: {
          type: 'workspace_symbols',
          predicates,
          count: predicates.length
        },
        processingTimeMs
      });
    }
  },
},

// DELETE old tool handlers:
// - knowledge_base_dump
// - knowledge_base_clear
// - symbols_list
```

---

### 5. Update Tool: `query_start`

**File**: `plugins/server/prolog/src/schemas.ts`

```typescript
export const queryStartSchema = {
  query: z.string().describe("Prolog query to execute"),
  use_engine: z.boolean()
    .optional()
    .default(true)
    .describe("Use engine mode (true, default) for true backtracking, or standard mode (false) for call_nth/2 pagination"),
} as const;

// DELETE queryStartEngineSchema
```

**File**: `plugins/server/prolog/src/tools.ts`

```typescript
query_start: {
  description: "Start a Prolog query. Engine mode (default) provides true backtracking with unlimited solutions and memory efficiency. Standard mode uses call_nth/2 pagination for predictable step-through.",
  inputSchema: zodToJsonSchema(z.object(queryStartSchema)),
  handler: async (
    { query, use_engine = true }: { query: string; use_engine?: boolean },
    _extra
  ) => {
    const startTime = performance.now();
    await prologInterface.start();

    let result;
    if (use_engine) {
      result = await prologInterface.startEngine(query);
    } else {
      result = await prologInterface.startQuery(query);
    }

    const processingTimeMs = performance.now() - startTime;

    return createToolResponse({
      message: `Query started in ${use_engine ? 'engine' : 'standard'} mode: ${query}`,
      structuredContent: {
        type: 'query_start',
        query,
        mode: use_engine ? 'engine' : 'standard',
        status: result.status,
        engine_ready: use_engine ? result.engine_ready : undefined
      },
      processingTimeMs
    });
  },
},

// DELETE query_startEngine tool handler
```

---

### 6. Update Tool: `help`

**File**: `plugins/server/prolog/src/schemas.ts`

```typescript
export const helpSchema = {
  topic: z.enum(['overview', 'tools', 'prompts', 'modes', 'security', 'examples'])
    .optional()
    .describe("Optional topic for focused help. Omit for complete overview."),
} as const;

// DELETE capabilitiesSchema
```

**File**: `plugins/server/prolog/src/tools.ts`

```typescript
help: {
  description: "Get comprehensive help about the Prolog MCP server. Includes tool reference, query modes, security model, expert prompts, and examples. Optimized for AI agents. Use topic parameter for focused queries.",
  inputSchema: zodToJsonSchema(z.object(helpSchema)),
  handler: async ({ topic }: { topic?: string }, _extra) => {
    let helpText = "";

    if (!topic || topic === 'overview') {
      helpText += `# SWI-Prolog MCP Server\n\n`;
      helpText += `Interactive Prolog environment for logic programming, constraint solving, and knowledge-based reasoning.\n\n`;
      helpText += `Server provides 7 tools across 3 categories: KB management (3), query execution (3), meta (1).\n\n`;
    }

    if (!topic || topic === 'tools') {
      helpText += `## Tools (7 total)\n\n`;
      helpText += `**KB Management**:\n`;
      helpText += `- clauses(operation, clauses) - Assert/retract facts and rules (inline, preserves source)\n`;
      helpText += `  - operation: 'assert' | 'retract'\n`;
      helpText += `  - clauses: string | string[] - Single or array for bulk\n`;
      helpText += `- files(operation, filename?) - Import/unimport/list .pl files (with provenance)\n`;
      helpText += `  - operation: 'import' | 'unimport' | 'list'\n`;
      helpText += `  - filename: required for import/unimport\n`;
      helpText += `- workspace(operation) - Snapshot/reset/list workspace state\n`;
      helpText += `  - operation: 'snapshot' | 'reset' | 'list_symbols'\n\n`;
      helpText += `**Query Execution**:\n`;
      helpText += `- query_start(query, use_engine?) - Start query (engine mode default)\n`;
      helpText += `- query_next() - Get next solution\n`;
      helpText += `- query_close() - Close query session\n\n`;
      helpText += `**Meta**:\n`;
      helpText += `- help(topic?) - This help (topics: overview, tools, prompts, modes, security, examples)\n\n`;
    }

    if (!topic || topic === 'modes') {
      helpText += `## Query Modes\n\n`;
      helpText += `**Engine Mode (default)**:\n`;
      helpText += `- True backtracking with SWI-Prolog engines\n`;
      helpText += `- Unlimited solutions, memory-efficient\n`;
      helpText += `- Best for: constraint solving, exploring solution spaces, complex searches\n`;
      helpText += `- Usage: query_start({query: "..."}) or query_start({query: "...", use_engine: true})\n\n`;
      helpText += `**Standard Mode**:\n`;
      helpText += `- Pagination via call_nth/2\n`;
      helpText += `- Predictable step-through, good for debugging\n`;
      helpText += `- Best for: simple queries, educational use, when deterministic order matters\n`;
      helpText += `- Usage: query_start({query: "...", use_engine: false})\n\n`;
    }

    if (!topic || topic === 'security') {
      helpText += `## Security Model\n\n`;
      helpText += `All code executes in sandboxed 'knowledge_base' module.\n\n`;
      helpText += `**Blocked**: File I/O (except via tools), network access, system calls, shell commands\n`;
      helpText += `**Allowed**: assert, retract, query, constraints, safe library predicates\n\n`;
      helpText += `**Pre-loaded Libraries**: clpfd (constraints), lists, between, pairs, ordsets\n`;
      helpText += `Additional libraries can be configured via --kb-libraries CLI flag or KB_LIBRARIES env var.\n\n`;
      helpText += `**File Operations**: Restricted to configured root directories.\n\n`;
    }

    if (!topic || topic === 'prompts') {
      helpText += `## Expert Prompts\n\n`;
      helpText += `Use MCP prompts for guided workflows (access via MCP prompt system):\n`;
      helpText += `- expert(mode?) - Prolog programming assistance with quick reference\n`;
      helpText += `  - Modes: 'init' (comprehensive), 'quick' (concise)\n`;
      helpText += `- knowledge(mode?) - Knowledge base design and analysis\n`;
      helpText += `  - Modes: 'builder' (design), 'analyzer' (analysis)\n`;
      helpText += `- optimize - Query optimization strategies and performance\n`;
      helpText += `- puzzle - Logic puzzle solving with constraint programming\n\n`;
    }

    if (!topic || topic === 'examples') {
      helpText += `## Quick Examples\n\n`;
      helpText += `**Assert facts/rules**:\n`;
      helpText += `  clauses({operation: 'assert', clauses: 'parent(john, mary).'})\n`;
      helpText += `  clauses({operation: 'assert', clauses: ['parent(a,b)', 'parent(b,c)']})\n`;
      helpText += `  clauses({operation: 'assert', clauses: 'ancestor(X,Y) :- parent(X,Y).'})\n\n`;
      helpText += `**Query**:\n`;
      helpText += `  query_start({query: 'parent(X, mary)'})\n`;
      helpText += `  query_next()\n`;
      helpText += `  query_close()\n\n`;
      helpText += `**Constraints** (clpfd pre-loaded):\n`;
      helpText += `  query_start({query: 'X #> 5, X #< 10, label([X])'})\n`;
      helpText += `  query_next()  % Gets X=6, X=7, X=8, X=9\n\n`;
      helpText += `**Import/manage files**:\n`;
      helpText += `  files({operation: 'import', filename: '/path/to/facts.pl'})\n`;
      helpText += `  files({operation: 'list'})  % Show all imports\n`;
      helpText += `  files({operation: 'unimport', filename: '/path/to/facts.pl'})\n\n`;
      helpText += `**Workspace operations**:\n`;
      helpText += `  workspace({operation: 'snapshot'})  % Export all clauses (source preserved)\n`;
      helpText += `  workspace({operation: 'list_symbols'})  % Show predicates\n`;
      helpText += `  workspace({operation: 'reset'})  % Clear everything\n\n`;
      helpText += `**Retract**:\n`;
      helpText += `  clauses({operation: 'retract', clauses: 'parent(john, mary).'})\n`;
      helpText += `  clauses({operation: 'retract', clauses: ['parent(a,b)', 'parent(b,c)']})\n\n`;
    }

    return {
      content: [{ type: 'text', text: helpText.trim() }]
    };
  },
},

// DELETE capabilities tool handler
```

---

### 7. Update Resources

**File**: `plugins/server/prolog/src/resources.ts`

**Update existing resources**:

```typescript
// OLD: prolog://knowledge_base/dump
// NEW: prolog://workspace/snapshot
{
  uri: 'prolog://workspace/snapshot',
  name: 'Workspace Snapshot',
  description: 'Current knowledge base state with preserved source formatting (synced with workspace tool)',
  mimeType: 'text/x-prolog',
  handler: async () => {
    await prologInterface.start();
    const dump = await prologInterface.getSnapshot(); // Same method as tool!

    return {
      contents: [{
        uri: 'prolog://workspace/snapshot',
        mimeType: 'text/x-prolog',
        text: dump || '% Empty workspace'
      }]
    };
  }
},

// OLD: prolog://knowledge_base/predicates
// NEW: prolog://workspace/symbols
{
  uri: 'prolog://workspace/symbols',
  name: 'Workspace Symbols',
  description: 'List of predicates in knowledge base (synced with workspace list_symbols)',
  mimeType: 'text/plain',
  handler: async () => {
    await prologInterface.start();
    const result = await prologInterface.query('list_knowledge_base_predicates');

    // Parse predicates (same logic as tool)
    const lines = result.split('\n');
    const predicates: string[] = [];

    for (const line of lines) {
      const trimmed = line.trim();
      if (trimmed && !trimmed.startsWith('%') && !trimmed.startsWith('?-')) {
        predicates.push(trimmed);
      }
    }

    return {
      contents: [{
        uri: 'prolog://workspace/symbols',
        mimeType: 'text/plain',
        text: predicates.join('\n') || '% No predicates defined'
      }]
    };
  }
},

// KEEP: reference://help (synced with help tool)
{
  uri: 'reference://help',
  name: 'Help',
  description: 'Usage guidelines and tool reference',
  mimeType: 'text/markdown',
  handler: async () => {
    // Could call help tool internally or duplicate logic
    const helpContent = `[Generate same content as help tool with no topic]`;

    return {
      contents: [{
        uri: 'reference://help',
        mimeType: 'text/markdown',
        text: helpContent
      }]
    };
  }
},

// KEEP: reference://license
// KEEP: reference://logo

// DELETE: reference://capabilities (merged into help)
```

---

### 8. Remove Library Loading Tool

**File**: `plugins/server/prolog/src/tools.ts`

```typescript
// DELETE knowledge_base_load_library tool handler entirely
```

**File**: `plugins/server/prolog/src/schemas.ts`

```typescript
// DELETE knowledgeBaseLoadLibrarySchema
```

---

### 9. Add Library Pre-Import

**File**: `plugins/server/prolog/prolog/prolog_server.pl`

**Update `ensure_knowledge_base_module/0` predicate** (around line 554):

```prolog
ensure_knowledge_base_module :-
    % Create knowledge_base module if needed
    ( current_module(knowledge_base) -> true ; create_module(knowledge_base) ),
    catch(set_prolog_flag(knowledge_base:unknown, fail), _, true),

    % Load safe libraries into prolog_server context
    catch(use_module(library(lists)), _, true),
    catch(use_module(library(between)), _, true),
    catch(use_module(library(apply)), _, true),
    catch(use_module(library(pairs)), _, true),
    catch(use_module(library(ordsets)), _, true),

    % Import predicates into knowledge_base module
    catch(knowledge_base:use_module(library(lists), [
        member/2, append/3, length/2, select/3, nth0/3, nth1/3,
        permutation/2, reverse/2, memberchk/2, last/2, flatten/2,
        sum_list/2, max_list/2, min_list/2, numlist/3, subtract/3,
        union/3, intersection/3, list_to_set/2, is_set/1, subset/2,
        delete/3, selectchk/3
    ]), _, true),
    catch(knowledge_base:use_module(library(between), [between/3]), _, true),
    catch(knowledge_base:use_module(library(pairs), [
        pairs_keys_values/3, pairs_keys/2, pairs_values/2,
        group_pairs_by_key/2, transpose_pairs/2
    ]), _, true),
    catch(knowledge_base:use_module(library(ordsets), [
        ord_union/3, ord_intersection/3, ord_subtract/3,
        ord_memberchk/2, list_to_ord_set/2
    ]), _, true),

    % Load library(clpfd) by default for constraint solving
    % Available in both prolog_server (for parsing) and knowledge_base (for execution)
    catch(use_module(library(clpfd)), _, true),
    catch(knowledge_base:use_module(library(clpfd)), _, true),

    % Load additional libraries from config (if specified)
    load_configured_libraries,

    true.

% Load libraries specified via CLI or environment
load_configured_libraries :-
    % Check for KB_LIBRARIES environment variable
    ( getenv('KB_LIBRARIES', LibsString) ->
        split_string(LibsString, ",", " ", LibNames),
        maplist(safe_load_configured_library, LibNames)
    ; true
    ).

safe_load_configured_library(LibName) :-
    atom_string(LibAtom, LibName),
    ( library_safe_to_load(LibAtom) ->
        catch(use_module(library(LibAtom)), E, (
            format(user_error, 'Warning: Failed to load library ~w: ~w~n', [LibAtom, E])
        )),
        catch(knowledge_base:use_module(library(LibAtom)), E2, (
            format(user_error, 'Warning: Failed to import library ~w into knowledge_base: ~w~n', [LibAtom, E2])
        ))
    ; format(user_error, 'Warning: Library ~w not in safe list, skipping~n', [LibAtom])
    ).
```

---

### 10. Add CLI Configuration

**File**: `products/swipl-mcp-server/src/index.ts`

**Add CLI flag parsing**:

```typescript
import { parseArgs } from 'util';

// Parse CLI arguments
const { values } = parseArgs({
  options: {
    'kb-libraries': {
      type: 'string',
      short: 'l',
      default: ''
    },
    // ... other existing options
  },
  allowPositionalArgs: true
});

// Set environment variable for Prolog to read
if (values['kb-libraries']) {
  process.env.KB_LIBRARIES = values['kb-libraries'];
}
```

**Update README** with usage example:

```bash
# Default (clpfd pre-loaded)
npx @vpursuit/swipl-mcp-server

# With additional libraries
npx @vpursuit/swipl-mcp-server --kb-libraries=clpr,clpb,aggregate

# Via environment
KB_LIBRARIES=clpr,clpb npx @vpursuit/swipl-mcp-server
```

---

### 11. Update Exports

**File**: `plugins/server/prolog/src/index.ts`

**Update tool exports** (around line 68):

```typescript
// OLD (15 tools)
export const tools = [
  'knowledge_base_assert',
  'knowledge_base_assert_many',
  'knowledge_base_retract',
  'knowledge_base_retract_many',
  'knowledge_base_load',
  'knowledge_base_load_library',
  'knowledge_base_dump',
  'knowledge_base_clear',
  'query_start',
  'query_startEngine',
  'query_next',
  'query_close',
  'symbols_list',
  'capabilities',
  'help',
  'license',
];

// NEW (7 tools)
export const tools = [
  'clauses',
  'files',
  'workspace',
  'query_start',
  'query_next',
  'query_close',
  'help',
];
```

**Update resource exports**:

```typescript
// OLD
export const resources = [
  'prolog://knowledge_base/predicates',
  'prolog://knowledge_base/dump',
  'reference://help',
  'reference://license',
  'reference://logo',
  'reference://capabilities',
];

// NEW
export const resources = [
  'prolog://workspace/symbols',
  'prolog://workspace/snapshot',
  'reference://help',
  'reference://license',
  'reference://logo',
];
```

**Update documentation comments** at top of file (lines 48-89).

---

### 12. Update Prompts

**File**: `plugins/server/prolog/src/prompts.ts`

**Find and replace** references to old tool names:

- `reference://capabilities` → Reference help tool instead
- `knowledge_base_assert_many` → `clauses({operation: 'assert', clauses: [...]})`
- `knowledge_base_retract_many` → `clauses({operation: 'retract', clauses: [...]})`
- `knowledge_base_load` → `files({operation: 'import', filename: ...})`
- `knowledge_base_dump` → `workspace({operation: 'snapshot'})`
- `knowledge_base_clear` → `workspace({operation: 'reset'})`
- `symbols_list` → `workspace({operation: 'list_symbols'})`
- `query_startEngine` → `query_start({query: ..., use_engine: true})`
- `capabilities` → `help({topic: 'tools'})`

**Example updates** (lines 53, 183, 370, 530):

```typescript
// OLD
- Read reference://capabilities resource

// NEW
- Use help tool with topic parameter for focused queries: help({topic: 'tools'})
```

---

## Migration Guide

### Test File Updates

**Pattern: Assert**

```typescript
// BEFORE
await toolHandlers.knowledgeBaseAssert({ fact: 'parent(john, mary)' });
await toolHandlers.knowledgeBaseAssertMany({
  facts: ['parent(a,b)', 'parent(b,c)']
});

// AFTER
await toolHandlers.clauses({
  operation: 'assert',
  clauses: 'parent(john, mary)'
});
await toolHandlers.clauses({
  operation: 'assert',
  clauses: ['parent(a,b)', 'parent(b,c)']
});
```

**Pattern: Retract**

```typescript
// BEFORE
await toolHandlers.knowledgeBaseRetract({ fact: 'parent(john, mary)' });
await toolHandlers.knowledgeBaseRetractMany({
  facts: ['parent(a,b)', 'parent(b,c)']
});

// AFTER
await toolHandlers.clauses({
  operation: 'retract',
  clauses: 'parent(john, mary)'
});
await toolHandlers.clauses({
  operation: 'retract',
  clauses: ['parent(a,b)', 'parent(b,c)']
});
```

**Pattern: Files**

```typescript
// BEFORE
await toolHandlers.knowledgeBaseLoad({ filename: '/path/to/file.pl' });

// AFTER
await toolHandlers.files({
  operation: 'import',
  filename: '/path/to/file.pl'
});

// NEW (unimport)
await toolHandlers.files({
  operation: 'unimport',
  filename: '/path/to/file.pl'
});

// NEW (list)
await toolHandlers.files({ operation: 'list' });
```

**Pattern: Workspace**

```typescript
// BEFORE
const dump = await toolHandlers.knowledgeBaseDump({});
await toolHandlers.knowledgeBaseClear({});
const symbols = await toolHandlers.symbolsList({});

// AFTER
const dump = await toolHandlers.workspace({ operation: 'snapshot' });
await toolHandlers.workspace({ operation: 'reset' });
const symbols = await toolHandlers.workspace({ operation: 'list_symbols' });
```

**Pattern: Query**

```typescript
// BEFORE
await toolHandlers.queryStartEngine({ query: 'member(X, [1,2,3])' });
await toolHandlers.queryStart({ query: 'member(X, [1,2,3])' });

// AFTER (engine mode is default)
await toolHandlers.queryStart({ query: 'member(X, [1,2,3])' });
// OR explicitly
await toolHandlers.queryStart({ query: 'member(X, [1,2,3])', use_engine: true });

// Standard mode
await toolHandlers.queryStart({ query: 'member(X, [1,2,3])', use_engine: false });
```

**Pattern: Help**

```typescript
// BEFORE
const caps = await toolHandlers.capabilities({});
const help = await toolHandlers.help({ topic: 'examples' });

// AFTER
const help = await toolHandlers.help({}); // Full help
const helpTools = await toolHandlers.help({ topic: 'tools' }); // Focused
```

**Pattern: Libraries**

```typescript
// BEFORE
await toolHandlers.knowledgeBaseLoadLibrary({ library: 'clpfd' });
await toolHandlers.queryStart({ query: 'X #> 5' });

// AFTER (clpfd pre-loaded)
await toolHandlers.queryStart({ query: 'X #> 5' });
// No explicit load needed!
```

---

## Implementation Order

Recommended sequence to minimize breakage and enable incremental testing:

1. **Add source storage infrastructure** (PrologInterface.ts)
   - Add Map data structure and all helper methods
   - No tools use it yet (non-breaking)
   - Add unit tests for source storage operations

2. **Add library pre-import** (prolog_server.pl)
   - Add clpfd default loading
   - Add CLI config support
   - Independent change, easy to test

3. **Implement `clauses` tool** (replaces assert/retract)
   - Add schema and handler
   - Keep old tools temporarily
   - Update subset of tests to use new tool
   - Verify source preservation works

4. **Implement `files` tool** (replaces load, adds unimport/list)
   - Add schema and handler
   - Keep old tool temporarily
   - Update subset of tests
   - Verify file provenance tracking

5. **Implement `workspace` tool** (replaces dump/clear/symbols_list)
   - Add schema and handler
   - Update resources to use new URIs
   - Keep old tools temporarily
   - Update subset of tests
   - Verify resource sync works

6. **Update `query_start` tool** (merge with startEngine)
   - Update schema and handler
   - Keep old tool temporarily
   - Update subset of tests

7. **Update `help` tool** (merge with capabilities)
   - Update schema and handler
   - Keep old tool temporarily
   - Update subset of tests
   - Update prompt references

8. **Remove all old tools**
   - Delete handlers and schemas
   - Delete from exports
   - Update all remaining tests
   - Update all prompt files
   - Update documentation

9. **Final integration testing**
   - Run full test suite (324+ tests)
   - Verify source preservation
   - Verify file import/unimport/list
   - Verify resource sync
   - Check token counts
   - Performance benchmarks

---

## Testing Checklist

### Source Storage Tests

- [ ] Assert fact, snapshot shows exact input (variable names preserved)
- [ ] Assert fact with variables X, Y - snapshot keeps X, Y (not A, B)
- [ ] Assert multiple facts, snapshot shows all in insertion order
- [ ] Retract fact, snapshot no longer shows it
- [ ] Retract specific fact from multiple identical, only one removed
- [ ] Reset clears source storage and Prolog KB
- [ ] Assert failure doesn't add to source storage (compiled=false)
- [ ] Source storage survives Prolog errors

### File Operations Tests

- [ ] Import file, clauses added with file provenance
- [ ] Import file, snapshot shows clauses with original variable names
- [ ] Import duplicate file, get error (already imported)
- [ ] Import file, then import different file, both tracked
- [ ] List imports, shows correct metadata (path, count, timestamp)
- [ ] List imports when empty, returns empty array
- [ ] Unimport file, only that file's clauses removed
- [ ] Unimport file, snapshot no longer shows those clauses
- [ ] Unimport nonexistent file, get error
- [ ] Import file with parse errors, partial import with errors reported

### Tool Consolidation Tests

- [ ] clauses(assert) with single clause works
- [ ] clauses(assert) with array of clauses works
- [ ] clauses(assert) with empty array, get error
- [ ] clauses(retract) with single clause works
- [ ] clauses(retract) with array of clauses works
- [ ] clauses(retract) non-existent clause, get error outcome
- [ ] workspace(snapshot) returns source text
- [ ] workspace(snapshot) when empty, returns empty or message
- [ ] workspace(reset) clears everything
- [ ] workspace(list_symbols) shows predicates
- [ ] workspace(list_symbols) when empty, returns empty list
- [ ] files(list) shows imported files
- [ ] files(import) without filename, get error
- [ ] files(unimport) without filename, get error
- [ ] query_start defaults to engine mode
- [ ] query_start use_engine=true uses engine mode
- [ ] query_start use_engine=false uses standard mode
- [ ] help with no topic shows overview
- [ ] help with each topic shows focused content

### Resource Sync Tests

- [ ] prolog://workspace/snapshot matches workspace(snapshot)
- [ ] prolog://workspace/symbols matches workspace(list_symbols)
- [ ] Resource snapshot updates after assert
- [ ] Resource snapshot updates after retract
- [ ] Resource snapshot clears after reset
- [ ] Resource symbols updates after assert new predicate
- [ ] reference://help returns help content

### Library Tests

- [ ] clpfd available without explicit loading
- [ ] Query with #> operator works immediately
- [ ] Query with #< operator works immediately
- [ ] Query with ins, label works immediately
- [ ] KB_LIBRARIES env var loads additional libraries
- [ ] KB_LIBRARIES with invalid library shows warning, doesn't crash
- [ ] KB_LIBRARIES with comma-separated list loads all

### Integration Tests

- [ ] All existing tests pass with updated tool names
- [ ] No performance regressions (benchmark)
- [ ] Token counts reduced (~50%)
- [ ] Error messages clear and helpful
- [ ] Complex workflow: import file, add clauses, query, snapshot, unimport, verify
- [ ] Memory usage reasonable with large KB (test with 10k+ clauses)

### Edge Cases

- [ ] Assert clause with trailing period
- [ ] Assert clause without trailing period
- [ ] Assert rule with :- operator
- [ ] Assert fact with special characters (quotes, etc.)
- [ ] Retract with/without period (both work)
- [ ] Import file with comments (comments skipped)
- [ ] Import file with directives (handled appropriately)
- [ ] Snapshot with mix of inline and file-imported clauses
- [ ] Unimport one file while other files remain

---

## Expected Metrics

### Before

- **Total tools**: 15
- **Token count per tool listing**: ~1200 tokens (estimate)
- **Common workflows**: 3-5 tool calls (including setup steps)
- **Source preservation**: None (variable names lost)
- **File operations**: Load only, no unload
- **Dump output**: Canonical Prolog (not original)

### After

- **Total tools**: 7 (53% reduction!)
- **Token count per tool listing**: ~550 tokens (estimate, 54% reduction)
- **Common workflows**: 2-3 tool calls (less setup)
- **Source preservation**: Complete (variable names kept)
- **File operations**: Import, unimport, list with provenance
- **Snapshot output**: Original source text

### Token Savings Calculation

- **Tool listing reduction**: 15 → 7 tools
- **Per-request savings**: ~650 tokens (54% of tool listing overhead)
- **For 100 requests/session**: ~65,000 tokens saved
- **Additional savings**: Better descriptions reduce follow-up clarifications

### Performance

- **Source storage overhead**: Minimal (Map lookup is O(1))
- **Memory overhead**: ~2x (source + compiled), acceptable trade-off
- **Assert performance**: Negligible difference (UUID + Map.set is fast)
- **Snapshot performance**: O(n) where n = clause count, faster than Prolog clause/2

---

## Risk Mitigation

### Breaking Changes

**All tool renames are breaking changes** for external clients.

**Mitigations**:
1. Version bump (major version increment, e.g., 1.x → 2.0)
2. Clear migration guide (above)
3. Announce in release notes with examples
4. Consider keeping old tools as deprecated aliases initially (if needed for transition)

### Source Storage Risks

**Memory growth**: Map grows with assertions, never shrinks except on reset.

**Mitigations**:
1. Add max size limit (e.g., MAX_SOURCE_ENTRIES = 10000)
2. Implement LRU eviction when limit reached
3. Document memory characteristics
4. Consider persistence layer for long-running sessions

**Example max size implementation**:
```typescript
if (this.kbSourceStorage.size >= MAX_SOURCE_ENTRIES) {
  // Remove oldest entry
  const oldestId = Array.from(this.kbSourceStorage.entries())
    .sort(([,a], [,b]) => a.timestamp - b.timestamp)[0][0];
  this.kbSourceStorage.delete(oldestId);
}
```

### File Provenance Risks

**File changes**: What if file contents change between import/unimport?

**Mitigations**:
1. Track by absolute path (canonical path resolution)
2. Warn if file modified since import (optional: check mtime)
3. Document that unimport removes by tracked clauses, not re-parsing file

### Implementation Risks

**Complex refactoring**: Many files, many tests, high chance of regression.

**Mitigations**:
1. Incremental implementation (as per "Implementation Order")
2. Test after each step
3. Keep old tools as deprecated aliases during transition
4. Comprehensive test coverage before removing old tools
5. Code review focused on edge cases

---

## Success Criteria

- [ ] All 324+ existing tests pass (with updated tool names)
- [ ] Token count reduced by ~50% in tool listings
- [ ] Source storage preserves variable names exactly
- [ ] File import/unimport/list works correctly
- [ ] Snapshot matches input (not compiled Prolog)
- [ ] Resources synced with tools (snapshot, symbols)
- [ ] clpfd works without explicit loading
- [ ] All operation enums work correctly
- [ ] No performance regressions (benchmark comparison)
- [ ] Documentation complete and accurate
- [ ] Migration guide tested with real code
- [ ] Memory usage reasonable (tested with large KB)

---

## Future Considerations

1. **Persistence**: Save source storage to disk for session recovery
   - Could use SQLite or JSON file
   - Restore on server restart

2. **Provenance queries**: Query which file/operation added a predicate
   - Add tool: `workspace(operation: 'provenance', clause: string)`
   - Returns: file path, timestamp, type

3. **Diff support**: Show what changed between snapshots
   - Track snapshots with IDs
   - Compare two snapshot IDs

4. **More default libraries**: Consider aggregate, dcg_basics, etc.
   - Evaluate usage patterns
   - Add to pre-import if commonly used

5. **Tool aliasing**: Temporary backward-compatible aliases for migration
   - Keep old tool names as deprecated wrappers
   - Log warnings when used
   - Remove in next major version

6. **Variable name extraction**: Improve file parsing to use Prolog's `variable_names` option
   - More robust than text parsing
   - Handles complex clauses correctly

7. **Undo/redo**: Stack-based operation history
   - Track operations with ability to undo
   - Limited history size

8. **Query consolidation**: Could query tools also consolidate?
   - Probably not needed - 3 query tools is reasonable
   - Clear separation: start, next, close

---

## Files Summary

### Modified Files

- **`plugins/server/prolog/src/PrologInterface.ts`**
  - Add source storage Map
  - Add helper methods (20+ new methods)
  - ~500 lines added

- **`plugins/server/prolog/src/tools.ts`**
  - Delete 10 old tool handlers
  - Add 3 new tool handlers (clauses, files, workspace)
  - Update 2 tool handlers (query_start, help)
  - Net change: ~300 lines removed, ~400 added

- **`plugins/server/prolog/src/schemas.ts`**
  - Delete 10 old schemas
  - Add 3 new schemas
  - Update 2 schemas
  - Net change: ~100 lines removed, ~50 added

- **`plugins/server/prolog/src/resources.ts`**
  - Update 2 resource URIs
  - Delete 1 resource (capabilities)
  - Update resource handlers
  - ~30 lines modified

- **`plugins/server/prolog/src/index.ts`**
  - Update tool exports (15 → 7)
  - Update resource exports (6 → 5)
  - Update documentation comments
  - ~50 lines modified

- **`plugins/server/prolog/src/prompts.ts`**
  - Update tool references (4 locations)
  - Update examples
  - ~20 lines modified

- **`plugins/server/prolog/prolog/prolog_server.pl`**
  - Add clpfd pre-import
  - Add CLI/env library loading
  - Add helper predicate for configured libraries
  - ~30 lines added

- **`products/swipl-mcp-server/src/index.ts`**
  - Add CLI flag parsing
  - Add KB_LIBRARIES env support
  - ~20 lines added

- **Test files** (~50+ files)
  - Update tool names throughout
  - Add new tests for source storage
  - Add new tests for file operations
  - ~150+ references updated

- **Documentation files**
  - README.md - Update tool listings and examples
  - TRANSFORMATION_GUIDE.md - Update references
  - Other docs - Update tool names

### Deleted Components

- `licenseSchema` (tool, not resource)
- `license` tool handler
- `knowledgeBaseAssertManySchema`
- `knowledge_base_assert_many` tool handler
- `knowledgeBaseRetractManySchema`
- `knowledge_base_retract_many` tool handler
- `knowledgeBaseLoadSchema`
- `knowledge_base_load` tool handler
- `knowledgeBaseLoadLibrarySchema`
- `knowledge_base_load_library` tool handler
- `knowledgeBaseDumpSchema`
- `knowledge_base_dump` tool handler
- `knowledgeBaseClearSchema`
- `knowledge_base_clear` tool handler
- `symbolsListSchema`
- `symbols_list` tool handler
- `queryStartEngineSchema`
- `query_startEngine` tool handler
- `capabilitiesSchema`
- `capabilities` tool handler
- `reference://capabilities` resource

### New Components

- `SourceEntry` interface
- `kbSourceStorage` Map in PrologInterface
- `assertClauseWithSource()` method
- `retractClauseWithSource()` method
- `importFileWithSource()` method
- `unimportFile()` method
- `getSnapshot()` method
- `getImportedFiles()` method
- `clearWorkspaceWithSource()` method
- `parseFileToStringArray()` method
- `clausesSchema` and `clauses` tool
- `filesSchema` and `files` tool
- `workspaceSchema` and `workspace` tool
- CLI flag `--kb-libraries`
- Environment variable `KB_LIBRARIES` support
- Prolog helper `load_configured_libraries/0`

---

## References

- Current tool implementations: `plugins/server/prolog/src/tools.ts`
- Current schemas: `plugins/server/prolog/src/schemas.ts`
- Prolog interface: `plugins/server/prolog/src/PrologInterface.ts`
- Prolog server: `plugins/server/prolog/prolog/prolog_server.pl`
- Resources: `plugins/server/prolog/src/resources.ts`
- Test suite: `plugins/server/prolog/test/`
- Product entry: `products/swipl-mcp-server/src/index.ts`
- Repository structure: `CLAUDE.md`

---

## Glossary

- **Source storage**: TypeScript Map that preserves original clause text with variable names
- **Compiled KB**: Prolog's internal clause database (variable names normalized)
- **Provenance**: Metadata tracking which file/operation added a clause
- **Snapshot**: Export of source storage (preserves original formatting)
- **Operation enum**: Parameter that selects between related operations (e.g., 'assert'|'retract')
- **Engine mode**: SWI-Prolog's engine-based backtracking (true search)
- **Standard mode**: call_nth/2 pagination (simpler, deterministic)
- **Resource sync**: Tool and resource returning same data from shared implementation

---

**End of Implementation Plan**
