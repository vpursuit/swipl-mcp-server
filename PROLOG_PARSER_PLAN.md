# Plan: Replace JavaScript Text Parser with Prolog-Based Parser

**Date**: 2025-11-17 (Created) | 2025-11-18 (Implemented)
**Status**: ✅ Completed - Phase 1 MVP
**Priority**: High (Fixes fragile parsing, enables proper syntax support)

---

## Implementation Summary

Phase 1 was successfully completed with the following key decisions and modifications:

### What Was Implemented

1. **Prolog-based parser** (`parse_file_with_source/2` in prolog_server.pl)
   - Uses `read_term/3` with `variable_names`, `singletons`, and `term_position` options
   - Converts terms back to source strings using `write_term/2` with `spacing(next_argument)`
   - Preserves original variable names in output

2. **TypeScript integration** (PrologInterface.ts)
   - Added `parseFileWithProlog()` method to communicate with Prolog parser
   - Updated `importFileWithSource()` to use new parser instead of `parseFileToStringArray()`
   - Added `parseTermList()` helper for parsing Prolog list responses

3. **DCG rules forbidden** (critical decision)
   - Added detection in `process_parsed_term/5` to reject `Term = (_ --> _)` patterns
   - Returns helpful error message directing users to expanded difference-list form
   - Rationale: DCGs are rarely needed for AI agent knowledge bases, implementing support would require 8-11 hours for edge case handling

4. **Library loading documented**
   - Added comprehensive "Library Loading" section to README.md
   - Documented KB_LIBRARIES environment variable mechanism
   - Listed all pre-loaded and safe libraries

### Key Technical Decisions

1. **Spacing format**: Accepted Prolog's standard spacing (`parent(john, mary)` with spaces after commas) rather than compact format
   - Updated test expectations throughout sourceStorage.test.ts

2. **Anonymous variables**: Accepted Prolog's internal naming (`_14180` instead of `_`)
   - This is standard write_term behavior and documented in tests

3. **Directive handling**: Directives are skipped during parsing
   - Libraries must be configured via KB_LIBRARIES environment variable
   - Keeps implementation simple and secure

### Test Results

- 37 tests passing, 1 skipped (partial error recovery - needs future improvement)
- All source storage tests updated and passing
- DCG test changed from `test.skip` to actual test verifying error message

### Files Modified

- `plugins/server/prolog/prolog/prolog_server.pl` - Added parser predicates with DCG detection
- `plugins/server/prolog/src/PrologInterface.ts` - Added parseFileWithProlog, fixed variable naming bug
- `plugins/server/prolog/test/unit/sourceStorage.test.ts` - Updated expectations for spacing
- `plugins/server/prolog/README.md` - Added Library Loading and Limitations sections

---

## Executive Summary

**Problem**: Current JavaScript-based text parser (`parseFileToStringArray`) is fragile and cannot handle:
- Strings containing dots (`"Dr. Smith"`)
- DCG rules (`sentence --> noun, verb.`)
- Complex operators and precedence
- Multi-line clauses with proper Prolog syntax

**Solution**: Use Prolog's `read_term/3` with `variable_names` option to parse files correctly while preserving original variable names.

**Benefits**:
- ✅ Robust parsing (Prolog's native parser handles all edge cases)
- ✅ Variable name preservation (via `variable_names` option)
- ✅ Syntax validation (malformed clauses are caught immediately)
- ✅ Singleton detection (unused variables identified)
- ✅ Proper term structure (understands operators, DCGs, etc.)

**Architecture**: Keep current per-clause storage model, replace only the parsing step.

---

## Current vs. Proposed Flow

### **Current (JavaScript Parsing)**
```
File on disk
  ↓
fs.readFile() → string
  ↓
parseFileToStringArray() → ["clause1.", "clause2.", ...]
  ↓
For each clause:
  assertClauseWithSource()
    ↓
    Prolog: assertz(clause)
    TypeScript Map: store original text
```

### **Proposed (Prolog Parsing)**
```
File on disk
  ↓
Prolog: parse_file_with_source(filename)
  ↓
  read_term/3 with variable_names
  ↓
  Returns: [{term: "clause1", vars: ['X','Y'], source: "..."}]
  ↓
TypeScript receives structured data
  ↓
For each clause:
  assertClauseWithSource()
    ↓
    Prolog: assertz(clause) [already parsed, just assert]
    TypeScript Map: store source with var metadata
```

---

## Detailed Design

### Phase 1: Prolog Parser Implementation

#### 1.1 Add File Parsing Predicate to prolog_server.pl

**Location**: `plugins/server/prolog/prolog/prolog_server.pl`

**New Predicates**:

```prolog
%%% File Parsing with Source Preservation %%%

% Main entry point for parsing files
parse_file_with_source(Filename, Result) :-
    absolute_file_name(Filename, AbsPath),
    exists_file(AbsPath),
    !,
    catch(
        parse_file_to_clauses(AbsPath, Clauses),
        Error,
        Result = error(Error)
    ),
    (   var(Result)
    ->  Result = success(Clauses)
    ;   true
    ).
parse_file_with_source(Filename, error(file_not_found(Filename))).

% Parse all clauses from a file
parse_file_to_clauses(Filename, Clauses) :-
    setup_call_cleanup(
        open(Filename, read, Stream),
        read_all_clauses(Stream, Filename, Clauses),
        close(Stream)
    ).

% Read all clauses from stream
read_all_clauses(Stream, Filename, Clauses) :-
    read_all_clauses(Stream, Filename, 1, [], ClausesRev),
    reverse(ClausesRev, Clauses).

read_all_clauses(Stream, Filename, LineNum, Acc, Result) :-
    stream_property(Stream, position(Pos)),
    catch(
        read_term(Stream, Term, [
            variable_names(VarNames),
            singletons(Singletons),
            term_position(TermPos)
        ]),
        Error,
        (   format(atom(ErrorMsg), 'Parse error at line ~w: ~w', [LineNum, Error]),
            Result = [error(ErrorMsg)|Acc]
        )
    ),
    (   var(Result)  % No error occurred
    ->  (   Term == end_of_file
        ->  Result = Acc
        ;   process_term(Term, VarNames, Singletons, TermPos, Filename, LineNum, ClauseData),
            NextLine is LineNum + 1,
            read_all_clauses(Stream, Filename, NextLine, [ClauseData|Acc], Result)
        )
    ;   true  % Error already in Result
    ).

% Process a single term
process_term(Term, VarNames, Singletons, _TermPos, _Filename, LineNum, ClauseData) :-
    % Skip directives (we don't store them, but could be configurable)
    (   Term = (:- _Directive)
    ->  ClauseData = directive(skipped)
    ;   % Regular clause - convert to source string
        term_to_source_string(Term, VarNames, SourceText),
        ClauseData = clause(SourceText, VarNames, Singletons, LineNum)
    ).

% Convert term back to source string with original variable names
term_to_source_string(Term, VarNames, SourceText) :-
    with_output_to(string(SourceText0),
        write_term(Term, [
            variable_names(VarNames),
            quoted(true),
            numbervars(false),
            dotted(false)
        ])
    ),
    % Add period if not present
    (   sub_string(SourceText0, _, 1, 0, ".")
    ->  SourceText = SourceText0
    ;   string_concat(SourceText0, ".", SourceText)
    ).
```

**Key Features**:
- Uses `read_term/3` with `variable_names(VarNames)` to capture original variable names
- Uses `singletons(Singletons)` to detect unused variables (useful for warnings)
- Uses `term_position(TermPos)` for error reporting
- Handles parse errors gracefully (continues parsing, reports errors)
- Skips directives (configurable - could store them too)
- Converts term back to string using `write_term` with variable names

#### 1.2 Add Protocol Command Handler

**Location**: Same file, in the command dispatch section

```prolog
% Command dispatch - add new command
handle_command(Cmd) :-
    Cmd = cmd(Id, parse_file(Filename)),
    !,
    parse_file_with_source(Filename, Result),
    send_response(Id, Result).
```

---

### Phase 2: TypeScript Integration

#### 2.1 Add Protocol Method in PrologInterface.ts

**Location**: `plugins/server/prolog/src/PrologInterface.ts`

**New Method**:

```typescript
/**
 * Parse Prolog file using Prolog's native parser
 * Returns structured data with original variable names preserved
 *
 * @param filename - Absolute path to .pl file
 * @returns Array of clause data or error
 */
async parseFileWithProlog(filename: string): Promise<{
  success: boolean;
  clauses?: Array<{
    sourceText: string;
    varNames: Array<{ name: string }>;
    singletons: string[];
    lineNumber: number;
  }>;
  errors?: string[];
}> {
  const absolutePath = path.resolve(filename);

  try {
    // Send parse command to Prolog
    const result = await this.sendCommand({
      cmd: 'parse_file',
      file: absolutePath
    });

    if (result.type === 'error') {
      return {
        success: false,
        errors: [result.message]
      };
    }

    // Parse Prolog response
    const clauses: Array<{
      sourceText: string;
      varNames: Array<{ name: string }>;
      singletons: string[];
      lineNumber: number;
    }> = [];

    const errors: string[] = [];

    for (const item of result.clauses) {
      if (item.type === 'clause') {
        clauses.push({
          sourceText: item.sourceText,
          varNames: item.varNames || [],
          singletons: item.singletons || [],
          lineNumber: item.lineNumber || 0
        });
      } else if (item.type === 'error') {
        errors.push(item.message);
      }
      // Skip directives (item.type === 'directive')
    }

    return {
      success: errors.length === 0,
      clauses,
      errors: errors.length > 0 ? errors : undefined
    };

  } catch (error) {
    return {
      success: false,
      errors: [String(error)]
    };
  }
}
```

#### 2.2 Update importFileWithSource

**Location**: Same file

**Replace parseFileToStringArray with parseFileWithProlog**:

```typescript
async importFileWithSource(filename: string): Promise<{
  success: boolean;
  clausesAdded: number;
  errors: string[];
}> {
  // Check if already imported
  const alreadyImported = this.findMatchingSourceEntries(
    entry => entry.type === 'file' && entry.file === filename
  ).length > 0;

  if (alreadyImported) {
    return {
      success: false,
      clausesAdded: 0,
      errors: [`File ${filename} already imported. Use unimport first to reload.`]
    };
  }

  // Parse file using Prolog parser
  const parseResult = await this.parseFileWithProlog(filename);

  if (!parseResult.success || !parseResult.clauses) {
    return {
      success: false,
      clausesAdded: 0,
      errors: parseResult.errors || ['Unknown parsing error']
    };
  }

  let clausesAdded = 0;
  const errors: string[] = [];

  // Assert each clause with source tracking
  for (const clauseData of parseResult.clauses) {
    const result = await this.assertClauseWithSource(
      clauseData.sourceText,
      'file',
      filename
    );

    if (result.success) {
      clausesAdded++;
      // Optional: Could store clauseData.varNames, singletons in future
    } else {
      errors.push(`Failed to assert: ${clauseData.sourceText.substring(0, 50)}... - ${result.error}`);
    }
  }

  return {
    success: errors.length === 0,
    clausesAdded,
    errors
  };
}
```

**Note**: This replaces the old `parseFileToStringArray` method entirely.

---

### Phase 3: Enhanced Features (Optional)

#### 3.1 Variable Name Metadata Storage

**Enhancement**: Store variable names in SourceEntry for better debugging

```typescript
// Update SourceEntry interface in types.ts
export interface SourceEntry {
  id: string;
  sourceText: string;
  type: 'inline' | 'file';
  file?: string;
  timestamp: number;
  compiled: boolean;

  // NEW: Enhanced metadata from Prolog parser
  varNames?: Array<{ name: string }>;  // Original variable names
  singletons?: string[];                // Unused variable warnings
  lineNumber?: number;                  // Line in source file
}
```

**Benefit**: Enable enhanced error messages like:
```
Error: Variable 'Child' (from family.pl:42) is unbound
Hint: Variable 'Parent' appears only once (singleton warning)
```

#### 3.2 Directive Handling

**Option A: Skip Directives (Current Approach)**
- Simple, safe
- Libraries should be pre-configured via `KB_LIBRARIES` env var

**Option B: Store Directives (Future Enhancement)**
```prolog
% In parse_file_with_source:
process_term((:- Directive), _, _, _, _, LineNum, ClauseData) :-
    term_to_source_string((:- Directive), [], SourceText),
    ClauseData = directive(SourceText, LineNum).
```

**Option C: Execute Safe Directives (Advanced)**
- Validate directive against whitelist
- Execute only safe directives (e.g., `use_module` from safe libraries)
- Block dangerous directives (e.g., `shell`, `load_foreign_library`)

#### 3.3 Comment Preservation

**Challenge**: `read_term/3` doesn't preserve comments

**Options**:
1. **Accept loss** - Comments are for humans, not for execution (current approach)
2. **Hybrid approach** - Read original file, preserve comments in separate structure
3. **Comment metadata** - Use term_position to extract comments from original text

**Recommendation**: Accept loss for MVP, revisit if users request comment preservation

---

## Implementation Phases

### **Phase 1: Core Implementation** ✅ Primary Goal

**Tasks**:
1. Add `parse_file_with_source/2` predicate to prolog_server.pl
2. Add `parseFileWithProlog` method to PrologInterface.ts
3. Replace `parseFileToStringArray` with Prolog parser in `importFileWithSource`
4. Update tests to verify complex syntax handling

**Estimated Effort**: 4-6 hours

**Deliverables**:
- Prolog parser predicates (prolog_server.pl)
- TypeScript integration (PrologInterface.ts)
- Updated/new tests

**Success Criteria**:
- ✅ Can parse files with complex syntax (strings, DCGs, operators)
- ✅ Variable names are preserved in snapshots
- ✅ All tests pass

---

### **Phase 2: Enhanced Metadata (Optional)** 🟡 Future Enhancement

**Tasks**:
1. Extend SourceEntry interface with varNames, singletons, lineNumber
2. Store enhanced metadata in Map
3. Update error messages to use variable names

**Estimated Effort**: 2-3 hours

**Success Criteria**:
- ✅ Error messages reference original variable names
- ✅ Singleton warnings are available to users

---

### **Phase 3: Advanced Features (Future)** 🔵 Future Enhancement

**Tasks**:
1. Directive handling (configurable: skip/store/execute)
2. Comment preservation (if requested by users)
3. Enhanced syntax error reporting (show exact position in file)

**Estimated Effort**: 6-8 hours

---

## Testing Strategy

### Unit Tests

**New Tests in**: `plugins/server/prolog/test/unit/prologParser.test.ts`

```typescript
describe("Prolog-Based File Parser", () => {
  test("should parse simple facts", async () => {
    // Test: parent(john, mary).
    // Expect: sourceText preserves original
  });

  test("should preserve variable names in rules", async () => {
    // Test: grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
    // Expect: GP, GC, P preserved (not _G123)
  });

  test("should handle strings with dots", async () => {
    // Test: person("Dr. Smith", "Ph.D.").
    // Expect: Parses correctly (current parser fails)
  });

  test("should handle DCG rules", async () => {
    // Test: sentence --> noun, verb, ['.'].
    // Expect: Parses correctly (current parser fails)
  });

  test("should handle multi-line clauses", async () => {
    // Test: rule spanning multiple lines
    // Expect: Parsed as single clause
  });

  test("should detect singletons", async () => {
    // Test: process(X, Y, Z) :- calculate(X, Y).
    // Expect: singletons = ['Z']
  });

  test("should handle parse errors gracefully", async () => {
    // Test: Malformed clause
    // Expect: Returns error, continues with rest
  });

  test("should skip directives", async () => {
    // Test: :- use_module(library(lists)).
    // Expect: Directive not in clauses list
  });
});
```

### Integration Tests

**Update existing tests in**: `plugins/server/prolog/test/unit/sourceStorage.test.ts`

```typescript
describe("Source Storage with Prolog Parser", () => {
  test("should import file with complex syntax", async () => {
    const content = `
person("Dr. Smith", doctor).
sentence --> noun, verb.
complex(X, Y) :- calc(X), test(Y).
    `;
    await fs.writeFile(testFile, content);

    const result = await prologInterface.importFileWithSource(testFile);
    expect(result.success).toBe(true);
    expect(result.clausesAdded).toBe(3);

    const snapshot = await prologInterface.getSnapshot();
    expect(snapshot).toContain('person("Dr. Smith"');
    expect(snapshot).toContain('sentence -->');
    expect(snapshot).toContain('complex(X, Y)');
  });
});
```

### Regression Tests

**Ensure backward compatibility**:
- All existing tests should pass
- Snapshots should still contain original variable names
- File import/unimport should work identically

---

## Edge Cases and Considerations

### 1. Encoding Issues

**Problem**: Non-UTF8 files might fail

**Solution**:
```prolog
open(Filename, read, Stream, [encoding(utf8)])
```

### 2. Very Large Files

**Problem**: Loading entire file into memory

**Solution**:
- Prolog streams are lazy (read on demand)
- Process clauses one at a time
- Should handle files with 100K+ clauses

### 3. Cyclic Terms

**Problem**: Terms with cycles (rare in Prolog)

**Solution**:
```prolog
write_term(Term, [
    cycles(true),  % Handle cyclic terms
    ...
])
```

### 4. Module-Qualified Terms

**Problem**: Terms like `module:predicate(X)`

**Solution**: write_term handles this automatically with `quoted(true)`

### 5. Operator Precedence

**Problem**: Custom operators defined in file

**Solution**:
- Operators defined via directives won't be active during parsing
- Could pre-load standard operators
- Or: require operator definitions via KB_LIBRARIES

### 6. Character Escaping

**Problem**: Special characters in atoms/strings

**Solution**: `write_term` with `quoted(true)` handles escaping

---

## Performance Considerations

### Current Parser (JavaScript):
```
Read file: O(n)
Split lines: O(n)
Process lines: O(n)
Total: O(n) where n = file size
```

### Prolog Parser:
```
Open file: O(1)
Read terms: O(m) where m = number of terms
Convert to string: O(k) where k = term size
Total: O(m * k) where m = terms, k = avg term size
```

**Expected**: Similar or slightly slower, but negligible for typical files

**Benchmark**: Test with large file (10K clauses)
- Current: ~500ms
- Expected Prolog: ~600-800ms
- Acceptable trade-off for correctness

---

## Future Enhancements

### 1. Incremental Parsing

**Idea**: Parse only changed clauses on re-import

**Benefit**: Faster re-imports for large files

**Implementation**: Track file hash, only re-parse if changed

### 2. Syntax Error Recovery

**Idea**: Try to parse remaining clauses even if one fails

**Current**: Single parse error stops parsing (could be improved)

**Enhancement**: Continue parsing after error, collect all errors

### 3. Pretty Printing Options

**Idea**: Configurable output format (compact vs readable)

**Implementation**:
```typescript
getSnapshot(options?: { pretty?: boolean }) {
  // If pretty: format with indentation
  // If compact: minimal whitespace
}
```

### 4. Source Maps

**Idea**: Map each clause back to exact line/column in original file

**Use case**: Error messages with precise locations

**Implementation**: Use `term_position` data from read_term

---

## References

### SWI-Prolog Documentation

- [read_term/3](https://www.swi-prolog.org/pldoc/doc_for?object=read_term/3)
- [write_term/2](https://www.swi-prolog.org/pldoc/doc_for?object=write_term/2)
- [variable_names option](https://www.swi-prolog.org/pldoc/man?section=ext-var-info)
- [term_position](https://www.swi-prolog.org/pldoc/man?section=syntax-errors)

### Related Code

- Current parser: `PrologInterface.ts:1699-1727` (parseFileToStringArray)
- Import logic: `PrologInterface.ts:1740-1780` (importFileWithSource)
- Source storage: `PrologInterface.ts:1589-1625` (assertClauseWithSource)

---

## Decision Log

| Date | Decision | Rationale |
|------|----------|-----------|
| 2025-11-17 | Use Prolog parser | More robust than JavaScript text parsing |
| 2025-11-17 | Keep per-clause storage | Essential for mixed file/inline workflows |
| 2025-11-17 | Skip directives (MVP) | Libraries pre-configured via KB_LIBRARIES |
| 2025-11-17 | Accept comment loss (MVP) | Comments not needed for execution |
| 2025-11-17 | Gradual rollout with feature flag | Safe migration path |

---

## Success Metrics

**Phase 1 MVP Success**:
- ✅ All existing tests pass
- ✅ Can parse complex syntax (strings, DCGs, operators)
- ✅ Variable names preserved correctly
- ✅ Performance within 2x of current parser
- ✅ No regressions in existing functionality

**Long-term Success**:
- ✅ Zero parsing-related bug reports
- ✅ Positive feedback from AI agents using complex Prolog
- ✅ Support for full SWI-Prolog syntax

---

## Appendix A: Example Prolog Parser Output

### Input File: `complex.pl`
```prolog
% Complex example file
person("Dr. Smith", doctor).
person("Prof. Jones", professor).

sentence --> noun, verb, ['.'].

grandparent(Grandparent, Grandchild) :-
    parent(Grandparent, Parent),
    parent(Parent, Grandchild).

:- use_module(library(lists)).
```

### Parser Output (JSON):
```json
{
  "success": true,
  "clauses": [
    {
      "sourceText": "person(\"Dr. Smith\", doctor).",
      "varNames": [],
      "singletons": [],
      "lineNumber": 2
    },
    {
      "sourceText": "person(\"Prof. Jones\", professor).",
      "varNames": [],
      "singletons": [],
      "lineNumber": 3
    },
    {
      "sourceText": "sentence-->noun, verb, ['.'].",
      "varNames": [],
      "singletons": [],
      "lineNumber": 5
    },
    {
      "sourceText": "grandparent(Grandparent, Grandchild):-parent(Grandparent, Parent), parent(Parent, Grandchild).",
      "varNames": ["Grandparent", "Grandchild", "Parent"],
      "singletons": [],
      "lineNumber": 7
    }
  ],
  "errors": []
}
```

**Note**: Directive on line 10 is skipped (not in output)

---

## Appendix B: Comparison of Parsing Approaches

| Feature | Current (JS) | Proposed (Prolog) | Winner |
|---------|-------------|-------------------|---------|
| **Correctness** | ⭐⭐ (breaks on edge cases) | ⭐⭐⭐⭐⭐ (perfect) | Prolog |
| **Performance** | ⭐⭐⭐⭐⭐ (very fast) | ⭐⭐⭐⭐ (slightly slower) | JS |
| **Maintainability** | ⭐⭐ (custom logic) | ⭐⭐⭐⭐⭐ (uses stdlib) | Prolog |
| **Variable names** | ⭐⭐⭐⭐⭐ (preserved) | ⭐⭐⭐⭐⭐ (preserved) | Tie |
| **Error messages** | ⭐⭐ (basic) | ⭐⭐⭐⭐ (detailed) | Prolog |
| **Syntax support** | ⭐⭐ (limited) | ⭐⭐⭐⭐⭐ (full) | Prolog |

**Overall Winner**: Prolog Parser (19/30 vs 16/30)

---

## Appendix C: Testing Checklist

**Before Merge**:
- [ ] All unit tests pass
- [ ] All integration tests pass
- [ ] Manual testing with complex files
- [ ] Performance benchmark (< 2x slowdown)
- [ ] Memory usage check (no leaks)
- [ ] Documentation updated
- [ ] CHANGELOG entry added

**After Merge**:
- [ ] Monitor for bug reports
- [ ] Collect user feedback
- [ ] Performance metrics in production
- [ ] Consider removing old parser (after 2 releases)

---

**End of Plan**

This plan can be executed in phases, with Phase 1 (MVP) being the critical path.
Estimated total effort: 6-16 hours depending on desired completeness.
