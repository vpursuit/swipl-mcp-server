# Prolog Server Enhancement Plan - Strict Parsing Focus

## Core Principle: **Trust Prolog's Parser Completely**
Stop trying to "fix" or guess user intent. Make the server strict, predictable, and robust by relying entirely on `read_term_from_atom` for syntax validation.

## Implementation Tasks

### 1. **Make parse_term/3 Completely Strict** (CRITICAL)

**Current Issue**: The predicate tries to "fix" malformed input by adding parentheses when it contains commas, leading to misinterpretation of user intent.

**Solution**:
```prolog
% NEW: Simple, strict parsing - no guessing, no fixing
parse_term(Line, Term, VarNames) :-
    ( sub_string(Line, _, 1, 0, '.') ->
        S = Line
    ; string_concat(Line, ".", S)
    ),
    read_term_from_atom(S, Term, [variable_names(VarNames), syntax_errors(error)]).
```

**Changes Required**:
- Remove ALL retry logic (lines 67-81 in prolog_server.pl)
- Remove catch/throw wrapper that obscures errors
- Let parse errors propagate directly to the client
- **Result**: Clear, unambiguous error messages

### 2. **Enhanced Error Reporting in process_line/1**

**Solution**:
```prolog
process_line(Line) :-
    ( parse_term(Line, Term0, VarNames) ->
        % Process successfully parsed term
        ( Term0 = cmd(Id, Term) ->
            setup_call_cleanup(
                assertz(current_request_id(Id)),
                catch(dispatch(Term, VarNames), E2, reply(error(E2))),
                retractall(current_request_id(_))
            )
        ;
            catch(dispatch(Term0, VarNames), E2, reply(error(E2)))
        )
    ; % Parsing failed - report exact error to client
        catch(parse_term(Line, _, _), ParseError, true),
        reply(error(syntax_error(ParseError, Line)))
    ).
```

**Benefits**:
- Report exact `syntax_error` or `type_error` from read_term_from_atom
- Include the problematic line for debugging
- Client knows exactly what's wrong with their input

### 3. **Remove ALL String-Based Validation**

**To Delete**:
- `malformed_compound/1` (lines 605-608) - redundant with parser
- `check_balanced_parens/2` (lines 593-603) - unused and unnecessary
- String-based checks in `validate_compound_structure/1`

**Result**: No duplicate validation, complete trust in the parser

### 4. **Simplify validate_complex_query/1**

**Solution**:
```prolog
validate_complex_query(Query) :-
    debug_trace(validating_complex_query(Query)),
    callable(Query),
    % Only do term-structure validation here
    % All syntax checking is done by read_term_from_atom
    debug_trace(complex_query_validation_passed(Query)).
```

**Changes**:
- Focus ONLY on term structure validation
- Remove all string-based validation attempts
- Keep semantic/security checks in `body_safe/1`
- Clear separation of concerns

### 5. **Clean Up Duplicate Code**

**Duplicates to Remove**:
- `body_safe/1` defined twice (keep lines 495-517, delete lines 563-585)
- `is_builtin_predicate/1` defined twice (keep lines 536-540, delete lines 587-591)

**Result**: Cleaner, more maintainable code

### 6. **Add Robust Input Protection** (Enhancement)

**Solution**:
```prolog
% Optional: Add max line length check for DOS protection
server_loop_ :-
    read_line_to_string(user_input, Line),
    ( Line == end_of_file ->
        cleanup_all_sessions,
        halt(0)
    ; Line == "__EXIT__" ->
        cleanup_all_sessions,
        halt(0)
    ; Line == "" ->
        server_loop_   % ignore empty lines
    ; string_length(Line, Len), Len > 100000 ->  % 100KB limit
        reply(error(line_too_long(Len))),
        server_loop_
    ; process_line(Line),
      server_loop_
    ).
```

### 7. **Improve Error Context for Common Issues**

**Enhanced Error Reporting**:
- Undefined predicates: Clear message about missing predicate
- Arithmetic errors: Specify the failing expression
- Syntax errors: Show exact position if available from read_term_from_atom

## Impact Analysis

### TypeScript API Compatibility
- **Fully preserved** - Interface remains identical
- Better error messages will improve debugging
- No changes needed to TypeScript code

### Breaking Changes
- **NONE** - All commands work exactly the same
- Only difference: malformed input that was "fixed" before will now properly error

### Benefits
1. **Predictable behavior** - No surprising "fixes" or reinterpretations
2. **Clear error messages** - Developers know exactly what went wrong
3. **Better performance** - No retry overhead on parse errors
4. **Simpler code** - Easier to maintain and debug
5. **Follows best practices** - Leverages Prolog's robust parser

## Testing Strategy

### 1. Compatibility Testing
- Run all existing integration tests unchanged
- Verify all pass with new strict parsing

### 2. Error Handling Tests
- Add tests for malformed input scenarios:
  - Missing parentheses: `a,b` should error, not become `(a,b)`
  - Double commas: `a,,b` should report syntax error
  - Unbalanced parentheses: Proper error from parser
  
### 3. Performance Testing
- Measure parse time improvement without retry logic
- Verify no regression in normal query processing

## Implementation Order

1. **First**: Remove parentheses heuristic from parse_term/3
2. **Second**: Clean up duplicate code definitions
3. **Third**: Remove string-based validation predicates
4. **Fourth**: Simplify validate_complex_query
5. **Fifth**: Add input length protection
6. **Finally**: Run comprehensive test suite

## Key Philosophy

> "The server's job is to parse what it receives strictly, not to guess the user's intent."

By making `parse_term/3` strict and removing the parentheses heuristic, we create a more predictable, robust, and easier-to-debug parsing layer that relies on Prolog's built-in parser for what it does best.

## File Locations

- Main changes: `src/prolog_server.pl`
- No changes needed: `src/PrologInterface.ts`
- Test updates: `test/integration/complexQueries.test.ts` (may need to update some tests that relied on the "fixing" behavior)