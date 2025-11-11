/**
 * Domain knowledge about Prolog errors
 */

import { PrologErrorKind } from "./PrologInterface.js";

/**
 * Domain-specific knowledge for each Prolog error type
 */
export const PROLOG_ERROR_KNOWLEDGE: Record<string, { description: string; examples: string }> = {
  [PrologErrorKind.UNSAFE_GOAL]: {
    description: `
In SWI-Prolog with sandbox security, an unsafe_goal error occurs when a goal attempts
to use predicates or operations that are not permitted in the sandboxed environment.

Common causes:
- Using file I/O predicates (open/3, read/2, write/2)
- Calling system/1 or shell/1
- Using call/1 with untrusted code
- Accessing predicates outside the allowed modules

The sandbox restricts operations to safe built-ins and user-defined predicates in
the knowledge_base module.`,
    examples: `
BAD:  query("shell('ls')").  % shell/1 is blocked
GOOD: Use allowed predicates only

BAD:  query("call(UserGoal)").  % call/1 is blocked
GOOD: Define predicates explicitly in knowledge base

Use knowledge_base_assert to add facts/rules before querying.`,
  },

  [PrologErrorKind.INSTANTIATION_ERROR]: {
    description: `
An instantiation_error occurs when a built-in predicate requires a variable to be
bound to a specific value, but the variable is still unbound (uninstantiated).

Common causes:
- Using a variable in arithmetic before binding it: X > 5 (X not bound)
- Calling predicates that expect ground terms: atom_length(X, _)
- Using variables in is/2 on the right side without binding
- Passing unbound variables to type-checking predicates

Fix by ensuring variables are bound before use, typically by reordering goals or
using member/2, between/3, or other binding predicates first.`,
    examples: `
BAD:  solve(X) :- X > 5, X = 10.  % X used before binding
GOOD: solve(X) :- X = 10, X > 5.  % X bound first

BAD:  foo(L) :- length(L, N), N > 0.  % L might be unbound
GOOD: foo(L) :- L = [_|_], length(L, N), N > 0.

BAD:  bar(X) :- Y is X + 1, X = 5.  % X used before binding
GOOD: bar(X) :- X = 5, Y is X + 1.  % X bound first`,
  },

  [PrologErrorKind.SYNTAX_ERROR]: {
    description: `
A syntax_error occurs when Prolog code violates the language's syntactic rules.

Common causes:
- Missing or mismatched parentheses: foo(a, b
- Missing operators: X 5 instead of X = 5
- Invalid operators or precedence: X == Y == Z
- Unclosed strings or atoms: 'hello
- Invalid character sequences
- Missing periods at end of clauses (when asserting)

Check for balanced parentheses, proper operator usage, and valid Prolog syntax.`,
    examples: `
BAD:  parent(john, mary  % Missing closing parenthesis
GOOD: parent(john, mary).

BAD:  X 5  % Missing operator
GOOD: X = 5.

BAD:  foo :- bar, % Trailing comma
GOOD: foo :- bar.

BAD:  'unclosed string
GOOD: 'closed string'`,
  },

  [PrologErrorKind.EXISTENCE_ERROR]: {
    description: `
An existence_error occurs when trying to reference something that doesn't exist.

Common causes:
- Calling undefined predicates (not in KB or libraries)
- Referencing non-existent procedures
- Using predicates from libraries that aren't loaded
- Typos in predicate names

Solutions:
- Use knowledge_base_assert to define predicates before querying
- Load required libraries with knowledge_base_load_library
- Check predicate names for typos
- Use symbols_list to see what's currently defined`,
    examples: `
BAD:  query("foo(X)").  % foo/1 not defined
GOOD: knowledge_base_assert("foo(a)."), then query

BAD:  query("append([1,2], [3], X)").  % lists library not loaded
GOOD: knowledge_base_load_library("lists"), then query

Use symbols_list to verify what predicates are available.`,
  },

  [PrologErrorKind.PERMISSION_ERROR]: {
    description: `
A permission_error occurs when attempting an operation that is not permitted.

Common causes:
- Trying to modify built-in predicates
- Attempting to abolish system predicates
- File access violations (when filesystem operations are restricted)
- Module access restrictions

Most operations in the sandbox are restricted to the knowledge_base module
for security reasons.`,
    examples: `
BAD:  Trying to retract built-in predicates
GOOD: Only retract user-defined predicates in knowledge_base

BAD:  Attempting to modify system modules
GOOD: All user code goes in knowledge_base module`,
  },

  [PrologErrorKind.TIMEOUT]: {
    description: `
A timeout error occurs when a query takes longer than the configured timeout
to complete.

Common causes:
- Infinite loops or infinite recursion
- Unoptimized queries generating too many solutions
- Queries without proper base cases
- Accidental non-terminating goals

Solutions:
- Add base cases to recursive predicates
- Use cuts (!) to limit backtracking
- Check for infinite loops in rules
- Optimize query with constraints
- Increase timeout if genuinely needed (via environment variables)`,
    examples: `
BAD:  ancestor(X,Y) :- ancestor(X,Z), parent(Z,Y).  % Infinite loop
GOOD: ancestor(X,Y) :- parent(X,Y).
      ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).

BAD:  loop :- loop.  % Infinite recursion
GOOD: Add base case or termination condition`,
  },

  [PrologErrorKind.SESSION_CONFLICT]: {
    description: `
A session_conflict error occurs when trying to start a new query or engine
while another session is already active.

The system now auto-closes previous sessions, so this error is rare in current
versions. If encountered:

Solutions:
- Call query_close to explicitly close the active session
- The system will auto-close previous sessions on new starts
- Check for proper query lifecycle management`,
    examples: `
PATTERN:
1. query_start or query_startEngine
2. query_next (iterate solutions)
3. query_close (or start new query to auto-close)

If session_conflict occurs, explicitly close with query_close.`,
  },

  [PrologErrorKind.NO_ACTIVE_SESSION]: {
    description: `
A no_active_session error occurs when calling query_next or query_close
without an active query or engine session.

Common causes:
- Calling query_next before query_start/query_startEngine
- Session already exhausted or closed
- Incorrect tool call order

Solution:
Always follow the pattern: query_start → query_next → query_close`,
    examples: `
BAD:  query_next without query_start
GOOD: query_start first, then query_next

PATTERN:
1. query_start { query: "member(X, [1,2,3])" }
2. query_next (get first solution)
3. query_next (get second solution)
4. query_next (returns done when exhausted)
5. query_close`,
  },

  [PrologErrorKind.QUERY_TOO_LARGE]: {
    description: `
A query_too_large error occurs when the query string exceeds the maximum
allowed length (currently 5000 characters).

Solutions:
- Break complex queries into smaller parts
- Assert complex rules to KB first, then query them
- Use knowledge_base_assert for large fact sets
- Simplify query logic`,
    examples: `
BAD:  Very long query string (>5000 chars)
GOOD: knowledge_base_assert("complex_rule :- ..."), then query("complex_rule")

BAD:  Inline hundreds of facts in query
GOOD: Use knowledge_base_assert_many to load facts first`,
  },

  [PrologErrorKind.UNKNOWN]: {
    description: `
An unknown error occurred that doesn't match known Prolog error patterns.

This could indicate:
- Internal server error
- Unexpected Prolog runtime error
- Malformed input
- Server communication issue

Check the raw error message for details and verify:
- Query syntax is valid
- Knowledge base is in consistent state
- Required libraries are loaded`,
    examples: `
If unknown error persists:
1. Check error message for clues
2. Try knowledge_base_clear to reset state
3. Verify library loading with symbols_list
4. Simplify query to isolate issue`,
  },
};

/**
 * Get domain knowledge for a specific error kind
 */
export function getErrorKnowledge(errorKind: PrologErrorKind): { description: string; examples: string } {
  return PROLOG_ERROR_KNOWLEDGE[errorKind] || PROLOG_ERROR_KNOWLEDGE[PrologErrorKind.UNKNOWN];
}
