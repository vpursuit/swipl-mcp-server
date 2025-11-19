# CLP(FD) vs. Standard Prolog in swipl-mcp-server

## Overview

The swipl-mcp-server **includes CLP(FD)** (`library(clpfd)`) **pre-loaded by default**. This document provides guidance on when to use CLP(FD) versus standard Prolog predicates, and shows how constraint problems can be solved with either approach.

**Note**: CLP(FD) is **pre-loaded at server startup** and available immediately - no need to load it manually. This document serves as a reference for understanding both approaches and choosing the appropriate one for your use case.

## What's Available vs. What's Not

### ✅ **Available** (Standard Prolog)

| Predicate | Purpose | Example |
|-----------|---------|---------|
| `member/2` | List membership | `member(X, [1,2,3])` |
| `between/3` | Generate integers in range | `between(1, 10, X)` |
| `permutation/2` | Generate permutations | `permutation([1,2,3], P)` |
| `=\=` | Arithmetic inequality | `X =\= Y` |
| `is/2` | Arithmetic evaluation | `Z is X + Y` |
| `abs/1` | Absolute value | `abs(X - Y)` |
| `>/2, </2, >=/2, =</2` | Comparisons | `X > 5` |
| `findall/3` | Collect solutions | `findall(X, goal(X), L)` |
| `append/3` | List concatenation | `append([1,2], [3,4], X)` |
| `length/2` | List length | `length([1,2,3], N)` |

### ✅ **Also Available** (CLP(FD))

| CLP(FD) Feature | Standard Prolog Alternative | When to Use Each |
|-----------------|----------------------------|------------------|
| `X in 1..10` | `between(1, 10, X)` | CLP(FD): complex constraints; Standard: simple iteration |
| `X #= Y + 1` | `X is Y + 1` | CLP(FD): bidirectional; Standard: when right side known |
| `X #\= Y` | `X =\= Y` (or `\+ X = Y` for unification) | CLP(FD): constraint propagation; Standard: direct test |
| `X #< Y` | `X < Y` (requires instantiated values) | CLP(FD): constraints; Standard: comparisons |
| `all_different/1` | Custom predicate (see below) | CLP(FD): efficiency; Standard: small domains |
| `all_distinct/1` | Custom predicate (see below) | CLP(FD): efficiency; Standard: small domains |
| `label/1` | `member/2` or `permutation/2` | CLP(FD): with constraints; Standard: simple search |
| Constraint propagation | Generate-and-test | CLP(FD): large search spaces; Standard: small problems |

## Key Differences

### CLP(FD) Approach: Constraint Propagation
```prolog
% CLP(FD) - Pre-loaded by default
queens_clpfd(N, Qs) :-
    length(Qs, N),
    Qs ins 1..N,          % Domain constraint
    all_distinct(Qs),      % Propagates immediately
    safe_queens(Qs),
    label(Qs).            % Search with propagation
```

### Standard Prolog Approach: Generate-and-Test
```prolog
% Standard Prolog - Works without library loading
queens(N, Qs) :-
    numlist(1, N, Domain),
    permutation(Domain, Qs),  % Generate candidate
    safe_queens(Qs).          % Test if valid
```

**Trade-off**: CLP(FD) is more efficient for large N and complex constraints. Standard Prolog is simpler for small to medium problems and when CLP(FD) features aren't needed.

## Common Patterns & Conversions

### Pattern 1: Domain Declaration

```prolog
% CLP(FD) approach (pre-loaded by default)
X in 1..10

% Standard Prolog approach (no library needed)
between(1, 10, X)

% Alternative for multiple variables:
member(X, [1,2,3,4,5,6,7,8,9,10])
```

### Pattern 2: All Different Constraint

```prolog
% CLP(FD) approach (pre-loaded by default)
all_different([X, Y, Z])

% Standard Prolog - Option 1: Explicit constraints
all_different([X, Y, Z]) :-
    X =\= Y, X =\= Z, Y =\= Z.

% Standard Prolog - Option 2: General predicate
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

% Standard Prolog - Option 3: Use permutation
% If you need unique values from a fixed set:
permutation([1,2,3,4], [X, Y, Z, W])  % Automatically all different
```

### Pattern 3: Arithmetic Constraints

```prolog
% CLP(FD) approach (pre-loaded by default)
X #= Y + 1,
Z #< X * 2

% Standard Prolog approach
X is Y + 1,
Z < X * 2
```

⚠️ **Important**: With `is/2`, the right side must be instantiated. Generate values first, then test:

```prolog
% CORRECT order:
between(1, 10, Y),   % Generate Y first
X is Y + 1,          % Then compute X
Z < X * 2            % Then test constraint

% WRONG order (will error):
X is Y + 1,          % Error: Y not instantiated yet
between(1, 10, Y)
```

### Pattern 4: Global Constraints

```prolog
% CLP(FD) approach (pre-loaded by default)
element(I, List, X)

% Standard Prolog approach
nth1(I, List, X)     % 1-indexed
% or
nth0(I, List, X)     % 0-indexed
```

## Real Example: 4-Queens Problem

### CLP(FD) Version
```prolog
% CLP(FD) - pre-loaded by default
queens_clpfd(Queens) :-
    Queens = [Q1, Q2, Q3, Q4],
    Queens ins 1..4,
    all_distinct(Queens),
    Q1 #\= Q2 + 1, Q1 #\= Q2 - 1,
    % ... more constraints
    label(Queens).
```

### Standard Prolog Version
```prolog
queens_4(Solution) :-
    % Generate candidates
    member(C1, [1,2,3,4]),
    member(C2, [1,2,3,4]),
    member(C3, [1,2,3,4]),
    member(C4, [1,2,3,4]),
    % Test: All different
    all_different([C1, C2, C3, C4]),
    % Test: Safe diagonals
    abs(1 - 2) =\= abs(C1 - C2),
    abs(1 - 3) =\= abs(C1 - C3),
    abs(1 - 4) =\= abs(C1 - C4),
    abs(2 - 3) =\= abs(C2 - C3),
    abs(2 - 4) =\= abs(C2 - C4),
    abs(3 - 4) =\= abs(C3 - C4),
    % Build solution
    Solution = [[1,C1], [2,C2], [3,C3], [4,C4]].

all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).
```

### Optimized Version (Using permutation)
```prolog
queens_4_fast(Solution) :-
    permutation([1,2,3,4], [C1, C2, C3, C4]),  % Auto all-different
    abs(1 - 2) =\= abs(C1 - C2),
    abs(1 - 3) =\= abs(C1 - C3),
    abs(1 - 4) =\= abs(C1 - C4),
    abs(2 - 3) =\= abs(C2 - C3),
    abs(2 - 4) =\= abs(C2 - C4),
    abs(3 - 4) =\= abs(C3 - C4),
    Solution = [[1,C1], [2,C2], [3,C3], [4,C4]].
```

## Performance Considerations

| Problem Size | Standard Prolog | CLP(FD) |
|--------------|-----------------|---------|
| N ≤ 8 | ✅ Fast enough | Faster with propagation |
| 8 < N ≤ 12 | ⚠️ Slower but works | Much faster |
| N > 12 | ❌ Too slow | Recommended |

**Guidance**: Use CLP(FD) for complex constraints and larger problem sizes. Standard Prolog is simpler for small problems where constraint propagation isn't needed.

## Common Mistakes

### Note: CLP(FD) is Pre-Loaded in MCP Server

```prolog
% ✅ CLP(FD) is pre-loaded - use directly in MCP server
solve :- X in 1..10, Y in 1..10, X #< Y.

% ✅ For standalone .pl files outside MCP server, you need to load it:
:- use_module(library(clpfd)).
solve :- X in 1..10, Y in 1..10, X #< Y.

% ✅ ALTERNATIVE - Use standard predicates (works everywhere)
solve :- between(1, 10, X), between(1, 10, Y), X < Y.
```

### Mistake 2: Wrong Order of Constraints

```prolog
% ❌ WRONG - X not instantiated
X < Y, between(1, 10, X)

% ✅ CORRECT - Generate first, test second
between(1, 10, X), X < Y
```

### Mistake 3: Expecting Constraint Propagation

```prolog
% CLP(FD) would propagate: if X #< Y and Y #< Z, then X #< Z
% Standard Prolog does NOT propagate - you must test explicitly

% ✅ CORRECT - Test all constraints
between(1, 10, X),
between(1, 10, Y),
between(1, 10, Z),
X < Y,              % Test explicitly
Y < Z               % Test explicitly
```

## Best Practices for swipl-mcp-server

1. **Choose the right approach**: Use CLP(FD) for complex constraints, standard predicates for simple problems
2. **CLP(FD) is pre-loaded**: Use constraint operators immediately - no need to load library(clpfd) in MCP server
3. **For standalone .pl files**: Add `:- use_module(library(clpfd)).` at the top if using CLP(FD) outside MCP server
4. **Use `permutation/2`** for "all different" constraints on small domains (standard approach)
5. **Generate before testing**: With standard predicates, use `between/3` or `member/2` to instantiate variables before testing
6. **Use `findall/3`** to collect all solutions efficiently
7. **Write helper predicates** to make code readable
8. **Test incrementally**: Start with simple constraints, add complexity gradually

## Useful Helper Predicates

These work great in swipl-mcp-server:

```prolog
% Check if all elements satisfy a condition
all_satisfy(_, []).
all_satisfy(Goal, [H|T]) :-
    call(Goal, H),
    all_satisfy(Goal, T).

% Check pairwise condition
all_pairs(_, []).
all_pairs(_, [_]).
all_pairs(Goal, [H|T]) :-
    maplist(call(Goal, H), T),
    all_pairs(Goal, T).

% Generate N variables in range
n_between(0, _, _, []).
n_between(N, Low, High, [X|Xs]) :-
    N > 0,
    between(Low, High, X),
    N1 is N - 1,
    n_between(N1, Low, High, Xs).
```

## Summary

**The server supports both approaches to constraint problems**:

**CLP(FD) approach** (pre-loaded in MCP server; load `:- use_module(library(clpfd))` in standalone .pl files):
- `X in Low..High` for domain constraints
- `#=`, `#\=`, `#<`, etc. for constraint arithmetic
- `all_distinct/1` for uniqueness constraints
- Constraint propagation reduces search space
- Available immediately in MCP server - no loading needed

**Standard Prolog approach** (no library loading needed):
- `between/3` for generating integer ranges
- `=\=`, `<`, `>` for arithmetic comparisons
- `permutation/2` for all-different on small domains
- Generate-and-test pattern

Choose based on problem complexity and performance requirements. See SECURITY.md for complete list of allowed libraries.
