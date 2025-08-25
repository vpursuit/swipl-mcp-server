% Test data for unit tests
% Simple facts for testing basic operations

% Test facts
test_fact(a).
test_fact(b).
test_fact(c).

% Simple rules
test_rule(X) :- test_fact(X).

% Family relationships for testing
parent(john, mary).
parent(mary, tom).
male(john).
male(tom).
female(mary).

% Mathematical predicates
is_even(0).
is_even(N) :- N > 0, N1 is N - 2, is_even(N1).