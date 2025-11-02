% Test Prolog file for SWI-MCP server
% Simple facts and rules for testing

% Family relationships
parent(tom, bob).
parent(tom, mary).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% Gender facts
male(tom).
male(bob).
male(jim).
female(mary).
female(ann).
female(pat).

% Derived rules
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).
grandfather(X, Z) :- father(X, Y), parent(Y, Z).
grandmother(X, Z) :- mother(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% Simple mathematical facts
add(0, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

% List operations
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).