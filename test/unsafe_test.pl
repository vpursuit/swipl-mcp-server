% Erlaubte Fakten und Regeln
parent(alice, bob).
parent(bob, carol).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

% Unsichere Direktiven (sollten während consult blockiert werden)
:- write('DIES SOLLTE NICHT AUSGEFÜHRT WERDEN!'), nl.
:- shell('ls').

% Blacklisted Prädikate (für Test der explicitly_dangerous/1 Blacklist)
% Diese sollten bei Abfrage blockiert werden, nicht bei consult

% call/* - arbitrary code execution
test_call_1 :- call(true).
test_call_2 :- call(write, 'dangerous').

% assert/* - database modification  
test_assert :- assert(malicious(fact)).
test_assertz :- assertz(malicious(fact)).
test_asserta :- asserta(malicious(fact)).

% retract/* - database modification
test_retract :- retract(parent(_, _)).
test_retractall :- retractall(parent(_, _)).

% abolish/* - predicate removal
test_abolish :- abolish(parent/2).

% system/* - system calls
test_system_1 :- system('rm -rf /').
test_system_2 :- system('echo dangerous', _).

% shell/* - system calls  
test_shell_1 :- shell('rm -rf /').
test_shell_2 :- shell('echo dangerous', _).

% halt/* - program termination
test_halt_0 :- halt.
test_halt_1 :- halt(1).