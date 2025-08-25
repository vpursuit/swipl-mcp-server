:- use_module(library(readutil)).
:- use_module(library(sandbox)).
:- use_module(library(apply)).

% Unified Prolog server supporting both call_nth/2 and engine-based query modes.
% Protocol: one Prolog term per line, structured replies with write_term/2.

% Unified session state tracking for mutual exclusion
:- dynamic active_session/2.     % active_session(Type, Details)
                                % Type: query(Query, VarNames, Index) | engine(EngineId)

% Legacy dynamic predicates for compatibility
:- dynamic current_query/2.     % current_query(QueryTerm, VarNamePairs) - standard mode
:- dynamic solution_index/1.    % 1-based index for call_nth/2 - standard mode
:- dynamic current_engine/1.    % current_engine(EngineId) - engine mode
:- dynamic swi_mcp_trace_enabled/0.
:- dynamic current_request_id/1.

% Debug helper: only emits when trace is enabled from Node side
debug_trace(Term) :-
    ( swi_mcp_trace_enabled ->
        write('@@DEBUG@@ '), write(Term), nl, flush_output
    ;   true
    ).

% Entry point: write READY marker and start loop
server_loop :-
    ensure_kb_module,
    write('@@READY@@'), nl, flush_output,
    server_loop_().

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
    ; process_line(Line),
      server_loop_
    ).

% Process a single input line: parse + optional cmd(ID,Term) wrapping + dispatch
process_line(Line) :-
    ( catch(parse_term(Line, Term0, VarNames), E, (reply(error(parse(E))), fail)) ->
        ( Term0 = cmd(Id, Term) ->
            setup_call_cleanup(
                assertz(current_request_id(Id)),
                catch(dispatch(Term, VarNames), E2, reply(error(E2))),
                retractall(current_request_id(_))
            )
        ;
            catch(dispatch(Term0, VarNames), E2, reply(error(E2)))
        )
    ; true
    ).

% Parse term with or without trailing '.'
parse_term(Line, Term, VarNames) :-
    ( sub_string(Line, _, 1, 0, '.') ->
        S = Line
    ; string_concat(Line, ".", S)
    ),
    read_term_from_atom(S, Term, [variable_names(VarNames), syntax_errors(error)]).

% Unified reply printer (wrap with id/2 when a request id is present)
reply(Term) :-
    ( current_request_id(Id) ->
        write_term(id(Id, Term), [quoted(true), numbervars(true)])
    ;
        write_term(Term, [quoted(true), numbervars(true)])
    ),
    nl, flush_output.

% ========== SESSION STATE MANAGEMENT ==========

% Check if any session is currently active
session_active :-
    active_session(_, _).

% Check if specific session type is active
session_active(Type) :-
    active_session(Type, _).

% Get current session details
get_active_session(Type, Details) :-
    active_session(Type, Details).

% Start a new session (with conflict checking)
start_session(Type, Details) :-
    ( session_active ->
        get_active_session(CurrentType, _),
        reply(error(session_conflict(CurrentType, Type))),
        fail
    ; assertz(active_session(Type, Details))
    ).

% End current session
end_session :-
    retractall(active_session(_, _)).

% Clean up all sessions and resources
cleanup_all_sessions :-
    ( retract(current_engine(E)) -> 
        catch(engine_destroy(E), _, true) 
    ; true
    ),
    retractall(current_query(_, _)),
    retractall(solution_index(_)),
    retractall(active_session(_, _)).

% ========== COMMAND DISPATCH ==========

% File loading commands
% Secure consult: only allow facts/rules; load into kb module
dispatch(consult(File), _) :- !,
    ( catch(safe_consult(File), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

dispatch(consult_file(File), VN) :- !, 
    dispatch(consult(File), VN).

% Dynamic database commands
dispatch(assert(Fact), _) :- !,
    ( catch(assert_kb_term_safe(Fact), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

dispatch(assertz(Fact), _) :- !,
    ( catch(assert_kb_term_safe(Fact), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

% Helper to extract predicate functor/arity from a fact or rule
head_of_clause((Head :- _Body), Name, Arity) :- !, functor(Head, Name, Arity).
head_of_clause(Fact, Name, Arity) :- functor(Fact, Name, Arity).

dispatch(retract(Fact), _) :- !,
    ( catch(retract(kb:Fact), _, fail) -> reply(ok) ; reply(error(nothing_to_retract)) ).

dispatch(retractall(Fact), _) :- !,
    ( catch(retractall(kb:Fact), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

% ========== STANDARD QUERY MODE (call_nth/2) ==========

dispatch(start_query(Query), VarNamesAll) :- !,
    % Filter var names to only those that appear in Query
    term_variables(Query, QVars),
    include_varnames_for(QVars, VarNamesAll, QVarNames),
    % Enforce sandbox safety on requested goal (permit undefined user predicates)
    ( safe_goal_ok(kb:Query) -> true ; debug_trace(unsafe_goal(Query)), reply(error(unsafe_goal(Query))), !, fail ),
    % Check for session conflicts and start new session
    ( start_session(query, (Query, QVarNames, 1)) ->
        % Set up legacy state for compatibility
        retractall(current_query(_, _)),
        retractall(solution_index(_)),
        assertz(current_query(Query, QVarNames)),
        assertz(solution_index(1)),
        reply(ok)
    ; true  % Error already reported by start_session
    ).

dispatch(next_solution, _) :- !,
    ( get_active_session(query, (Query, VarNames, N)) ->
        copy_term(Query-VarNames, Q2-V2),
        ( catch(call_nth(kb:Q2, N), E, (reply(error(E)), cleanup_query_session, !, fail)) ->
            % Build bindings from the fresh VarName pairs
            var_bindings(V2, Bindings),
            reply(solution(Bindings)),
            N1 is N + 1,
            % Update session state
            retract(active_session(query, (Query, VarNames, N))),
            assertz(active_session(query, (Query, VarNames, N1))),
            % Update legacy state
            retract(solution_index(N)),
            assertz(solution_index(N1))
        ; reply(no_more_solutions),
          cleanup_query_session
        )
    ; reply(error(no_active_query))
    ).

cleanup_query_session :-
    end_session,
    retractall(current_query(_, _)),
    retractall(solution_index(_)).

dispatch(close_query, _) :- !,
    ( get_active_session(query, _) ->
        end_session,
        retractall(current_query(_, _)),
        retractall(solution_index(_)),
        reply(done)
    ; reply(error(no_active_query))
    ).

% ========== ENGINE QUERY MODE ==========

dispatch(start_engine(Query), VarNamesAll) :- !,
    % Filter var names to those actually in Query
    term_variables(Query, QVars),
    include_varnames_for(QVars, VarNamesAll, QVarNames),
    % Enforce sandbox safety on requested goal (permit undefined user predicates)
    ( safe_goal_ok(kb:Query) -> true ; debug_trace(unsafe_goal(Query)), reply(error(unsafe_goal(Query))), !, fail ),
    
    % Check for session conflicts
    ( session_active ->
        get_active_session(CurrentType, _),
        reply(error(session_conflict(CurrentType, engine))),
        fail
    ; true
    ),
    
    % Ensure any legacy query state is cleared (defensive)
    retractall(current_query(_, _)),
    retractall(solution_index(_)),
    
    % Create new engine
    % Answer template = QVarNames (list of Name=Var pairs for variable bindings)
    % Goal = Query (the goal to execute)
    % Engine = resulting engine ID
    ( catch(engine_create(QVarNames, kb:Query, Engine), E, (reply(error(E)), fail)) ->
        % Start new engine session
        assertz(active_session(engine, Engine)),
        assertz(current_engine(Engine)),
        reply(ok)
    ; reply(error(engine_creation_failed))
    ).

dispatch(next_engine, _) :- !,
    ( get_active_session(engine, Engine) ->
        ( engine_next(Engine, Bindings) ->
            reply(solution(Bindings))
        ; % No more answers - cleanup engine
          reply(no_more_solutions),
          catch(engine_destroy(Engine), _, true),
          end_session,
          retractall(current_engine(_))
        )
    ; reply(error(no_active_engine))
    ).

dispatch(close_engine, _) :- !,
    ( get_active_session(engine, Engine) ->
        catch(engine_destroy(Engine), _, true),
        end_session,
        retractall(current_engine(_)),
        reply(done)
    ; reply(error(no_active_engine))
    ).

% ========== UTILITY COMMANDS ==========

dispatch(list_predicates, _) :- !,
    findall(Name/Arity,
        ( predicate_property(kb:Head, dynamic),
          predicate_property(kb:Head, number_of_clauses(C)), C > 0,
          functor(Head, Name, Arity)
        ), Preds0),
    sort(Preds0, Preds),
    reply(Preds).

dispatch(dump_kb, _) :- !,
    findall(ClauseText,
        ( current_predicate(kb:Name/Arity),
          functor(Head, Name, Arity),
          predicate_property(kb:Head, dynamic),
          \+ predicate_property(kb:Head, imported_from(_)),
          clause(kb:Head, Body),
          ( Body == true ->
              with_output_to(string(ClauseText), write_term(Head, [quoted(true), numbervars(true)]))
          ; with_output_to(string(ClauseText), write_term((Head :- Body), [quoted(true), numbervars(true)]))
          )
        ), ClauseTexts0),
    ( ClauseTexts0 == [] ->
        reply('% No clauses in knowledge base')
    ; maplist(add_period, ClauseTexts0, ClauseTexts),
      atomic_list_concat(ClauseTexts, '\n', DumpText),
      reply(DumpText)
    ).

% Helper predicate to add period to each clause
add_period(Clause, ClauseWithPeriod) :-
    atom_concat(Clause, '.', ClauseWithPeriod).

dispatch(halt, _) :- !, 
    cleanup_all_sessions,
    halt(0).

dispatch(exit, _) :- !, 
    cleanup_all_sessions,
    halt(0).

% Fallback: treat the term itself as a goal; return one solution
dispatch(Goal, VarNamesAll) :-
    term_variables(Goal, GVars),
    include_varnames_for(GVars, VarNamesAll, GVarNames),
    % Enforce sandbox safety on ad-hoc goal (permit undefined user predicates)
    ( safe_goal_ok(kb:Goal) -> true ; debug_trace(unsafe_goal(Goal)), reply(error(unsafe_goal(Goal))), !, fail ),
    ( catch(call(kb:Goal), _, fail) ->
        var_bindings(GVarNames, Bindings),
        reply(solution(Bindings))
    ; reply(false)
    ).

% ========== HELPER PREDICATES ==========

% Keep only Name=Var pairs whose Var is in Vars
include_varnames_for(Vars, VarNamesIn, VarNamesOut) :-
    include(is_var_in(Vars), VarNamesIn, VarNamesOut).

is_var_in(Vars, _Name=Var) :-
    member(V, Vars), V == Var, !.

% Convert Name=Var pairs to Name=Value term list (values may be unbound)
var_bindings([], []).
var_bindings([Name=Var|T], [Name=Var|BT]) :-
    var_bindings(T, BT).

% Ensure the kb module exists without changing this files module context
ensure_kb_module :-
    ( current_module(kb) -> true ; create_module(kb) ),
    catch(set_prolog_flag(kb:unknown, fail), _, true),
    % Import a safe subset of library predicates into kb
    catch(kb:use_module(library(lists), [
        member/2, append/3, length/2, select/3, nth0/3, nth1/3,
        permutation/2, reverse/2, memberchk/2, last/2, flatten/2,
        sum_list/2, max_list/2, min_list/2, numlist/3, subtract/3,
        union/3, intersection/3, list_to_set/2, is_set/1, subset/2,
        delete/3, selectchk/3
    ]), _, true),
    catch(kb:use_module(library(between), [between/3]), _, true),
    % Note: Extended safe set functionality removed - now handled by library(sandbox)
    true.

% Guarded kb:maplist variants delegating to library(apply)
kb:maplist(P, L) :-
    strip_module(P, M, Plain),
    ( var(M) -> M2 = kb ; M2 = M ),
    M2 == kb,
    apply:maplist(M2:Plain, L).

kb:maplist(P, L1, L2) :-
    strip_module(P, M, Plain),
    ( var(M) -> M2 = kb ; M2 = M ),
    M2 == kb,
    apply:maplist(M2:Plain, L1, L2).

% To run: swipl -q -s prolog_server.pl -g server_loop

% ========== SANDBOXED LOADING ==========

% Read a file and only accept facts/rules. Disallow directives and module changes.
safe_consult(File) :-
    absolute_file_name(File, Abs, [access(read)]),
    read_file_to_terms(Abs, Terms, [syntax_errors(error)]),
    maplist(assert_kb_term_safe, Terms).

% Reject directives and only assert plain clauses into kb
assert_kb_term_safe((:- _Directive)) :-
    throw(error(permission_error(execute, directive, 'Directives are not allowed in sandboxed consult'), _)).
assert_kb_term_safe((?- _Query)) :-
    throw(error(permission_error(execute, directive, 'Queries are not allowed in sandboxed consult'), _)).
assert_kb_term_safe(Term) :-
    % Validate clause body is safe before asserting
    clause_safe(Term),
    % Ensure target predicate is dynamic in kb
    head_of_clause(Term, Name, Arity),
    ( catch(dynamic(kb:Name/Arity), _, true) ; true ),
    assertz(kb:Term).

% Validate a clause/fact has a safe body
clause_safe((Head :- Body)) :- !,
    callable(Head),
    body_safe(Body).
clause_safe(Fact) :-
    callable(Fact).

% Allowed body forms (conservative):
body_safe(true) :- !.
body_safe((A,B)) :- !, body_safe(A), body_safe(B).
body_safe((A;B)) :- !, body_safe(A), body_safe(B).
body_safe((A->B)) :- !, body_safe(A), body_safe(B).
body_safe(\+ A) :- !, body_safe(A).
% Allow calls to user-defined predicates (treat as kb: predicates for safety)
body_safe(Term) :-
    strip_module(Term, M, Plain),
    callable(Plain),
    % Either explicitly kb: module or user module (default for clauses)  
    ( M == kb ; M == user ),
    % Reject explicitly dangerous operations even if sandbox allows them
    \+ explicitly_dangerous(Plain),
    % For user/kb predicates: allow if sandbox approves OR it's user-defined
    ( catch(sandbox:safe_goal(Plain), 
            error(existence_error(procedure,_), _), 
            true  % Allow undefined user predicates (existence error = user-defined)
      ) -> 
        debug_trace(body_safe_user_predicate_sandbox_ok(Plain))
    ; % If sandbox rejects it, still allow if it's not a built-in
      \+ is_builtin_predicate(Plain),
      debug_trace(body_safe_user_predicate_allowed(Plain))
    ).

% Explicitly dangerous operations we never want to allow
explicitly_dangerous(Term) :-
    functor(Term, F, _),
    dangerous_functor(F).

dangerous_functor(call).         % call/1,2,... - arbitrary code execution
dangerous_functor(assert).       % assert/1 - database modification
dangerous_functor(assertz).      % assertz/1 - database modification  
dangerous_functor(asserta).      % asserta/1 - database modification
dangerous_functor(retract).      % retract/1 - database modification
dangerous_functor(retractall).   % retractall/1 - database modification
dangerous_functor(abolish).      % abolish/1 - predicate removal
dangerous_functor(system).       % system/1,2 - system calls
dangerous_functor(shell).        % shell/1,2 - system calls
dangerous_functor(halt).         % halt/0,1 - program termination

% Check if a predicate is a built-in (has predicate properties)
is_builtin_predicate(Term) :-
    catch(predicate_property(Term, built_in), _, fail), !.
is_builtin_predicate(Term) :-
    catch(predicate_property(Term, imported_from(_)), _, fail), !.


% Determine if a goal is safe under sandbox rules, but permit undefined
% user predicates (they will simply fail at runtime due to unknown=fail).
% Treat a goal in kb: as safe if its structure is safe per body_safe/1.
safe_goal_ok(Goal) :-
    strip_module(Goal, M, Plain),
    M == kb, callable(Plain),
    body_safe(Plain),
    debug_trace(kb_goal_ok(Plain)),
    !.
% As a fallback for kb goals: allow if not imported (unknown user preds fail harmlessly)
safe_goal_ok(Goal) :-
    strip_module(Goal, M, Plain),
    M == kb, callable(Plain),
    \+ predicate_property(kb:Plain, imported_from(_)),
    debug_trace(kb_goal_default(Plain)),
    !.

% Otherwise, defer to library(sandbox). Allow undefined predicates by
% treating the specific existence_error from the sandbox as safe.
safe_goal_ok(Goal) :-
    catch(sandbox:safe_goal(Goal), E, (
        ( E = error(existence_error(procedure,_), sandbox(_, _))
        -> debug_trace(sandbox_undefined_allowed(Goal)), true
        ; throw(E)
        )),
    debug_trace(sandbox_ok(Goal)),
    !.