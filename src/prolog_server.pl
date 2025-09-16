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
    ensure_knowledge_base_module,
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
    ; string_length(Line, Len), Len > 100000 ->  % 100KB limit
        reply(error(line_too_long(Len))),
        server_loop_
    ; process_line(Line),
      server_loop_
    ).

% Process a single input line: parse + optional cmd(ID,Term) wrapping + dispatch
process_line(Line) :-
    ( catch(parse_term(Line, Term0, VarNames), ParseError, (reply(error(syntax_error(ParseError, Line))), fail)) ->
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
    ; % Parse error was already handled and reported
        true
    ).

% Parse term with or without trailing '.' - STRICT parsing only
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

retract_all_clauses(Name/Arity) :-
    functor(Head, Name, Arity),
    retractall(knowledge_base:Head).

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
% Secure consult: only allow facts/rules; load into knowledge_base module
dispatch(consult(File), _) :- !,
    ( catch(safe_consult(File), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

dispatch(consult_file(File), VN) :- !, 
    dispatch(consult(File), VN).

% Dynamic database commands
dispatch(assert(Fact), _) :- !,
    ( catch(assert_knowledge_base_term_safe(Fact), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

dispatch(assertz(Fact), _) :- !,
    ( catch(assert_knowledge_base_term_safe(Fact), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

% Helper to extract predicate functor/arity from a fact or rule
head_of_clause((Head :- _Body), Name, Arity) :- !, functor(Head, Name, Arity).
head_of_clause(Fact, Name, Arity) :- functor(Fact, Name, Arity).

dispatch(retract(Fact), _) :- !,
    ( catch(retract(knowledge_base:Fact), _, fail) -> reply(ok) ; reply(error(nothing_to_retract)) ).

dispatch(retractall(Fact), _) :- !,
    ( catch(retractall(knowledge_base:Fact), E, (reply(error(E)), fail)) -> reply(ok) ; true ).

% ========== STANDARD QUERY MODE (call_nth/2) ==========

% String-based query start (new preferred approach)
dispatch(start_query_string(QueryString), _) :- !,
    debug_trace(parsing_query_string(QueryString)),
    ( catch(read_term_from_atom(QueryString, Query, [variable_names(VarNames)]), ParseError, (
        debug_trace(query_parse_error(ParseError, QueryString)),
        reply(error(invalid_query_syntax(ParseError))),
        !, fail
    )) ->
        debug_trace(parsed_query(Query, VarNames)),
        % Call the start_query logic directly with parsed Query and VarNames
        start_query_impl(Query, VarNames)
    ; true
    ).

% Common implementation for both string-based and direct query starts
start_query_impl(Query, VarNamesAll) :-
    % Basic validation of complex queries
    ( catch(validate_complex_query(Query), ValidationError, (
        debug_trace(query_validation_failed(ValidationError, Query)),
        % Only fail on serious validation errors, not minor issues
        ( ValidationError = error(type_error(_, _), _) ->
            (reply(error(invalid_query_structure(ValidationError))), !, fail)
        ; true  % Allow other validation "errors" to proceed
        )
    )) -> true ; true ),  % Don't fail if validation fails
    % Filter var names to only those that appear in Query
    term_variables(Query, QVars),
    include_varnames_for(QVars, VarNamesAll, QVarNames),
    % Enforce sandbox safety on requested goal (permit undefined user predicates)
    ( safe_goal_ok(knowledge_base:Query) -> true ; debug_trace(unsafe_goal(Query)), reply(error(unsafe_goal(Query))), !, fail ),
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

dispatch(start_query(Query), VarNamesAll) :- !,
    start_query_impl(Query, VarNamesAll).

dispatch(next_solution, _) :- !,
    ( get_active_session(query, (Query, VarNames, N)) ->
        copy_term(Query-VarNames, Q2-V2),
        ( catch(call_nth(knowledge_base:Q2, N), E, (
            debug_trace(query_execution_error(E, N)),
            reply(error(E)), 
            cleanup_query_session, 
            !, fail)) ->
            % Build bindings from the fresh VarName pairs
            var_bindings(V2, Bindings),
            reply(solution(Bindings)),
            N1 is N + 1,
            % Update session state atomically - if any step fails, cleanup
            ( catch((
                retract(active_session(query, (Query, VarNames, N))),
                assertz(active_session(query, (Query, VarNames, N1))),
                retract(solution_index(N)),
                assertz(solution_index(N1))
            ), StateError, (
                debug_trace(state_update_error(StateError, N, N1)),
                cleanup_query_session,
                reply(error(state_inconsistency)),
                fail
            )) -> true ; (cleanup_query_session, fail) )
        ; reply(no_more_solutions),
          cleanup_query_session
        )
    ; reply(error(no_active_query))
    ).

cleanup_query_session :-
    % Robust cleanup - don't fail on individual cleanup steps
    catch(end_session, _, true),
    catch(retractall(current_query(_, _)), _, true),
    catch(retractall(solution_index(_)), _, true),
    debug_trace(query_session_cleaned_up).

dispatch(close_query, _) :- !,
    ( get_active_session(query, _) ->
        end_session,
        retractall(current_query(_, _)),
        retractall(solution_index(_)),
        reply(done)
    ; reply(error(no_active_query))
    ).

% ========== ENGINE QUERY MODE ==========

% String-based engine start (new preferred approach)
dispatch(start_engine_string(QueryString), _) :- !,
    debug_trace(parsing_engine_query_string(QueryString)),
    ( catch(read_term_from_atom(QueryString, Query, [variable_names(VarNames)]), ParseError, (
        debug_trace(engine_query_parse_error(ParseError, QueryString)),
        reply(error(invalid_query_syntax(ParseError))),
        !, fail
    )) ->
        debug_trace(parsed_engine_query(Query, VarNames)),
        % Call the start_engine logic directly with parsed Query and VarNames
        start_engine_impl(Query, VarNames)
    ; true
    ).

% Common implementation for both string-based and direct engine starts
start_engine_impl(Query, VarNamesAll) :-
    % Basic validation of complex queries
    ( catch(validate_complex_query(Query), ValidationError, (
        debug_trace(engine_query_validation_failed(ValidationError, Query)),
        % Only fail on serious validation errors, not minor issues  
        ( ValidationError = error(type_error(_, _), _) ->
            (reply(error(invalid_query_structure(ValidationError))), !, fail)
        ; true  % Allow other validation "errors" to proceed
        )
    )) -> true ; true ),  % Don't fail if validation fails
    % Filter var names to those actually in Query
    term_variables(Query, QVars),
    include_varnames_for(QVars, VarNamesAll, QVarNames),
    % Enforce sandbox safety on requested goal (permit undefined user predicates)
    ( safe_goal_ok(knowledge_base:Query) -> true ; debug_trace(unsafe_goal(Query)), reply(error(unsafe_goal(Query))), !, fail ),
    
    % Check for session conflicts
    ( session_active ->
        get_active_session(CurrentType, _),
        reply(error(session_conflict(CurrentType, engine))),
        fail
    ; true
    ),
    
    % Defensive cleanup of any existing state  
    catch(retractall(current_query(_, _)), _, true),
    catch(retractall(solution_index(_)), _, true),
    catch(retractall(current_engine(_)), _, true),
    
    % Create new engine with enhanced error handling
    % Answer template = QVarNames (list of Name=Var pairs for variable bindings)
    % Goal = Query (the goal to execute)
    % Engine = resulting engine ID
    ( catch(engine_create(QVarNames, knowledge_base:Query, Engine), EngineError, (
        debug_trace(engine_creation_error(EngineError, Query)),
        ( EngineError = error(existence_error(procedure, Pred), _) ->
            reply(error(undefined_predicate_in_query(Pred, Query)))
        ; EngineError = error(evaluation_error(Kind, Culprit), _) ->
            reply(error(arithmetic_error_in_query(Kind, Culprit, Query)))
        ; EngineError = error(syntax_error(Desc), _) ->
            reply(error(syntax_error_in_query(Desc, Query)))
        ; reply(error(engine_creation_failed(EngineError)))
        ),
        fail
    )) ->
        % Start new engine session
        assertz(active_session(engine, Engine)),
        assertz(current_engine(Engine)),
        reply(ok)
    ; reply(error(engine_creation_failed))
    ).

dispatch(start_engine(Query), VarNamesAll) :- !,
    start_engine_impl(Query, VarNamesAll).

dispatch(next_engine, _) :- !,
    debug_trace(next_engine_called),
    ( get_active_session(engine, Engine) ->
        debug_trace(found_active_engine(Engine)),
        catch(
            ( engine_next(Engine, Bindings) ->
                debug_trace(engine_next_success(Bindings)),
                reply(solution(Bindings))
            ; debug_trace(engine_next_failed_no_more_solutions),
              % No more answers - cleanup engine gracefully
              catch(engine_destroy(Engine), DestroyError, (
                  debug_trace(engine_destroy_error(DestroyError))
              )),
              end_session,
              retractall(current_engine(_)),
              reply(no_more_solutions)
            ),
            EngineError,
            (
                debug_trace(engine_next_exception(EngineError)),
                % Clean up failed engine gracefully
                catch(engine_destroy(Engine), DestroyError, (
                    debug_trace(engine_destroy_error(DestroyError))
                )),
                end_session,
                retractall(current_engine(_)),
                ( EngineError = error(evaluation_error(Kind, Culprit), _) ->
                    reply(error(arithmetic_evaluation_failed(Kind, Culprit)))
                ; EngineError = error(existence_error(procedure, Pred), _) ->
                    reply(error(undefined_predicate_during_execution(Pred)))
                ; reply(error(engine_execution_failed(EngineError)))
                )
            )
        )
    ; debug_trace(no_active_engine),
      reply(error(no_active_engine))
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
        ( predicate_property(knowledge_base:Head, dynamic),
          predicate_property(knowledge_base:Head, number_of_clauses(C)), C > 0,
          functor(Head, Name, Arity)
        ), Preds0),
    sort(Preds0, Preds),
    reply(Preds).

% List only the safe, interesting modules we aim to expose
base_exposed_modules([knowledge_base]).

% Report the base exposed modules that are currently loaded
dispatch(list_exposed_modules, _) :- !,
    base_exposed_modules(Base),
    findall(M, (member(M, Base), current_module(M)), Mods0),
    sort(Mods0, Mods),
    reply(Mods).

% List all modules present (excluding internal '$' modules)
dispatch(list_modules, _) :- !,
    findall(M,
        ( current_module(M),
          \+ sub_atom(M, 0, _, _, '$')
        ), Mods0),
    sort(Mods0, Mods),
    reply(Mods).

% List exported predicates for a given module
dispatch(list_module_predicates(Module), _) :- !,
    atom(Module),
    ( Module == knowledge_base ->
        % For user KB, include dynamic predicates that have at least one clause
        findall(Name/Arity,
            ( current_predicate(knowledge_base:Name/Arity),
              functor(Head, Name, Arity),
              predicate_property(knowledge_base:Head, dynamic),
              predicate_property(knowledge_base:Head, number_of_clauses(C)), C > 0
            ), Preds0)
    ;   % For other modules, include only exported/public, defined predicates
        findall(Name/Arity,
            ( current_predicate(Module:Name/Arity),
              functor(Head, Name, Arity),
              ( predicate_property(Module:Head, exported) -> true ; predicate_property(Module:Head, public) ),
              predicate_property(Module:Head, defined)
            ), Preds0)
    ),
    sort(Preds0, Preds),
    reply(Preds).

% RDF-related handlers intentionally omitted to keep API minimal for now.

dispatch(dump_knowledge_base, _) :- !,
    findall(ClauseText,
        ( current_predicate(knowledge_base:Name/Arity),
          functor(Head, Name, Arity),
          predicate_property(knowledge_base:Head, dynamic),
          \+ predicate_property(knowledge_base:Head, imported_from(_)),
          clause(knowledge_base:Head, Body),
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

% Dump only clauses for a specific predicate Name/Arity
dispatch(dump_predicate(Name, Arity), _) :- !,
    atom(Name), integer(Arity),
    ( current_predicate(knowledge_base:Name/Arity) -> true ; reply('% No clauses for predicate'), ! ),
    findall(ClauseText,
        ( functor(Head, Name, Arity),
          predicate_property(knowledge_base:Head, dynamic),
          \+ predicate_property(knowledge_base:Head, imported_from(_)),
          clause(knowledge_base:Head, Body),
          ( Body == true ->
              with_output_to(string(ClauseText), write_term(Head, [quoted(true), numbervars(true)]))
          ; with_output_to(string(ClauseText), write_term((Head :- Body), [quoted(true), numbervars(true)]))
          )
        ), ClauseTexts0),
    ( ClauseTexts0 == [] ->
        reply('% No clauses for predicate')
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

dispatch(clear_knowledge_base, _) :- !,
    findall(Name/Arity, 
        (predicate_property(knowledge_base:Head, dynamic),
         predicate_property(knowledge_base:Head, number_of_clauses(C)), C > 0,
         functor(Head, Name, Arity)
        ), Predicates),
    length(Predicates, Count),
    maplist(retract_all_clauses, Predicates),
    reply(removed(Count)).

% Fallback: treat the term itself as a goal; return one solution
dispatch(Goal, VarNamesAll) :-
    term_variables(Goal, GVars),
    include_varnames_for(GVars, VarNamesAll, GVarNames),
    % Enforce sandbox safety on ad-hoc goal (permit undefined user predicates)
    ( safe_goal_ok(knowledge_base:Goal) -> true ; debug_trace(unsafe_goal(Goal)), reply(error(unsafe_goal(Goal))), !, fail ),
    ( catch(call(knowledge_base:Goal), _, fail) ->
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

% Ensure the knowledge_base module exists without changing this files module context
ensure_knowledge_base_module :-
    ( current_module(knowledge_base) -> true ; create_module(knowledge_base) ),
    catch(set_prolog_flag(knowledge_base:unknown, fail), _, true),
    % Ensure safe library modules are loaded so they appear in module listings
    catch(use_module(library(lists)), _, true),
    catch(use_module(library(between)), _, true),
    catch(use_module(library(apply)), _, true),
    catch(use_module(library(pairs)), _, true),
    catch(use_module(library(ordsets)), _, true),
    % Import a safe subset of library predicates into knowledge_base
    catch(knowledge_base:use_module(library(lists), [
        member/2, append/3, length/2, select/3, nth0/3, nth1/3,
        permutation/2, reverse/2, memberchk/2, last/2, flatten/2,
        sum_list/2, max_list/2, min_list/2, numlist/3, subtract/3,
        union/3, intersection/3, list_to_set/2, is_set/1, subset/2,
        delete/3, selectchk/3
    ]), _, true),
    catch(knowledge_base:use_module(library(between), [between/3]), _, true),
    % Pure, data-only helpers: safe to import into knowledge_base
    catch(knowledge_base:use_module(library(pairs), [
        map_list_to_pairs/3, group_pairs_by_key/2, pairs_keys_values/3,
        pairs_keys/2, pairs_values/2
    ]), _, true),
    catch(knowledge_base:use_module(library(ordsets), [
        list_to_ord_set/2, ord_memberchk/2, ord_union/3, ord_intersection/3,
        ord_subtract/3, ord_subset/2, ord_add_element/3, ord_del_element/3,
        ord_disjoint/2
    ]), _, true),
    % Note: Extended safe set functionality removed - now handled by library(sandbox)
    true.

% Guarded knowledge_base:maplist variants delegating to library(apply)
knowledge_base:maplist(P, L) :-
    strip_module(P, M, Plain),
    ( var(M) -> M2 = knowledge_base ; M2 = M ),
    M2 == knowledge_base,
    apply:maplist(M2:Plain, L).

knowledge_base:maplist(P, L1, L2) :-
    strip_module(P, M, Plain),
    ( var(M) -> M2 = knowledge_base ; M2 = M ),
    M2 == knowledge_base,
    apply:maplist(M2:Plain, L1, L2).

% To run: swipl -q -s prolog_server.pl -g server_loop

% ========== SANDBOXED LOADING ==========

% Read a file and only accept facts/rules. Disallow directives and module changes.
safe_consult(File) :-
    absolute_file_name(File, Abs, [access(read)]),
    read_file_to_terms(Abs, Terms, [syntax_errors(error)]),
    maplist(assert_knowledge_base_term_safe, Terms).

% Reject directives and only assert plain clauses into knowledge_base
assert_knowledge_base_term_safe((:- _Directive)) :-
    throw(error(permission_error(execute, directive, 'Directives are not allowed in sandboxed consult'), _)).
assert_knowledge_base_term_safe((?- _Query)) :-
    throw(error(permission_error(execute, directive, 'Queries are not allowed in sandboxed consult'), _)).
assert_knowledge_base_term_safe(Term) :-
    % Validate clause body is safe before asserting
    clause_safe(Term),
    % Ensure target predicate is dynamic in knowledge_base
    head_of_clause(Term, Name, Arity),
    ( catch(dynamic(knowledge_base:Name/Arity), _, true) ; true ),
    assertz(knowledge_base:Term).

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
% Allow calls to user-defined predicates (treat as knowledge_base: predicates for safety)
body_safe(Term) :-
    strip_module(Term, M, Plain),
    callable(Plain),
    % Either explicitly knowledge_base: module or user module (default for clauses)  
    ( M == knowledge_base ; M == user ),
    % Reject explicitly dangerous operations even if sandbox allows them
    \+ explicitly_dangerous(Plain),
    % For user/knowledge_base predicates: allow if sandbox approves OR it's user-defined
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

% ========== COMPLEX QUERY VALIDATION ==========

% Validate complex queries before execution (basic validation only)
validate_complex_query(Query) :-
    debug_trace(validating_complex_query(Query)),
    callable(Query),
    % Only do term-structure validation here
    % All syntax checking is done by read_term_from_atom
    debug_trace(complex_query_validation_passed(Query)).



% Determine if a goal is safe under sandbox rules, but permit undefined
% user predicates (they will simply fail at runtime due to unknown=fail).

% First, always block explicitly dangerous predicates
safe_goal_ok(Goal) :-
    strip_module(Goal, _, Plain),
    explicitly_dangerous(Plain),
    !, fail.

% Treat a goal in knowledge_base: as safe if its structure is safe per body_safe/1.
safe_goal_ok(Goal) :-
    strip_module(Goal, M, Plain),
    M == knowledge_base, callable(Plain),
    body_safe(Plain),
    debug_trace(knowledge_base_goal_ok(Plain)),
    !.
% As a fallback for knowledge_base goals: allow if not imported (unknown user preds fail harmlessly)
% Only allow if we can verify the predicate is truly user-defined
safe_goal_ok(Goal) :-
    strip_module(Goal, M, Plain),
    M == knowledge_base, callable(Plain),
    % Only allow if we can successfully verify it's not imported AND not built-in
    catch(\+ predicate_property(knowledge_base:Plain, imported_from(_)), _, fail),
    catch(\+ predicate_property(knowledge_base:Plain, built_in), _, fail),
    debug_trace(knowledge_base_goal_default(Plain)),
    !.

% Otherwise, defer to library(sandbox). Allow undefined predicates by
% treating the specific existence_error from the sandbox as safe.
safe_goal_ok(Goal) :-
    catch(sandbox:safe_goal(Goal), E, (
        ( E = error(existence_error(procedure,_), sandbox(_, _))
        -> debug_trace(sandbox_undefined_allowed(Goal)), true
        ; throw(E)
        ))),
    debug_trace(sandbox_ok(Goal)),
    !.
