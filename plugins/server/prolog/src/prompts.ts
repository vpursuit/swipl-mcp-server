/**
 * MCP Prompts for SWI-Prolog Server
 *
 * Domain-specific prompts that teach MCP tool usage through concrete problem-solving.
 * Each prompt demonstrates key server patterns within a focused problem domain.
 */

export interface PromptArgument {
  name: string;
  description: string;
  required: boolean;
}

export interface PromptMessage {
  role: "user" | "assistant";
  content: {
    type: "text";
    text: string;
  };
}

export interface PrologPrompt {
  name: string;
  title?: string;
  description: string;
  arguments: PromptArgument[];
  messages: (args?: Record<string, string | undefined>) => PromptMessage[];
}

export const prologPrompts: Record<string, PrologPrompt> = {
  // Family tree reasoning with relational logic
  genealogy: {
    name: "genealogy",
    title: "Family Tree Reasoning",
    description: "Build family trees with relational logic. Demonstrates assert_many, recursive rules, query modes, relationship inference.",
    arguments: [
      { name: "family_info", description: "Family members and relationships to model. Provide names and relationships (e.g., 'John is Mary's father, Mary has two children: Alice and Bob')", required: true },
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Build a family tree knowledge base and demonstrate Prolog's relational reasoning:

FAMILY INFORMATION:
${args.family_info || '[Please provide family members and their relationships]'}

EXECUTION PATTERN: For each step: announce action, show tool calls/results, explain insights.

PREREQUISITE: If you haven't already, first read the capabilities:
- Use the help tool, OR
- Read reference://capabilities resource
This shows available tools, security model, and server features.

WORKFLOW - Demonstrate these MCP tool usage patterns:

STEP 1 - Define Base Facts
Use clauses to efficiently add facts for:
- parent_of(Parent, Child) - parent-child relationships
- male(Person) / female(Person) - gender information
- spouse_of(Person1, Person2) - marriage relationships (if provided)

Example:
clauses({
  facts: [
    "parent_of(john, mary)",
    "parent_of(john, alice)",
    "male(john)",
    "female(mary)",
    "female(alice)"
  ]
})

→ Show the facts you added

STEP 2 - Define Inference Rules
Use clauses to add rules that derive relationships:

Essential Rules:
\`\`\`prolog
% Direct parent relationships
father_of(F, C) :- male(F), parent_of(F, C).
mother_of(M, C) :- female(M), parent_of(M, C).

% Sibling relationships
sibling_of(X, Y) :- parent_of(P, X), parent_of(P, Y), X \\= Y.
brother_of(X, Y) :- male(X), sibling_of(X, Y).
sister_of(X, Y) :- female(X), sibling_of(X, Y).

% Grandparent relationships
grandparent_of(GP, GC) :- parent_of(GP, P), parent_of(P, GC).
grandfather_of(GF, GC) :- male(GF), grandparent_of(GF, GC).
grandmother_of(GM, GC) :- female(GM), grandparent_of(GM, GC).

% Ancestor (recursive)
ancestor_of(A, D) :- parent_of(A, D).
ancestor_of(A, D) :- parent_of(A, X), ancestor_of(X, D).

% Cousins
cousin_of(X, Y) :- parent_of(PX, X), parent_of(PY, Y), sibling_of(PX, PY).

% Uncle/Aunt
uncle_of(U, N) :- male(U), sibling_of(U, P), parent_of(P, N).
aunt_of(A, N) :- female(A), sibling_of(A, P), parent_of(P, N).
\`\`\`

→ Display the rules you created

STEP 3 - Verify with symbols_list
Use symbols_list to confirm all predicates were loaded successfully.
Check output for: parent_of/2, father_of/2, sibling_of/2, ancestor_of/2, etc.

→ Confirm all predicates loaded successfully

STEP 4 - Query Relationships
Demonstrate both query modes:

A. Simple queries with query:
   - Find specific relationships: "father_of(john, Who)"
   - Check relationships: "sibling_of(mary, alice)"

B. Complex queries with query:
   - Find all ancestors: "ancestor_of(Ancestor, alice)"
   - Find all cousins: "cousin_of(X, Y)"
   - Multi-hop relationships: "grandfather_of(GF, alice)"

Use query_nextSolution to iterate through multiple results.

NOTE: Starting a new query automatically closes any previous query/engine session.
While explicit query_close is best practice, it's not strictly required.

→ Show all solutions for each query

STEP 5 - Demonstrate Results
For each query:
1. Show the query pattern
2. Display all solutions found
3. Explain the logical inference path

Example output format:
Query: "grandfather_of(Who, alice)"
Solutions:
  - Who = john (because john is parent of mary, mary is parent of alice)

→ Present the complete family tree analysis

KEY LEARNING POINTS:
- clauses: Batch loading facts and rules
- Recursive rules: ancestor_of demonstrates recursive descent
- Query modes: query for simple, query for backtracking
- symbols_list: Verify loaded predicates
- Logical variables: Use capitalized vars (X, Y, Who) for unknowns

Now build the family tree knowledge base and demonstrate these patterns.`
        }
      }
    ]
  },

  // Task scheduling with constraints
  scheduling: {
    name: "scheduling",
    title: "Task Scheduling",
    description: "Schedule tasks with dependencies using CLP(FD) constraints. Demonstrates load_library, constraint solving, labeling optimization.",
    arguments: [
      { name: "tasks", description: "Tasks to schedule with durations and dependencies. Format: 'Task1 (duration X), Task2 (duration Y) depends on Task1, ...'", required: true },
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Create a task scheduling solution using constraint logic programming:

SCHEDULING PROBLEM:
${args.tasks || '[Please provide tasks with durations and dependencies]'}

EXECUTION PATTERN: For each step: announce action, show tool calls/results, explain insights.

PREREQUISITE: If you haven't already, first read the capabilities:
- Use the help tool, OR
- Read reference://capabilities resource
This shows available tools, security model, and server features.

WORKFLOW - Demonstrate these MCP tool usage patterns:

STEP 1 - Load CLP(FD) Library
Use files to load constraint programming:
files({ library: "clpfd" })

This provides:
- Constraint operators: #=, #<, #>, #=<, #>=, #\\=
- Domain specification: ins, in
- Constraint predicates: all_different/1, cumulative/2
- Search: label/1, labeling/2

→ Show library loaded and constraints available

STEP 2 - Model the Scheduling Problem
Define a schedule/1 predicate that models tasks as constraint variables.

Task representation:
- Each task has: Start time, Duration, End time
- Constraints: End #= Start + Duration
- Dependencies: TaskB_Start #>= TaskA_End

Example for 3 tasks (Design, Code, Test):
\`\`\`prolog
% Task durations
task_duration(design, 5).
task_duration(code, 10).
task_duration(test, 3).

% Dependencies
depends_on(code, design).
depends_on(test, code).

% Main scheduling predicate
schedule(Tasks) :-
    % Define task start times as variables
    Tasks = [DesignStart, CodeStart, TestStart],

    % All tasks start at time 0 or later
    Tasks ins 0..100,

    % Get durations
    task_duration(design, DD),
    task_duration(code, CD),
    task_duration(test, TD),

    % Calculate end times
    DesignEnd #= DesignStart + DD,
    CodeEnd #= CodeStart + CD,
    TestEnd #= TestStart + TD,

    % Dependency constraints
    CodeStart #>= DesignEnd,    % Code starts after Design ends
    TestStart #>= CodeEnd,       % Test starts after Code ends

    % Optimize for earliest completion
    ProjectEnd #= TestEnd,

    % Find solution
    labeling([min(ProjectEnd)], Tasks).
\`\`\`

STEP 3 - Add Scheduling Rules
Use clauses to load all predicates at once:

clauses({
  facts: [
    "task_duration(design, 5)",
    "task_duration(code, 10)",
    "task_duration(test, 3)",
    "depends_on(code, design)",
    "depends_on(test, code)",
    "schedule(Tasks) :- Tasks = [DS, CS, TS], Tasks ins 0..100, task_duration(design, DD), task_duration(code, CD), task_duration(test, TD), DE #= DS + DD, CE #= CS + CD, TE #= TS + TD, CS #>= DE, TS #>= CE, PE #= TE, labeling([min(PE)], Tasks)"
  ]
})

→ Display the rules and constraints added

STEP 4 - Solve with Constraint Solving
Use query (not query) for constraint problems:

query({ query: "schedule(Tasks)" })

NOTE: Starting a new query automatically closes any previous query/engine session.

CLP(FD) will:
1. Set up constraint network
2. Propagate constraints to prune search space
3. Use labeling to find optimal solution
4. Return task start times

→ Show the solution returned by the constraint solver

STEP 5 - Display Schedule
Format the solution as a Gantt chart or timeline:

Example output:
\`\`\`
Optimal Schedule (Project completes at time 18):
  Design:  Start=0,  End=5   (duration 5)
  Code:    Start=5,  End=15  (duration 10)
  Test:    Start=15, End=18  (duration 3)

Critical Path: Design → Code → Test (total: 18 time units)
\`\`\`

→ Present the complete schedule with timeline visualization

ADVANCED FEATURES (if applicable):
- Resource constraints: Use cumulative/2 for limited resources
- Alternative schedules: Use query_nextSolution for other solutions
- Parallel tasks: Tasks without dependencies can overlap

KEY LEARNING POINTS:
- files: Load CLP(FD) for constraint solving
- CLP(FD) operators: #=, #>=, ins for constraint specification
- labeling/2: Trigger search with optimization (min/max)
- query: Required for constraint solving (not query)
- Constraint propagation: Prolog prunes impossible solutions automatically

Now create the scheduling solution and demonstrate these constraint programming patterns.`
        }
      }
    ]
  },

  // Logic puzzle solver
  puzzle: {
    name: "puzzle",
    title: "Logic Puzzle Solver",
    description: "Solve logic puzzles with CLP(FD) constraint programming. Demonstrates constraint encoding, all_different, labeling strategies.",
    arguments: [
      {
        name: "puzzle",
        description: "The logic puzzle to solve with numbered clues. Leave empty or use '?' to get puzzle suggestions.",
        required: true
      }
    ],
    messages: (args = {}) => {
      // Treat undefined, null, empty string, and whitespace-only as "not provided"
      const hasPuzzle = args.puzzle != null && args.puzzle.trim() !== "";

      if (!hasPuzzle) {
        return [
          {
            role: "user",
            content: {
              type: "text",
              text: `Suggest 3 interesting logic puzzles to solve with Prolog constraint programming.

For each puzzle provide:
- Name (catchy and descriptive)
- Description (2-3 sentences explaining the puzzle)
- Difficulty (Easy/Medium/Hard)
- Why it's interesting with CLP(FD) (what constraints make it elegant in Prolog)

Good puzzle types:
- Zebra Puzzle (Einstein's Riddle) - classic constraint satisfaction
- N-Queens - placement with diagonal constraints
- Sudoku - grid constraints with all_different
- Send More Money - cryptarithmetic with carry constraints
- Magic Square - sum constraints in rows/columns/diagonals
- Graph Coloring - adjacency constraints

Present 3 compelling options and wait for selection.`
            }
          }
        ];
      }

      return [
        {
          role: "user",
          content: {
            type: "text",
            text: `Solve this logic puzzle using Prolog constraint programming:

PUZZLE:
${args.puzzle}

EXECUTION PATTERN: For each step: announce action, show tool calls/results, explain insights.

PREREQUISITE: If you haven't already, first read the capabilities:
- Use the help tool, OR
- Read reference://capabilities resource
This shows available tools, security model, and server features.

WORKFLOW - Demonstrate these MCP tool usage patterns:

STEP 1 - Load CLP(FD)
Use files to enable constraint programming:
files({ library: "clpfd" })

→ Show library loaded successfully

STEP 2 - Design solve/1 Predicate Pattern
\`\`\`prolog
solve(Vars) :-
    Vars ins 1..N,              % Set domains
    % ... add constraints for each clue ...
    all_different(Vars),         % Uniqueness
    labeling([ff], Vars).        % Search (use [ff] for performance)
\`\`\`

STEP 3 - Assert Rules
\`\`\`
clauses({
  facts: [
    "solve(...) :- ... ins 1..N, constraints..., labeling([ff], ...)"
  ]
})
\`\`\`

IMPORTANT: If clauses fails with "Invalid Prolog syntax" for complex rules:
- Use clauses instead (handles rules with :- better)
- Compress the rule into a single line without newlines
- Example: clauses({ fact: "solve(S):-S ins 1..9, all_distinct(S), labeling([ff],S)." })

→ Display the constraints defined

STEP 4 - Query with query
\`\`\`
query({ query: "solve(Solution)" })
\`\`\`
CLP(FD) REQUIRES engine mode (not query).

NOTE: Starting a new query automatically closes any previous query/engine session.

STEP 5 - Extract ALL Solutions
After starting the engine, you MUST call query_next repeatedly to extract solutions:
\`\`\`
query_next()  // First solution
query_next()  // Second solution (if desired)
... continue until status='done'
\`\`\`

CRITICAL: The engine only starts in Step 4. Solutions are retrieved in Step 5 by calling query_next.
Do NOT stop after query - always follow with query_next calls.

PERFORMANCE NOTE:
- Use labeling([ff], Vars) for first-fail heuristic (2,500x faster)
- N-Queens: N≤20 is fast, N>25 may timeout
- Default timeout: 30 seconds

→ Show all solutions found

STEP 6 - Present Solution
Map variable assignments to puzzle entities and verify constraints are satisfied.

→ Present complete solution with proper formatting (e.g., grids for magic squares, boards for N-Queens)

CONCRETE EXAMPLE - 4-Queens Step-by-Step:

Step 1: Load CLP(FD) library
\`\`\`
Tool: files({ library: "clpfd" })
Result: "Successfully loaded library(clpfd)"
\`\`\`

Step 2: Assert N-Queens predicates
\`\`\`
Tool: clauses({
  facts: [
    "solve(Qs) :- length(Qs, 4), Qs ins 1..4, all_different(Qs), safe(Qs), labeling([ff], Qs)",
    "safe([])",
    "safe([Q|Qs]) :- safe(Qs, Q, 1), safe(Qs)",
    "safe([], _, _)",
    "safe([Q|Qs], Q0, D) :- Q0 #\\\\= Q, abs(Q0-Q) #\\\\= D, D1 #= D+1, safe(Qs, Q0, D1)"
  ]
})
Result: "Successfully asserted 5 facts/rules"
\`\`\`

Step 3: Start engine
\`\`\`
Tool: query({ query: "solve(Qs)" })
Result: { status: "ready", engine_ready: true }
\`\`\`

Step 4: Extract solutions
\`\`\`
Tool: query_next()
Result: { status: "success", solution: "Qs = [2,4,1,3]" }

Tool: query_next()  // Get another solution (optional)
Result: { status: "success", solution: "Qs = [3,1,4,2]" }

Tool: query_next()  // Continue until done
Result: { status: "done" }
\`\`\`

Interpretation of first solution:
- Row 1: Queen at column 2
- Row 2: Queen at column 4
- Row 3: Queen at column 1
- Row 4: Queen at column 3

This shows the EXACT tool calls and expected results. Note the complete workflow: startEngine → query_next (repeated) → done.

KEY POINTS:
- Load library(clpfd) first
- Use query (not query) for CLP(FD) queries
- Pattern: Vars ins 1..N, constraints, labeling([ff], Vars)
- Use [ff] for first-fail heuristic (much faster)
- ALWAYS call query_next after query to extract solutions
- If clauses fails on complex rules, use clauses instead

Now solve the puzzle using these constraint programming patterns.

AFTER SOLVING: Suggest 2-3 related puzzles the user might enjoy next, such as:
- Sudoku (grid constraints with all_different)
- Magic Square (sum constraints)
- Graph Coloring (adjacency constraints)
- Send More Money (cryptarithmetic)
- Zebra Puzzle (Einstein's Riddle)
Choose puzzles that build on similar constraint types or increase complexity.`
          }
        }
      ];
    }
  },

  // Natural language parsing with DCGs
  grammar: {
    name: "grammar",
    title: "Grammar Parser",
    description: "Parse natural language with Definite Clause Grammars (DCGs). Demonstrates DCG syntax, phrase/2, parse tree generation.",
    arguments: [
      { name: "sentence", description: "Sentence to parse (e.g., 'the cat sat on the mat'). If not provided, use a default example.", required: false },
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Parse a sentence using Prolog's Definite Clause Grammars (DCGs):

SENTENCE TO PARSE:
${args.sentence || 'the cat sat on the mat'}

EXECUTION PATTERN: For each step: announce action, show tool calls/results, explain insights.

PREREQUISITE: If you haven't already, first read the capabilities:
- Use the help tool, OR
- Read reference://capabilities resource
This shows available tools, security model, and server features.

WORKFLOW - Demonstrate these MCP tool usage patterns:

STEP 1 - Define DCG Grammar Rules
DCG notation: rule_name --> components.

Basic English grammar structure:
\`\`\`prolog
% Sentence structure
sentence --> noun_phrase, verb_phrase.

% Noun phrase: (Determiner) + (Adjective) + Noun
noun_phrase --> determiner, noun.
noun_phrase --> determiner, adjective, noun.
noun_phrase --> noun.  % For proper nouns

% Verb phrase: Verb + (Prepositional Phrase)
verb_phrase --> verb.
verb_phrase --> verb, noun_phrase.
verb_phrase --> verb, prepositional_phrase.
verb_phrase --> verb, prepositional_phrase, prepositional_phrase.

% Prepositional phrase
prepositional_phrase --> preposition, noun_phrase.

% Lexicon (vocabulary)
determiner --> [the].
determiner --> [a].

adjective --> [big].
adjective --> [small].
adjective --> [happy].

noun --> [cat].
noun --> [dog].
noun --> [mat].
noun --> [house].

verb --> [sat].
verb --> [ran].
verb --> [slept].

preposition --> [on].
preposition --> [in].
preposition --> [under].
\`\`\`

STEP 2 - Add Parse Tree Generation
Enhance DCG to build parse trees:

\`\`\`prolog
% Parse tree version
sentence(s(NP, VP)) --> noun_phrase(NP), verb_phrase(VP).

noun_phrase(np(Det, N)) --> determiner(Det), noun(N).
noun_phrase(np(Det, Adj, N)) --> determiner(Det), adjective(Adj), noun(N).

verb_phrase(vp(V)) --> verb(V).
verb_phrase(vp(V, NP)) --> verb(V), noun_phrase(NP).
verb_phrase(vp(V, PP)) --> verb(V), prepositional_phrase(PP).

prepositional_phrase(pp(Prep, NP)) --> preposition(Prep), noun_phrase(NP).

% Lexicon with parse tree nodes
determiner(det(W)) --> [W], {member(W, [the, a])}.
noun(n(W)) --> [W], {member(W, [cat, dog, mat])}.
verb(v(W)) --> [W], {member(W, [sat, ran])}.
preposition(prep(W)) --> [W], {member(W, [on, in, under])}.
\`\`\`

STEP 3 - Load Grammar with clauses
Use clauses to load all DCG rules:

clauses({
  facts: [
    "sentence --> noun_phrase, verb_phrase",
    "noun_phrase --> determiner, noun",
    "verb_phrase --> verb, prepositional_phrase",
    "prepositional_phrase --> preposition, noun_phrase",
    "determiner --> [the]",
    "determiner --> [a]",
    "noun --> [cat]",
    "noun --> [mat]",
    "verb --> [sat]",
    "preposition --> [on]"
  ]
})

Note: Prolog automatically translates DCG notation into phrase/2 compatible predicates.

→ Show the DCG rules loaded

STEP 4 - Parse with phrase/2
Use query to parse sentences with phrase/2:

The sentence as a word list:
sentence = [the, cat, sat, on, the, mat]

Query patterns:
A. Validity check:
   query({ query: "phrase(sentence, [the, cat, sat, on, the, mat])" })
   Result: true/false (grammatically correct?)

B. With parse tree:
   query({ query: "phrase(sentence(Tree), [the, cat, sat, on, the, mat])" })
   Result: Tree = s(np(det(the), n(cat)), vp(v(sat), pp(prep(on), np(det(the), n(mat)))))

C. Generate sentences:
   query({ query: "phrase(sentence, Sentence)" })
   Result: All grammatically valid sentences from the grammar

NOTE: Starting a new query automatically closes any previous query/engine session.

→ Display parse results for each query pattern

STEP 5 - Display Parse Tree
Format the parse tree hierarchically:

\`\`\`
Parse Tree for "the cat sat on the mat":

s (sentence)
├─ np (noun phrase)
│  ├─ det: "the"
│  └─ n: "cat"
└─ vp (verb phrase)
   ├─ v: "sat"
   └─ pp (prepositional phrase)
      ├─ prep: "on"
      └─ np (noun phrase)
         ├─ det: "the"
         └─ n: "mat"
\`\`\`

Structure:
- Subject: "the cat" (noun phrase)
- Action: "sat on the mat" (verb phrase with prepositional phrase)

→ Present the complete parse tree visualization

STEP 6 - Verify with symbols_list
Use symbols_list to confirm DCG predicates were created:
- Should see: sentence/2, noun_phrase/2, verb_phrase/2, phrase/2 (built-in)

ADVANCED FEATURES:
- Semantic actions: Add {Prolog goals} within DCG rules for constraints
- Ambiguity handling: Use query_nextSolution for multiple parse trees
- Context-free languages: DCGs can parse any context-free grammar

KEY LEARNING POINTS:
- DCG notation: --> for grammar rules, [word] for terminals
- phrase/2: Built-in predicate to invoke DCG parsers
- Parse trees: Pass arguments to DCG rules to build structure
- clauses: Batch-load grammar rules
- query vs query: Start for single parse, Engine for generation
- Prolog's strength: Pattern matching + backtracking = natural parser

Now parse the sentence and demonstrate these DCG patterns.`
        }
      }
    ]
  }
};
