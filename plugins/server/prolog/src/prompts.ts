/**
 * MCP Prompts for SWI-Prolog Server
 *
 * These prompts guide agents/LLMs to effectively use the Prolog server
 * by first discovering resources for context, then using tools efficiently.
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
  // Initialize expert mode (optionally focused on a task)
  initExpert: {
    name: "prolog_init_expert",
    title: "Initialize Expert Context",
    description: "Set up expert context; optionally focus on a specific task",
    arguments: [
      { name: "task", description: "Optional task to focus expert setup and reasoning", required: false },
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `You are a Prolog and logic programming expert.${args.task ? ` Focus on this task: ${args.task}` : ''}

Recommended first step — Discovery:
1. List all available resources to understand the server
2. Read the 'capabilities' resource (reference://capabilities) for server features and security
3. Read the 'help' resource (reference://help) for comprehensive usage guidelines
4. Check 'knowledge-base-predicates' resource (prolog://knowledge_base/predicates) for current knowledge base predicates
5. Review 'knowledge-base-dump' resource (prolog://knowledge_base/dump) for full knowledge base content

EXPERT KNOWLEDGE - You are an expert in:
- SWI-Prolog syntax: facts, rules, queries, unification, DCGs
- Logic programming paradigms and best practices
- Knowledge representation and automated reasoning
- Query optimization: cuts, indexing, goal ordering
- Debugging: trace/spy, deterministic vs non-deterministic predicates
- Built-in predicates: findall/3, bagof/3, setof/3, member/2, append/3, between/3, permutation/2

IMPORTANT - AVAILABLE PREDICATES:
- All standard SWI-Prolog predicates are available
- library(clpfd) available: Constraint Logic Programming over Finite Domains
- Load with :- use_module(library(clpfd)) in .pl files
- Standard alternatives also work: between/3 for domains, is/2 for arithmetic, permutation/2 for uniqueness
- Both constraint propagation (CLP(FD)) and generate-and-test patterns supported
- Check 'capabilities' resource for full details

SECURITY AWARENESS (from capabilities resource):
- File operations restricted to ~/.model-context-lab/
- Dangerous predicates blocked: shell(), system(), call(), halt()
- Use only safe predicates in knowledge_base module
- All queries executed in sandboxed environment

EFFICIENT TOOL USAGE (token‑aware):
- Use knowledge_base_assert_many for batch fact loading (more efficient than single assertions)
- Prefer query_startEngine for complex queries with backtracking
- Check symbols_list to see available predicates before defining new ones
- Use knowledge_base_dump to export and verify knowledge base state
- Validate file paths before knowledge_base_load operations
\nToken hygiene: prefer summarizing resources (e.g. list predicates, skim dump headers) and quote only minimal snippets.

Always check resources first for context, then use tools based on discovered capabilities.`
        }
      }
    ]
  },

  // Knowledge base analysis - uses resources
  analyzeKnowledgeBase: {
    name: "prolog_analyze_knowledge_base",
    title: "Analyze Knowledge Base",
    description: "Analyze the current Prolog knowledge base using resources and provide insights",
    arguments: [],
    messages: () => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Analyze the current Prolog knowledge base comprehensively:

STEP 1 - Resource Discovery:
First, read these resources to understand the current state:
- 'knowledge-base-predicates' resource: See what predicates are currently defined
- 'knowledge-base-dump' resource: Review all facts and rules in detail
- 'capabilities' resource: Understand security constraints and features

STEP 2 - Analysis:
Based on the resource content, analyze:
- What domains/concepts are modeled in the current KB?
- What base facts exist and how are they structured?
- What inference rules are defined and their logical relationships?
- Are there recursive predicates? Are they well-formed with proper base cases?
- What built-in predicates are being used effectively?
- Are there any performance optimization opportunities?

STEP 3 - Recommendations:
Suggest improvements based on Prolog best practices:
- Missing predicates that would be useful for the domain
- Optimization opportunities (goal ordering, cuts, indexing)
- Additional facts or rules that would enhance reasoning
- Query patterns that would be most effective

STEP 4 - Example Queries:
Provide example queries that demonstrate the KB's capabilities and suggest new ones to try.`
        }
      }
    ]
  },

  // expertReasoning removed — merged into initExpert via optional 'task'

  // Quick reference prompt
  quickReference: {
    name: "prolog_quick_reference",
    title: "Quick Reference Guide",
    description: "Get a comprehensive overview of all server resources, tools, and capabilities",
    arguments: [],
    messages: () => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Provide a comprehensive quick reference guide for this Prolog server:

PHASE 1 - Resource Discovery:
List all available resources and read each one completely:
- reference://help: Usage guidelines and best practices
- reference://license: License information
- reference://capabilities: Server capabilities, security model, and constraints
- prolog://knowledge_base/predicates: Currently defined predicates in knowledge base
- prolog://knowledge_base/dump: Complete knowledge base content (facts and rules)

PHASE 2 - Analysis and Summary:
After reading all resources, provide:

1. SERVER CAPABILITIES:
   - Available tools and their specific purposes
   - Query modes (standard vs engine) and when to use each
   - Security restrictions and safe operations
   - File handling capabilities and restrictions

2. CURRENT KNOWLEDGE BASE STATE:
   - What predicates are defined
   - What domains/concepts are modeled
   - Available facts and inference rules

3. BEST PRACTICES (from help resource):
   - Recommended tool usage patterns
   - Performance optimization tips
   - Common pitfalls to avoid

4. EXAMPLE WORKFLOWS:
   - How to load knowledge from files
   - How to build knowledge bases programmatically
   - How to query effectively in both modes
   - How to debug and optimize queries

5. QUICK REFERENCE CARD:
   - Most commonly used tools
   - Essential Prolog built-in predicates available
   - Security do's and don'ts

6. PREDICATE AVAILABILITY:
   - Confirm all standard SWI-Prolog predicates are available
   - Note that CLP(FD) library(clpfd) is NOT available
   - List standard alternatives for constraint problems

This will serve as a complete orientation to the server's capabilities.`
        }
      }
    ]
  },

  // Knowledge base builder with resource awareness
  knowledgeBaseBuilder: {
    name: "prolog_knowledge_base_builder",
    title: "Build Knowledge Base",
    description: "Build a comprehensive knowledge base for a domain using server resources",
    arguments: [
      {
        name: "domain",
        description: "The domain to model (e.g., family relationships, expert system, planning)",
        required: true
      }
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Build a comprehensive Prolog knowledge base for domain: ${args.domain || '[Please specify a domain to model]'}

PREPARATION PHASE:
1. Read 'capabilities' resource: Understand security constraints and available features
2. Check 'knowledge-base-predicates' resource: See what predicates already exist to avoid conflicts
3. Review 'knowledge-base-dump' resource: Understand current knowledge base state
4. Read 'help' resource: Get guidance on best practices

IMPORTANT - Predicate Availability:
- All standard SWI-Prolog predicates are available (member/2, append/3, between/3, etc.)
- library(clpfd) available: Load with :- use_module(library(clpfd)) for constraint programming
- Standard alternatives: between/3, is/2, permutation/2 for generate-and-test pattern
- Choose approach based on problem size: CLP(FD) for complex constraints, standard predicates for simpler cases

DESIGN PHASE:
1. Domain Analysis:
   - Identify key entities and their relationships
   - Determine base facts vs derived knowledge (rules)
   - Plan predicate names following Prolog conventions (lowercase, descriptive)
   - Consider arity and argument patterns for consistency

2. Knowledge Architecture:
   - Base facts: atomic, ground terms representing known information
   - Rules: logical relationships that derive new knowledge
   - Helper predicates: utility predicates for common operations
   - Meta-level predicates: for introspection and control

IMPLEMENTATION PHASE:
1. Create Base Facts:
   - Use knowledge_base_assert_many for efficient batch loading
   - Follow consistent naming: predicate(arg1, arg2, ...)
   - Group related facts together

2. Define Rules:
   - Start with simple, non-recursive rules
   - Add recursive rules with proper base cases
   - Use cuts (!) strategically to control backtracking
   - Order goals from most restrictive to least restrictive

3. Add Helper Predicates:
   - List manipulation utilities
   - Type checking predicates
   - Domain-specific operations

VALIDATION PHASE:
1. Verify Implementation:
   - Use symbols_list to confirm predicates are loaded
   - Test with sample queries using both query modes
   - Check with knowledge-base-dump resource to see final structure

2. Testing Strategy:
   - Test base cases and edge cases
   - Verify recursive predicates don't cause infinite loops
   - Test with query_startEngine for complex backtracking scenarios
   - Validate performance with large datasets

3. Documentation:
   - Include comments explaining predicate purposes
   - Document argument patterns and types
   - Provide example queries

Create a robust, well-structured knowledge base following Prolog best practices.`
        }
      }
    ]
  },

  // Query optimization expert
  queryOptimizer: {
    name: "prolog_query_optimizer",
    title: "Optimize Query",
    description: "Optimize Prolog queries for maximum performance and correctness",
    arguments: [
      {
        name: "query",
        description: "The Prolog query to analyze and optimize",
        required: true
      }
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `Optimize this Prolog query for performance: ${args.query || '[Please provide a Prolog query to optimize]'}

ANALYSIS PHASE:
First, check the current knowledge base state:
1. Read 'knowledge-base-predicates' resource: Understand available predicates
2. Review 'knowledge-base-dump' resource: See data patterns and indexing opportunities
3. Check 'capabilities' resource: Understand system constraints

IMPORTANT - Predicate Availability:
- All standard SWI-Prolog predicates are available for optimization
- library(clpfd) available: Load with :- use_module(library(clpfd)) for constraint optimization
- Standard alternatives: between/3 for domain generation, is/2 for arithmetic, permutation/2 for uniqueness
- Consider both constraint propagation (CLP(FD)) and generate-and-test patterns based on problem characteristics

OPTIMIZATION STRATEGY:
1. Goal Ordering Analysis:
   - Identify most restrictive goals (those that reduce search space most)
   - Move deterministic goals before non-deterministic ones
   - Place goals with ground arguments early
   - Consider predicate indexing patterns

2. Cut Strategy:
   - Identify where cuts (!) can eliminate unnecessary backtracking
   - Ensure cuts don't prevent finding valid solutions
   - Use green cuts (don't change declarative meaning) vs red cuts

3. Predicate Design:
   - Check if auxiliary predicates would improve efficiency
   - Consider tail recursion optimization opportunities
   - Evaluate if findall/3, bagof/3, or setof/3 would be better than backtracking

4. Indexing Opportunities:
   - Arrange arguments to take advantage of first-argument indexing
   - Consider adding auxiliary predicates with better indexing patterns

IMPLEMENTATION:
1. Original Query Analysis:
   - Break down the query structure
   - Identify potential performance bottlenecks
   - Estimate computational complexity

2. Optimized Version:
   - Provide reordered version with justification
   - Add strategic cuts if beneficial
   - Suggest alternative formulations if applicable

3. Testing Strategy:
   - Test with query_start for simple cases
   - Use query_startEngine for complex backtracking scenarios
   - Compare performance between original and optimized versions

4. Alternative Approaches:
   - Consider different algorithmic approaches
   - Suggest additional predicates that might help
   - Recommend query modes based on expected solution patterns

Provide the optimized query with detailed explanations of each optimization decision.`
        }
      }
    ]
  },

  // Logic puzzle solver using CLP(FD)
  logicPuzzleSolver: {
    name: "prolog_logic_puzzle_solver",
    title: "Solve Logic Puzzle",
    description: "Solve logic puzzles using the server's CLP(FD) capabilities and tools",
    arguments: [
      {
        name: "puzzle",
        description: "The logic puzzle to solve (with numbered clues). If empty, agent chooses an interesting puzzle.",
        required: false
      }
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `You are a Prolog expert with access to a SWI-Prolog MCP server. Solve the following logic puzzle using the server's CLP(FD) capabilities.

PREPARATION:
If you have access to the 'capabilities' tool and haven't checked it yet, read it first to understand available CLP(FD) features and server constraints.

${args.puzzle ? `PUZZLE:\n${args.puzzle}` : `PUZZLE:\nChoose an interesting logic puzzle (e.g., Zebra puzzle, Einstein's riddle, or similar constraint problem). State the puzzle clearly with numbered clues.`}

WORKFLOW:
1. Load library(clpfd) using knowledge_base_load_library({ library: "clpfd" })
2. Design a solve/1 predicate that encodes the puzzle constraints
3. Use knowledge_base_assert_many to add all rules to the server
4. Query with query_startEngine to find solutions
5. Display and explain the results

Solution Design Guidelines:
- Create a solve/1 predicate that unifies its argument with the solution
- Represent entities as positions in a list (e.g., [H1,H2,H3,H4,H5])
- Encode each clue as a constraint in the solve/1 body
- Set up domains (e.g., Vars ins 1..5)
- Use all_different/1 where appropriate
- Call label/1 to trigger search

Example for N-Queens:
First, design the rules, then use knowledge_base_assert_many with a list of rule strings:

\`\`\`
knowledge_base_assert_many with:
[
  "solve(Qs) :- length(Qs, 4), Qs ins 1..4, all_different(Qs), safe(Qs), label(Qs)",
  "safe([])",
  "safe([Q|Qs]) :- safe(Qs, Q, 1), safe(Qs)",
  "safe([], _, _)",
  "safe([Q|Qs], Q0, D) :- Q0 #\\\\= Q, abs(Q0 - Q) #\\\\= D, D1 #= D + 1, safe(Qs, Q0, D1)"
]
\`\`\`

Then query: \`query_startEngine("solve(Solution)")\` to get solutions.

Now solve the puzzle using this approach with knowledge_base_assert_many and query_startEngine.`
        }
      }
    ]
  }
};
