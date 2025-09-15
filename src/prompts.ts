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
  title: string;
  description: string;
  arguments: PromptArgument[];
  messages: (args?: Record<string, string>) => PromptMessage[];
}

export const prologPrompts: Record<string, PrologPrompt> = {
  // Initialize expert mode - START HERE
  initExpert: {
    name: "prolog_init_expert",
    title: "Initialize Prolog Expert Mode",
    description: "Set up the LLM as a Prolog expert and discover server capabilities",
    arguments: [],
    messages: () => [
      {
        role: "user",
        content: {
          type: "text",
          text: `You are now a Prolog and logic programming expert.

IMPORTANT - Discovery Phase (Do this FIRST):
1. List all available resources to understand the server
2. Read the 'capabilities' resource (meta://capabilities) for server features and security
3. Read the 'help' resource (meta://help) for comprehensive usage guidelines
4. Check 'kb-predicates' resource (prolog://kb/predicates) for current knowledge base predicates
5. Review 'kb-dump' resource (prolog://kb/dump) for full knowledge base content

EXPERT KNOWLEDGE - You are an expert in:
- SWI-Prolog syntax: facts, rules, queries, unification, DCGs
- Logic programming paradigms and best practices
- Knowledge representation and automated reasoning
- Constraint Logic Programming (CLP) and meta-programming
- Query optimization: cuts, indexing, goal ordering
- Debugging: trace/spy, deterministic vs non-deterministic predicates
- Built-in predicates: findall/3, bagof/3, setof/3, member/2, append/3

SECURITY AWARENESS (from capabilities resource):
- File operations restricted to ~/.swipl-mcp-server/
- Dangerous predicates blocked: shell(), system(), call(), halt()
- Use only safe predicates in kb module
- All queries executed in sandboxed environment

EFFICIENT TOOL USAGE:
- Use db_assert_many for batch fact loading (more efficient than single assertions)
- Prefer query_startEngine for complex queries with backtracking
- Check symbols_list to see available predicates before defining new ones
- Use db_dump to export and verify knowledge base state
- Validate file paths before db_load operations

Always check resources first for context, then use tools based on discovered capabilities.`
        }
      }
    ]
  },

  // Knowledge base analysis - uses resources
  analyzeKB: {
    name: "prolog_analyze_kb",
    title: "Analyze Current Knowledge Base",
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
- 'kb-predicates' resource: See what predicates are currently defined
- 'kb-dump' resource: Review all facts and rules in detail
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

  // Expert reasoning with resource context
  expertReasoning: {
    name: "prolog_expert_reasoning",
    title: "Prolog Expert Reasoning with Full Context",
    description: "Configure as expert with full server awareness for a specific reasoning task",
    arguments: [
      {
        name: "task",
        description: "The reasoning task to solve using Prolog",
        required: true
      }
    ],
    messages: (args = {}) => [
      {
        role: "user",
        content: {
          type: "text",
          text: `You are a Prolog expert tackling this task: ${args.task || '[Please specify a reasoning task]'}

DISCOVERY PHASE (Critical - Do First):
1. Read 'capabilities' resource: Understand server limits, security model, and available features
2. Read 'help' resource: Review comprehensive usage guidelines and best practices
3. Check 'kb-predicates' resource: See what predicates are already defined
4. Review 'kb-dump' resource: Understand existing knowledge base content

PROLOG EXPERTISE:
- Syntax: facts(atom), rules(Head :- Body), queries(?- Goal)
- Unification and pattern matching with variables (X, Y, _)
- List operations: [H|T], append/3, member/2, length/2
- Backtracking control: cuts (!), deterministic predicates
- Built-ins: findall/3, bagof/3, setof/3, forall/2, once/1
- Arithmetic: is/2, =:=/2, >/2, between/3
- Meta-predicates: call/1-N, =../2, functor/3, arg/3
- DCGs: phrase/2, -->/2 for parsing and generation

SECURITY CONSTRAINTS (from capabilities):
- File access limited to ~/.swipl-mcp-server/ directory only
- Blocked predicates: shell(), system(), call(), assert(), halt()
- Safe environment: only kb module predicates and approved built-ins
- Use library predicates and user-defined predicates in kb module

OPTIMAL WORKFLOW:
1. Start by reading resources for full context
2. Check existing predicates with symbols_list tool
3. Design solution following these principles:
   - Write deterministic predicates when possible
   - Use cuts strategically to prevent unnecessary backtracking
   - Order goals from most restrictive to least restrictive
   - Prefer tail recursion for efficiency
   - Use findall/3 family for collecting solutions
4. Implement using db_assert_many for batch operations
5. Test with appropriate query mode (standard for simple, engine for complex backtracking)
6. Validate results and optimize as needed

Always prioritize correctness, then efficiency, following Prolog best practices.`
        }
      }
    ]
  },

  // Quick reference prompt
  quickReference: {
    name: "prolog_quick_reference",
    title: "Prolog Server Quick Reference Guide",
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
- meta://help: Usage guidelines and best practices
- meta://license: License information
- meta://capabilities: Server capabilities, security model, and constraints
- prolog://kb/predicates: Currently defined predicates in knowledge base
- prolog://kb/dump: Complete knowledge base content (facts and rules)

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

This will serve as a complete orientation to the server's capabilities.`
        }
      }
    ]
  },

  // Knowledge base builder with resource awareness
  kbBuilder: {
    name: "prolog_kb_builder",
    title: "Build Prolog Knowledge Base with Resource Guidance",
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
2. Check 'kb-predicates' resource: See what predicates already exist to avoid conflicts
3. Review 'kb-dump' resource: Understand current knowledge base state
4. Read 'help' resource: Get guidance on best practices

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
   - Use db_assert_many for efficient batch loading
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
   - Check with kb-dump resource to see final structure

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
    title: "Prolog Query Performance Optimizer",
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
1. Read 'kb-predicates' resource: Understand available predicates
2. Review 'kb-dump' resource: See data patterns and indexing opportunities
3. Check 'capabilities' resource: Understand system constraints

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
  }
};