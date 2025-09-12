# Implementation Plan for Code Review Fixes

## Overview
This plan addresses the issues identified in the code review of the SWI-Prolog MCP Server, organized by milestones rather than weekly schedules. Each milestone represents a complete, testable improvement to the codebase.

## Milestone 1: Critical Stability Fixes
**Goal**: Resolve memory leaks and race conditions that could cause system failures

### Tasks:
1. **Fix Memory Leak in Response Buffer** (`src/PrologInterface.ts:88`)
   - Remove unused `responseBuffer: string[]` property
   - Add `MAX_BUFFER_SIZE = 1024 * 1024` constant
   - Implement buffer overflow protection in `handleResponse()`
   - Add automatic truncation with error logging when buffer exceeds limit

2. **Fix Command Queue Race Condition** (`src/PrologInterface.ts:547-555`)
   - Refactor error handling to isolate failures per command
   - Modify queue chain to prevent error propagation between commands
   - Ensure queue remains functional after individual command failures

3. **Optimize String Processing** (`src/PrologInterface.ts:255-256`)
   - Replace O(n²) `split("\n")` and `pop()` pattern
   - Implement O(n) indexOf-based line processing
   - Reduce memory allocations during response parsing

### Success Criteria:
- No memory growth under sustained load
- Command queue continues after individual failures
- Response parsing time reduced by >50% for large outputs

### Tests to Add:
- Memory leak detection test with 10MB responses
- Queue resilience test with failing commands
- Performance benchmark for large response parsing

---

## Milestone 2: Correctness and Consistency
**Goal**: Fix logical inconsistencies and standardize behavior across the codebase

### Tasks:
1. **Fix Timeout Logic** (`src/PrologInterface.ts:535`)
   - Add `DEFAULT_QUERY_TIMEOUT_MS = 30000` constant
   - Fix fallback to use query-specific default, not `DEFAULT_READY_TIMEOUT_MS`
   - Document timeout hierarchy in code comments

2. **Standardize Input Validation**
   - Create validation constants:
     - `MAX_QUERY_LENGTH = 5000`
     - `MAX_FILENAME_LENGTH = 1000`
   - Apply consistent validation across all tool handlers
   - Create `validateInput()` utility function

3. **Simplify Promise Structure** (`src/PrologInterface.ts:482-555`)
   - Reduce nested promise complexity
   - Remove redundant wrapper functions
   - Clarify error propagation paths

4. **Session State Documentation** (`src/PrologInterface.ts:97`)
   - Add state transition diagram in comments
   - Validate all state transitions
   - Add guards against invalid transitions

### Success Criteria:
- All timeouts use appropriate defaults
- Input validation consistent across all endpoints
- Promise chains simplified without breaking functionality
- State transitions fully documented and validated

### Tests to Add:
- Timeout edge case testing
- Input validation boundary testing
- State transition coverage tests

---

## Milestone 3: Code Quality and Maintainability
**Goal**: Reduce duplication, improve readability, and establish patterns for future development

### Tasks:
1. **Extract Common Patterns**
   - Create `src/utils/response.ts` with:
     ```typescript
     function createToolResponse(
       result: { success: boolean; data?: any; error?: string },
       startTime: number
     ): ToolResponse
     ```
   - Refactor all tool handlers to use common utility
   - Remove duplicated error handling code

2. **Centralize Constants**
   - Create `src/constants.ts` for protocol strings:
     - `READY_MARK = "@@READY@@"`
     - `TERM_SOLUTION = "solution("`
     - `TERM_ERROR = "error("`
     - `NO_MORE_SOLUTIONS = "no_more_solutions"`
   - Replace all magic strings with constants

3. **Standardize Error Handling**
   - Document error handling strategy
   - Create consistent error response format
   - Ensure all handlers follow same pattern

### Success Criteria:
- Code duplication reduced by >40%
- All magic strings replaced with constants
- Consistent error handling across all endpoints

### Tests to Add:
- Unit tests for utility functions
- Error response format validation tests

---

## Milestone 4: Performance Optimization
**Goal**: Implement monitoring and optimization for production readiness

### Tasks:
1. **Buffer Management**
   - Add `MAX_QUERY_PROMISES = 100` limit
   - Implement `cleanupExpiredPromises()` method
   - Add memory usage tracking

2. **Performance Metrics**
   - Create performance tracking:
     ```typescript
     interface PerformanceMetrics {
       queryStartTime: number;
       responseParseTime: number;
       commandQueueWait: number;
       bufferHighWaterMark: number;
     }
     ```
   - Add timing measurements to critical paths
   - Create performance logging utilities

3. **Async File Operations**
   - Convert `validateFilePath()` to use async fs operations
   - Prevent event loop blocking

### Success Criteria:
- Memory usage stable under load
- Performance metrics available for monitoring
- No event loop blocking on file operations

### Tests to Add:
- Load testing with 100+ concurrent queries
- Memory usage monitoring tests
- Event loop lag detection tests

---

## Milestone 5: Comprehensive Testing
**Goal**: Ensure reliability through extensive testing

### Tasks:
1. **Load Testing Suite** (`test/integration/load.test.ts`)
   - Test with 1000 queries per minute
   - Monitor memory usage over time
   - Verify cleanup mechanisms

2. **Concurrent Access Testing** (`test/integration/concurrent.test.ts`)
   - Test parallel command execution
   - Verify queue serialization
   - Test session mutual exclusion

3. **Performance Benchmarks** (`test/unit/performance.test.ts`)
   - Establish baseline metrics
   - Test degradation under load
   - Verify optimization improvements

4. **Security Validation** (`test/integration/security-extended.test.ts`)
   - Additional path traversal attempts
   - Buffer overflow attempts
   - Command injection tests

### Success Criteria:
- All new tests passing
- No performance regression
- Security model remains intact
- Code coverage >90%

---

## Implementation Approach

### For Each Milestone:
1. Create feature branch from main
2. Implement changes with tests
3. Run full test suite
4. Code review
5. Merge to main

### Priority Order:
1. **Milestone 1** - Critical (blocks production use)
2. **Milestone 2** - High (affects correctness)
3. **Milestone 3** - Medium (improves maintainability)
4. **Milestone 4** - Medium (enhances monitoring)
5. **Milestone 5** - Low (increases confidence)

### Risk Mitigation:
- Keep security features untouched
- Maintain backward compatibility
- Document all breaking changes
- Create rollback plan for each milestone
- Monitor production metrics if deployed

## Tracking Progress
Each milestone completion should be marked in this document with:
- Completion date
- Actual vs expected outcomes
- Any deviations from plan
- Lessons learned

## Next Steps
1. Review and approve this plan
2. Create feature branch for Milestone 1
3. Begin implementation of critical fixes
4. Schedule code review after each milestone