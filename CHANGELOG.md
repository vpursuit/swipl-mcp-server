# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [3.0.1-beta.8] - 2025-11-04

### BREAKING CHANGES

- **query_next iterator pattern**: Removed `more_solutions` boolean field in favor of standard iterator pattern
  - Response now uses `status` enum field: `"success"` when solution available, `"done"` when exhausted
  - Response format: `{solution: string | null, status: "success" | "done", processing_time_ms: number}`
  - Migration: Replace `if (!result.more_solutions)` with `if (result.status === "done")`
  - Affects: All code using `query_next` tool responses (both standard and engine modes)
  - Benefits: Standard iterator pattern familiar to developers, eliminates ambiguity about solution availability

### Documentation

- Updated all examples to demonstrate standard iterator pattern with `status` field checking
- Enhanced help tool, README files, and feature documentation with iterator usage examples

## [3.0.1-beta.7] - 2025-11-03

### Bug Fixes

- fix prompt capability advertisement to enable MCP client access (products/swipl-mcp-server/src/index.ts:67-72)
  - Added `prompts: {}` to server capabilities object
  - Prompts were registered but not advertised, preventing protocol-compliant clients from discovering them
  - MCP Inspector could still access prompts by directly probing endpoints

## [3.0.1-beta.4] - 2025-11-03

### Features

- add automated changelog and release notes system (2f3dd34)
- add strict npm signature verification and document provenance (b8cb3e3)

## [3.0.1-beta.3] - 2025-11-03

### Performance Improvements

- optimize CI/CD workflow by removing redundant builds (dc81e80)

## [3.0.1-beta.1] - 2025-11-03

### Bug Fixes

- prevent test interference by disabling concurrent execution (7e5bc1a)
- force exit event if SIGKILL doesn't work (47d66ad)
- add hard timeout to stop() to prevent indefinite hangs (f54dcbc)
- add cleanup delays to prevent CI test timeout (74be1d1)
- improve release scripts to ensure tags are created and pushed (cfc79ef)

## [3.0.1-beta.0] - 2025-11-02

### Features

- restore version and release management scripts (cd42fd1)
- migrate to products/plugins architecture and Rollup bundler (7e89b2e)
- add MCP SDK enhancements for better UX and type safety (88c0cda)
- enable safe library loading with two-module architecture (590c02b)
- enhance assert/retract response formatting for better readability (7d4b138)
- restore missing inspect and server scripts (e47648f)
- modernize monorepo structure and publishing workflow (eb12013)
- create @vpursuit/mcp-roots plugin (9f65a13)
- create @vpursuit/mcp-core plugin system (6918c63)
- create monorepo workspace structure (5ce7a6e)

### Bug Fixes

- replace workspace:* with * for npm compatibility (4654b9d)
- upgrade npm to v11 in all workflow jobs for workspace protocol support (39d2704)
- refine publish workflow with pre-release support and SWI-Prolog (a6edb96)
- improve timeout recovery and process management (1b9d295)
- implement circuit breaker and recovery for timeout degradation (63067b4)
- prevent backtracking to fallback dispatch after assert validation errors (4c534c9)
- update capabilities outputSchema to match actual return structure (efab327)
- enforce type safety and resolve async lifecycle issues (6d57762)
- resolve root removal notification and logging issues (9d22677)
- resolve vitest deprecation warning and update dependencies (26f1200)
- enable npx pre-publish testing with local tarballs (467e08b)

## [2.0.1] - 2025-09-16

### Bug Fixes

- correct prompt parameter schemas for proper CLI integration (9fc451b)

