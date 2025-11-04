Contributing
============

Thanks for your interest in improving the SWI‑Prolog MCP Server! This guide covers local setup, workflow, and what to check before opening a pull request.

Prerequisites
-------------
- Node.js ≥ 20.0.0
- SWI‑Prolog installed and on PATH (run: swipl --version)
- macOS/Linux recommended (Windows WSL works too)

Setup
-----
- Clone: git clone https://github.com/vpursuit/model-context-lab.git
- Install: cd model-context-lab && npm install

Quick Dev Loop
--------------
- Build then run (recommended): npm run server
- Or: npm run build && npm start
- Inspect via MCP Inspector (stdio): npx @modelcontextprotocol/inspector node dist/index.js

Workflow Checklist (Before PR)
------------------------------
- Sanity
  - Confirm swipl --version works locally
  - Run unit tests: npm test (and optionally npm run test:coverage)
  - If you changed Prolog integration, sessions, or protocol, verify with MCP Inspector

- Build & Package
  - **Development workflow**: `npm run build` → `npm pack` → `npm install -g *.tgz`
  - **Production workflow**: `npm run build:package` → `npm pack dist/` → inspect tarball contents
  - **Live development**: Use `npm link` for immediate changes without rebuilding

- Changes that need extra attention
  - Tool schemas/protocol
    - Update src/schemas.ts (zod + JSON schemas) and ensure src/index.ts registers JSON schemas
    - Update README.md if inputs/outputs or protocol envelopes change
    - Add/adjust tests (e.g., test/jsonSchemas.test.ts, tool tests) to cover new shapes
  - Tool handlers
    - Keep responses text-first and add a JSON content item second (for structured clients)
    - Update tests that parse text or expect specific ordering
  - Security-impacting changes
    - Call out blacklist/sandbox/engine changes
    - Update docs/SECURITY.md as needed

- Hygiene
  - Follow TypeScript strict mode and style (2 spaces, semicolons, double quotes, relative .js imports in src/)
  - Avoid any; where unavoidable (e.g., SDK typing gap for JSON schemas), add a brief note
  - Never commit secrets, credentials, or machine-specific paths

- Commit/PR
  - Commit messages: short, imperative (e.g., "Fix engine session cleanup")
  - PRs: clear description, linked issues, repro steps; attach logs/Inspector screenshots for engine/session changes

Release
-------
- Maintainers publish the package. prepublishOnly runs build:package to create a minimal dist
- To test a release candidate locally: `npm pack dist/` and `cd dist && npm link`
- To test development changes: `npm run build && npm pack && npm install -g *.tgz`

Thank you for contributing! 🙌
