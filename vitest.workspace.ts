import { defineWorkspace } from 'vitest/config';

export default defineWorkspace([
  // Core package
  {
    test: {
      name: 'mcp-core',
      include: ['packages/mcp-core/test/**/*.test.ts'],
      environment: 'node',
      globals: true,
      testTimeout: 10000,
    },
  },
  // Roots package
  {
    test: {
      name: 'mcp-roots',
      include: ['packages/mcp-roots/test/**/*.test.ts'],
      environment: 'node',
      globals: true,
      testTimeout: 10000,
    },
  },
  // Prolog package
  {
    test: {
      name: 'mcp-prolog',
      include: ['packages/mcp-prolog/test/**/*.test.ts'],
      environment: 'node',
      globals: true,
      testTimeout: 10000,
    },
  },
  // Orchestrator package
  {
    test: {
      name: 'swipl-mcp-server',
      include: ['packages/swipl-mcp-server/test/**/*.test.ts'],
      environment: 'node',
      globals: true,
      testTimeout: 10000,
    },
  },
]);
