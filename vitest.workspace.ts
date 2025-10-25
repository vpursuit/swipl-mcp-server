import { defineWorkspace } from 'vitest/config';

export default defineWorkspace([
  // Core package
  {
    extends: './vitest.config.ts',
    test: {
      name: 'mcp-core',
      include: ['packages/mcp-core/test/**/*.test.ts'],
      environment: 'node',
    },
  },
  // Roots package
  {
    extends: './vitest.config.ts',
    test: {
      name: 'mcp-roots',
      include: ['packages/mcp-roots/test/**/*.test.ts'],
      environment: 'node',
    },
  },
  // Prolog package
  {
    extends: './vitest.config.ts',
    test: {
      name: 'mcp-prolog',
      include: ['packages/mcp-prolog/test/**/*.test.ts'],
      environment: 'node',
    },
  },
  // Orchestrator package
  {
    extends: './vitest.config.ts',
    test: {
      name: 'swipl-mcp-server',
      include: ['packages/swipl-mcp-server/test/**/*.test.ts'],
      environment: 'node',
    },
  },
]);
