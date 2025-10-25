import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'node',
    testTimeout: 10000,
    setupFiles: ['./test/setup.js'],
    include: ['test/**/*.test.ts'],
    coverage: {
      include: ['src/**/*.ts'],
      exclude: ['src/**/*.d.ts'],
      reporter: ['text', 'html'],
      reportOnFailure: true,
      thresholds: {
        statements: 60,
        branches: 60,
        functions: 60,
        lines: 60
      }
    },
    globals: true, // Enable Jest-compatible globals
    // Projects configuration (replaces deprecated workspace file)
    projects: [
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
          include: [
            'packages/mcp-prolog/test/**/*.test.ts',
            'packages/mcp-prolog/test/**/*.test.js',
          ],
          environment: 'node',
          globals: true,
          testTimeout: 60000,
          setupFiles: ['./packages/mcp-prolog/test/setup.js'],
        },
      },
      // Orchestrator package
      {
        test: {
          name: 'swipl-mcp-server',
          include: [
            'packages/swipl-mcp-server/test/**/*.test.ts',
            'packages/swipl-mcp-server/test/**/*.test.js',
          ],
          exclude: ['packages/swipl-mcp-server/test/e2e/npx-integration.test.js'],
          environment: 'node',
          globals: true,
          testTimeout: 60000,
          setupFiles: ['./packages/swipl-mcp-server/test/setup.js'],
        },
      },
    ]
  }
});