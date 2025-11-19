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
      // Core plugin
      {
        test: {
          name: 'mcp-server-core',
          include: ['plugins/server/core/test/**/*.test.ts'],
          environment: 'node',
          globals: true,
          testTimeout: 10000,
        },
      },
      // Roots plugin
      {
        test: {
          name: 'mcp-server-roots',
          include: ['plugins/server/roots/test/**/*.test.ts'],
          environment: 'node',
          globals: true,
          testTimeout: 10000,
        },
      },
      // Prolog plugin
      {
        test: {
          name: 'mcp-server-prolog',
          include: [
            'plugins/server/prolog/test/**/*.test.ts',
            'plugins/server/prolog/test/**/*.test.js',
          ],
          environment: 'node',
          globals: true,
          testTimeout: 60000,
          setupFiles: ['./plugins/server/prolog/test/setup.js'],
          // Run tests sequentially - they share a singleton PrologInterface
          fileParallelism: false,
        },
      },
      // Server product
      {
        test: {
          name: 'swipl-mcp-server',
          include: [
            'products/swipl-mcp-server/test/**/*.test.ts',
            'products/swipl-mcp-server/test/**/*.test.js',
          ],
          exclude: ['products/swipl-mcp-server/test/e2e/npx-integration.test.js'],
          environment: 'node',
          globals: true,
          testTimeout: 60000,
          setupFiles: ['./products/swipl-mcp-server/test/setup.js'],
        },
      },
      // MCP Host Client
      {
        test: {
          name: 'mcp-client-core',
          include: ['plugins/client/mcp-host/test/**/*.test.ts'],
          environment: 'node',
          globals: true,
          testTimeout: 10000,
        },
      },
    ]
  }
});