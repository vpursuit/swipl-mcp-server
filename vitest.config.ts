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
    globals: true // Enable Jest-compatible globals
  }
});