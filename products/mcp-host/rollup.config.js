import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';

export default {
  // Entry point
  input: 'src/index.ts',

  // Output configuration
  output: {
    file: 'dist/index.js',
    format: 'esm',
    sourcemap: true,
    // No shebang - this is a library, not a CLI tool
  },

  // Plugins (order matters!)
  plugins: [
    // 1. TypeScript compilation FIRST (USES OFFICIAL TSC!)
    // Must come before resolution so it processes entry point
    typescript({
      // Use dedicated tsconfig for Rollup bundling
      tsconfig: './tsconfig.rollup.json',

      // Use official TypeScript compiler
      typescript: (await import('typescript')).default,

      // Type checking enabled with rollup-specific config
      noEmitOnError: true,

      // Include only product source files
      include: ['src/**/*.ts'],
      exclude: ['node_modules', '**/*.test.ts'],
    }),

    // 2. Resolve node_modules (including TypeScript files)
    resolve({
      preferBuiltins: true,
      extensions: ['.ts', '.tsx', '.js', '.jsx', '.json'],
      // Don't resolve these (they're external)
      modulesOnly: false,
    }),

    // 3. Handle CommonJS modules
    commonjs(),

    // 4. Handle JSON imports
    json(),
  ],

  // External dependencies (don't bundle these)
  external: [
    // MCP SDK - runtime dependency
    '@modelcontextprotocol/sdk',
    // Also exclude any sub-paths
    /^@modelcontextprotocol\/sdk\//,
    // Node built-ins
    'fs',
    'path',
    'child_process',
    'stream',
    'util',
    'events',
    'crypto',
    'readline',
    'os',
    'url',
    'http',
    'https',
    'net',
    'tls',
  ],

  // Suppress warnings
  onwarn(warning, warn) {
    // Ignore circular dependency warnings from some packages
    if (warning.code === 'CIRCULAR_DEPENDENCY') return;
    // Use default warning handler for other warnings
    warn(warning);
  },
};
