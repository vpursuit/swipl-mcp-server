import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * Workspace resolver for Rollup
 * Maps @vpursuit/* packages to their source files
 * (Same concept as esbuild-workspace-resolver.js)
 */
function workspaceResolver() {
  const workspaceMap = {};

  // Scan node_modules/@vpursuit for workspace symlinks
  const vpursuitDir = path.resolve(__dirname, '../../node_modules/@vpursuit');

  if (fs.existsSync(vpursuitDir)) {
    for (const entry of fs.readdirSync(vpursuitDir)) {
      const symlinkPath = path.join(vpursuitDir, entry);
      const stats = fs.lstatSync(symlinkPath);

      if (stats.isSymbolicLink()) {
        const realPath = fs.realpathSync(symlinkPath);
        const srcPath = path.join(realPath, 'src/index.ts');

        if (fs.existsSync(srcPath)) {
          workspaceMap[`@vpursuit/${entry}`] = srcPath;
        }
      }
    }
  }

  return {
    name: 'workspace-resolver',
    resolveId(source) {
      // Resolve workspace packages to source files
      if (workspaceMap[source]) {
        return workspaceMap[source];
      }
      return null; // Let other plugins handle it
    }
  };
}

export default {
  // Entry point
  input: 'src/index.ts',

  // Output configuration
  output: {
    file: 'dist/index.js',
    format: 'esm',
    sourcemap: true,
    banner: '#!/usr/bin/env node',
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

      // Explicitly include TypeScript files
      include: ['src/**/*.ts', '../../plugins/server/*/src/**/*.ts'],
      exclude: ['node_modules', '**/*.test.ts'],
    }),

    // 2. Resolve workspace packages
    workspaceResolver(),

    // 3. Resolve node_modules
    resolve({
      preferBuiltins: true,
      extensions: ['.ts', '.js', '.json'],
    }),

    // 4. Handle CommonJS modules
    commonjs(),

    // 5. Handle JSON imports
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
  ],

  // Suppress warnings
  onwarn(warning, warn) {
    // Ignore circular dependency warnings from some packages
    if (warning.code === 'CIRCULAR_DEPENDENCY') return;
    // Use default warning handler for other warnings
    warn(warning);
  },
};

console.log('✅ Rollup configuration loaded (using official TypeScript compiler)');
