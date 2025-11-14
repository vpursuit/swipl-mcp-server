#!/usr/bin/env node

/**
 * Integration test for npx usage scenarios
 *
 * This test simulates real-world usage via npx by:
 * 1. Verifying the product is built (dist/ directory exists)
 * 2. Using a pre-existing tarball (created by 'npm pack')
 * 3. Installing the tarball in a clean /tmp environment
 * 4. Testing MCP protocol functionality
 * 5. Verifying path resolution and security model
 *
 * PREREQUISITES:
 * 1. Run 'npm run build' in products/swipl-mcp-server/ to build the product
 * 2. Run 'npm pack' in products/swipl-mcp-server/ to create the tarball
 *
 * This test uses the REAL product tarball (same artifact published to npm).
 * It does NOT rebuild or repack - it uses existing build artifacts.
 * Workflow: build → pack → test (matching CI/CD pattern)
 */

import { exec, spawn } from 'child_process';
import { promisify } from 'util';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const execAsync = promisify(exec);
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = path.resolve(__dirname, '../../../..');

class NPXIntegrationTest {
  constructor() {
    this.tempDir = null;
    this.testFilesDir = null; // Directory for test Prolog files (allowed root)
    this.tarballPath = null; // Path to the product tarball
    this.timeout = 30000; // 30 second timeout
  }

  async setup() {
    console.log('🏗️  Setting up npx integration test...');

    // Create temporary directory
    this.tempDir = await fs.mkdtemp('/tmp/swipl-mcp-test-');
    console.log(`📁 Created temp directory: ${this.tempDir}`);

    // Create temporary directory for test Prolog files (used as allowed root)
    this.testFilesDir = path.join(this.tempDir, 'test-prolog-files');
    await fs.mkdir(this.testFilesDir, { recursive: true });
    console.log(`📁 Created test files directory: ${this.testFilesDir}`);

    // Verify product is built
    // NOTE: Run 'npm run build' before running this test
    console.log('🔍 Verifying product is built...');
    const productDir = path.join(ROOT_DIR, 'products/swipl-mcp-server');
    const distDir = path.join(productDir, 'dist');

    try {
      await fs.access(distDir);
      console.log('✅ Product is built');
    } catch (err) {
      throw new Error(
        `Product is not built. Missing: ${distDir}\n` +
        `Run 'npm run build' in products/swipl-mcp-server/ first.`
      );
    }

    // Verify tarball exists (should be created by 'npm pack')
    console.log('🔍 Looking for product tarball...');
    const files = await fs.readdir(productDir);
    const tarball = files.find(f => f.match(/^vpursuit-swipl-mcp-server-.*\.tgz$/));

    if (!tarball) {
      throw new Error(
        `Tarball not found. Expected pattern: vpursuit-swipl-mcp-server-*.tgz\n` +
        `In directory: ${productDir}\n` +
        `Run 'npm pack' in products/swipl-mcp-server/ first.`
      );
    }

    this.tarballPath = path.join(productDir, tarball);
    console.log(`✅ Found tarball: ${tarball}`);
  }

  async installPackage() {
    console.log('📥 Installing product from tarball...');

    // Initialize minimal package.json to ensure npm install isolation
    // This prevents npm from traversing to parent directories
    await fs.writeFile(
      path.join(this.tempDir, 'package.json'),
      JSON.stringify({ name: 'test-install', version: '1.0.0', private: true }, null, 2)
    );

    // Install the single product tarball
    // npm will automatically fetch @modelcontextprotocol/sdk from registry
    console.log(`  📦 Installing ${path.basename(this.tarballPath)}...`);
    await execAsync(`npm install "${this.tarballPath}"`, {
      cwd: this.tempDir
    });

    console.log('✅ Package installed successfully');
  }

  async testMCPProtocol() {
    console.log('🔧 Testing MCP protocol functionality...');
    
    // Initialize server first
    const initResult = await this.runMCPCommand({
      jsonrpc: '2.0', 
      id: 1, 
      method: 'initialize', 
      params: {
        protocolVersion: '1.0.0',
        capabilities: {},
        clientInfo: { name: 'test', version: '1.0.0' }
      }
    });
    
    // Check version is correct (not "0.0.0")
    const version = initResult.result?.serverInfo?.version;
    console.log(`🔍 Server version: ${version}`);
    if (version === "0.0.0") {
      console.log('⚠️  WARNING: Server version is 0.0.0 - package.json path resolution may be incorrect');
    }
    
    const tests = [
      {
        name: 'clauses assert test',
        request: { jsonrpc: '2.0', id: 3, method: 'tools/call', params: { name: 'clauses', arguments: { operation: 'assert', clauses: 'test_fact(hello)' } } },
        validate: (result) => {
          const responseText = result.result?.content?.[0]?.text || '';
          if (responseText.includes('Prolog server not started')) {
            throw new Error('Prolog server failed to start - prolog_server.pl path resolution failed');
          }
          if (responseText.includes('Prolog server script not found')) {
            throw new Error('prolog_server.pl not found - path resolution failed');
          }
          // Check for success in structuredContent
          const success = result.result?.structuredContent?.success;
          if (success !== 1) {
            throw new Error(`clauses assert did not succeed. Response: ${responseText}`);
          }
        }
      },
      {
        name: 'files import with non-existent file',
        request: {
          jsonrpc: '2.0',
          id: 4,
          method: 'tools/call',
          params: { name: 'files', arguments: { operation: 'import', filename: '/non/existent/file.pl' } }
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          if (!text.includes('Security Error:') || !text.includes('File must be within allowed roots')) {
            throw new Error(`Expected security error for file outside allowed directory, got: ${text}`);
          }
        }
      },
      {
        name: 'query next without active query',
        request: {
          jsonrpc: '2.0',
          id: 5,
          method: 'tools/call',
          params: { name: 'query', arguments: { operation: 'next' } }
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          if (!text.includes('No active query')) {
            throw new Error(`Expected "No active query" error, got: ${text}`);
          }
        }
      },
      {
        name: 'invalid query syntax',
        request: {
          jsonrpc: '2.0',
          id: 6,
          method: 'tools/call',
          params: { name: 'query', arguments: { operation: 'start', query: 'invalid_syntax(' } }
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          if (!text.includes('Error:')) {
            throw new Error(`Expected syntax error for invalid query, got: ${text}`);
          }
        }
      },
      {
        name: 'clauses retract non-existent fact',
        request: {
          jsonrpc: '2.0',
          id: 7,
          method: 'tools/call',
          params: { name: 'clauses', arguments: { operation: 'retract', clauses: 'non_existent_fact(x)' } }
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          // Retract should succeed even if fact doesn't exist (returns 0 retractions)
          const success = result.result?.structuredContent?.success;
          if (success === undefined) {
            throw new Error(`Expected retraction response, got: ${text}`);
          }
        }
      }
    ];

    const results = [];
    
    for (const test of tests) {
      try {
        console.log(`  ⏳ Testing ${test.name}...`);
        const result = await this.runMCPCommand(test.request);
        
        // Basic validation
        if (result.error) {
          throw new Error(`MCP error: ${JSON.stringify(result.error)}`);
        }
        
        // Specific validation
        if (test.validate) {
          test.validate(result);
        }
        
        results.push({ ...test, success: true, result });
        console.log(`  ✅ ${test.name} - OK`);
        
      } catch (error) {
        results.push({ ...test, success: false, error: error.message });
        console.log(`  ❌ ${test.name} - FAILED: ${error.message}`);
      }
    }
    
    return results;
  }

  async runMCPCommand(request) {
    return new Promise((resolve, reject) => {
      // Run from clean directory that doesn't have source files
      // This simulates real user environment
      const child = spawn('node', [path.join(this.tempDir, 'node_modules', '@vpursuit', 'swipl-mcp-server', 'dist', 'index.js')], {
        cwd: '/tmp',  // Clean directory, no access to development files
        env: {
          ...process.env,
          // Remove any paths that might lead back to development directory
          NODE_PATH: '',
          // Configure allowed roots to avoid listRoots() timeout
          // The test runs without a real MCP client, so we must use env var
          SWI_MCP_ALLOWED_ROOTS: this.testFilesDir,
          SWI_MCP_READY_TIMEOUT_MS: '10000',
          SWI_MCP_QUERY_TIMEOUT_MS: '5000',
          DEBUG: 'swipl-mcp-server',  // Enable our debug logging
          SWI_MCP_TRACE: '1'
        },
        stdio: ['pipe', 'pipe', 'pipe']
      });

      let stdout = '';
      let stderr = '';
      let responded = false;

      const timeout = setTimeout(() => {
        if (!responded) {
          child.kill();
          reject(new Error('Command timeout'));
        }
      }, this.timeout);

      child.stdout.on('data', (data) => {
        const text = data.toString();
        stdout += text;
        // Also log stdout for debugging
        if (text.trim()) {
          console.log('🐛 STDOUT:', text.trim());
        }
        // Look for JSON-RPC response
        const lines = stdout.split('\n');
        for (const line of lines) {
          if (line.trim() && line.includes('"jsonrpc"')) {
            try {
              const response = JSON.parse(line.trim());
              if (response.id === request.id) {
                console.log('🐛 RESPONSE:', JSON.stringify(response, null, 2));
                clearTimeout(timeout);
                responded = true;
                child.kill();
                resolve(response);
                return;
              }
            } catch (e) {
              // Not a valid JSON response, continue
            }
          }
        }
      });

      child.stderr.on('data', (data) => {
        const text = data.toString();
        stderr += text;
        // Log ALL stderr output for debugging
        if (text.trim()) {
          console.log('🐛 STDERR:', text.trim());
        }
      });

      child.on('error', (error) => {
        clearTimeout(timeout);
        if (!responded) {
          responded = true;
          reject(new Error(`Process error: ${error.message}\nstderr: ${stderr}`));
        }
      });

      child.on('exit', (code) => {
        clearTimeout(timeout);
        if (!responded) {
          responded = true;
          if (code !== 0) {
            reject(new Error(`Process exited with code ${code}\nstderr: ${stderr}`));
          } else {
            reject(new Error(`Process exited without sending response\nstdout: ${stdout}\nstderr: ${stderr}`));
          }
        }
      });

      // Send the request (keep stdin open to avoid premature disconnect)
      child.stdin.write(JSON.stringify(request) + '\n');
    });
  }

  async cleanup() {
    console.log('🧹 Cleaning up...');

    // Remove temporary test directory
    if (this.tempDir) {
      await fs.rm(this.tempDir, { recursive: true, force: true });
      console.log(`  🗑️  Removed temp directory: ${path.basename(this.tempDir)}`);
    }

    // Note: Tarball is kept in products/swipl-mcp-server/ for reuse
    // Run 'rm products/swipl-mcp-server/*.tgz' to clean manually if needed

    console.log('✅ Cleanup complete');
  }

  async run() {
    try {
      await this.setup();
      await this.installPackage();
      const results = await this.testMCPProtocol();
      
      console.log('\n📊 Test Results Summary:');
      const passed = results.filter(r => r.success).length;
      const total = results.length;
      
      results.forEach(result => {
        const status = result.success ? '✅' : '❌';
        console.log(`  ${status} ${result.name}`);
        if (!result.success) {
          console.log(`    Error: ${result.error}`);
        }
      });
      
      console.log(`\n${passed}/${total} tests passed`);
      
      if (passed === total) {
        console.log('🎉 All npx integration tests passed!');
        return true;
      } else {
        console.log('💥 Some npx integration tests failed!');
        return false;
      }
      
    } finally {
      await this.cleanup();
    }
  }
}

// Run the test if this file is executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const test = new NPXIntegrationTest();
  test.run()
    .then(success => process.exit(success ? 0 : 1))
    .catch(error => {
      console.error('💥 Test failed with error:', error);
      process.exit(1);
    });
}

export { NPXIntegrationTest };
