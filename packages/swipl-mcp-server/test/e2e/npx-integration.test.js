#!/usr/bin/env node

/**
 * Integration test for npx usage scenarios
 * 
 * This test simulates real-world usage via npx by:
 * 1. Creating a temporary package from dist/
 * 2. Installing it in a clean environment 
 * 3. Testing core functionality through the MCP protocol
 * 4. Verifying path resolution works correctly
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
    this.packagePath = null;
    this.timeout = 30000; // 30 second timeout
  }

  async setup() {
    console.log('🏗️  Setting up npx integration test...');
    
    // Create temporary directory
    this.tempDir = await fs.mkdtemp('/tmp/swipl-mcp-test-');
    console.log(`📁 Created temp directory: ${this.tempDir}`);
    
    // Use the exact same build process as production
    console.log('📦 Building package using production scripts...');
    await execAsync('npm run build:package', { cwd: ROOT_DIR });
    
    // Use the exact same pack process as production
    console.log('📤 Creating package tarball using production process...');
    const { stdout } = await execAsync('npm pack', { cwd: path.join(ROOT_DIR, 'dist') });
    const tarballName = stdout.trim();
    this.packagePath = path.join(ROOT_DIR, 'dist', tarballName);
    
    console.log(`✅ Package created: ${this.packagePath}`);
  }

  async installPackage() {
    console.log('📥 Installing locally built package...');
    
    // Install the exact package we just built (not from registry)
    // This ensures we test our actual changes
    await execAsync(`npm install "${this.packagePath}"`, {
      cwd: this.tempDir
    });
    
    console.log('✅ Locally built package installed successfully');
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
        name: 'license tool',
        request: { jsonrpc: '2.0', id: 2, method: 'tools/call', params: { name: 'license', arguments: {} } },
        validate: (result) => {
          if (!result.result?.content?.[0]?.text?.includes('BSD')) {
            throw new Error('License tool did not return expected BSD license text');
          }
          if (result.result?.structuredContent?.error === 'license_file_not_found') {
            throw new Error('LICENSE file not found - path resolution failed');
          }
        }
      },
      {
        name: 'knowledge_base_assert test',
        request: { jsonrpc: '2.0', id: 3, method: 'tools/call', params: { name: 'knowledge_base_assert', arguments: { fact: 'test_fact(hello)' } } },
        validate: (result) => {
          const responseText = result.result?.content?.[0]?.text || '';
          if (responseText.includes('Prolog server not started')) {
            throw new Error('Prolog server failed to start - prolog_server.pl path resolution failed');
          }
          if (responseText.includes('Prolog server script not found')) {
            throw new Error('prolog_server.pl not found - path resolution failed');
          }
          if (!responseText.includes('Result: ok') && !responseText.includes('Asserted 1/1 clauses successfully')) {
            throw new Error(`knowledge_base_assert did not succeed. Response: ${responseText}`);
          }
        }
      },
      {
        name: 'knowledge_base_load with non-existent file',
        request: { 
          jsonrpc: '2.0', 
          id: 4, 
          method: 'tools/call', 
          params: { name: 'knowledge_base_load', arguments: { filename: '/non/existent/file.pl' } } 
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          if (!text.includes('Security Error:') || !text.includes('Files can only be loaded from')) {
            throw new Error(`Expected security error for file outside allowed directory, got: ${text}`);
          }
        }
      },
      {
        name: 'query_next without active query',
        request: { 
          jsonrpc: '2.0', 
          id: 5, 
          method: 'tools/call', 
          params: { name: 'query_next', arguments: {} } 
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
          params: { name: 'query_start', arguments: { query: 'invalid_syntax(' } } 
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          if (!text.includes('Error:')) {
            throw new Error(`Expected syntax error for invalid query, got: ${text}`);
          }
        }
      },
      {
        name: 'knowledge_base_retract non-existent fact',
        request: { 
          jsonrpc: '2.0', 
          id: 7, 
          method: 'tools/call', 
          params: { name: 'knowledge_base_retract', arguments: { fact: 'non_existent_fact(x)' } } 
        },
        validate: (result) => {
          const text = result.result?.content?.[0]?.text || '';
          if (!text.includes('Retracted')) {
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
      const child = spawn('node', [path.join(this.tempDir, 'node_modules', '@vpursuit', 'swipl-mcp-server', 'lib', 'index.js')], {
        cwd: '/tmp',  // Clean directory, no access to development files
        env: {
          ...process.env,
          // Remove any paths that might lead back to development directory
          NODE_PATH: '',
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
    if (this.tempDir) {
      console.log('🧹 Cleaning up...');      
      await fs.rm(this.tempDir, { recursive: true, force: true });
      console.log('✅ Cleanup complete');
    }
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
