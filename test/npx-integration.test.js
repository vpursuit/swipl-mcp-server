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
const ROOT_DIR = path.resolve(__dirname, '..');

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
    
    // Build the package
    console.log('📦 Building package...');
    await execAsync('node scripts/build-package.js', { cwd: ROOT_DIR });
    
    // Create package tarball
    console.log('📤 Creating package tarball...');
    const { stdout } = await execAsync('npm pack', { cwd: path.join(ROOT_DIR, 'dist') });
    const tarballName = stdout.trim();
    this.packagePath = path.join(ROOT_DIR, 'dist', tarballName);
    
    console.log(`✅ Package created: ${this.packagePath}`);
  }

  async installPackage() {
    console.log('📥 Installing package in test environment...');
    
    // Install the package globally in the temp directory
    await execAsync(`npm install -g "${this.packagePath}"`, {
      cwd: this.tempDir,
      env: {
        ...process.env,
        NPM_CONFIG_PREFIX: this.tempDir,
        PATH: `${this.tempDir}/bin:${process.env.PATH}`
      }
    });
    
    console.log('✅ Package installed successfully');
  }

  async testMCPProtocol() {
    console.log('🔧 Testing MCP protocol functionality...');
    
    const tests = [
      {
        name: 'capabilities',
        request: { jsonrpc: '2.0', id: 1, method: 'initialize', params: { protocolVersion: '1.0.0', capabilities: {} } }
      },
      {
        name: 'tools/list',
        request: { jsonrpc: '2.0', id: 2, method: 'tools/list' }
      },
      {
        name: 'license tool',
        request: { jsonrpc: '2.0', id: 3, method: 'tools/call', params: { name: 'license', arguments: {} } }
      },
      {
        name: 'help tool',
        request: { jsonrpc: '2.0', id: 4, method: 'tools/call', params: { name: 'help', arguments: {} } }
      },
      {
        name: 'db_assert test',
        request: { jsonrpc: '2.0', id: 5, method: 'tools/call', params: { name: 'db_assert', arguments: { fact: 'test_fact(hello_world)' } } }
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
        
        // Specific validations
        if (test.name === 'license tool') {
          if (!result.result?.content?.[0]?.text?.includes('BSD')) {
            throw new Error('License tool did not return expected BSD license text');
          }
        }
        
        if (test.name === 'db_assert test') {
          if (result.result?.content?.[0]?.text?.includes('Prolog server not started')) {
            throw new Error('Prolog server failed to start');
          }
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
      const child = spawn('npx', ['@vpursuit/swipl-mcp-server'], {
        cwd: this.tempDir,
        env: {
          ...process.env,
          NPM_CONFIG_PREFIX: this.tempDir,
          PATH: `${this.tempDir}/bin:${process.env.PATH}`,
          SWI_MCP_READY_TIMEOUT_MS: '10000',
          SWI_MCP_QUERY_TIMEOUT_MS: '5000'
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
        stdout += data.toString();
        // Look for JSON-RPC response
        const lines = stdout.split('\n');
        for (const line of lines) {
          if (line.trim() && line.includes('"jsonrpc"')) {
            try {
              const response = JSON.parse(line.trim());
              if (response.id === request.id) {
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
        stderr += data.toString();
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

      // Send the request
      child.stdin.write(JSON.stringify(request) + '\n');
      child.stdin.end();
    });
  }

  async cleanup() {
    if (this.tempDir) {
      console.log('🧹 Cleaning up...');
      try {
        await execAsync(`npm uninstall -g @vpursuit/swipl-mcp-server`, {
          cwd: this.tempDir,
          env: {
            ...process.env,
            NPM_CONFIG_PREFIX: this.tempDir
          }
        });
      } catch (e) {
        // Ignore cleanup errors
      }
      
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