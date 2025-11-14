/**
 * NPX Integration Tests
 *
 * This test runs the full end-to-end NPX integration scenario within the Vitest framework.
 * It builds the package, installs it in isolation, and tests MCP protocol functionality.
 *
 * This ensures the package works correctly when installed via npm/npx in real environments.
 *
 * TESTING STRATEGY:
 * - In monorepo dev: Uses local tarballs for all workspace dependencies
 * - Post-publish: Can optionally test against npm registry versions
 * - This allows testing npx functionality BEFORE publishing to npm
 */

import { describe, beforeEach, afterEach, test, expect } from "vitest";
import { NPXIntegrationTest } from "./npx-integration.test.js";

// Run tests if SWI-Prolog is available (uses local tarballs in monorepo)
const maybeDescribe = (globalThis as any).HAS_SWIPL ? describe : describe.skip;

maybeDescribe("NPX Integration Tests", () => {
  let npxTest: NPXIntegrationTest;

  beforeEach(() => {
    npxTest = new NPXIntegrationTest();
  });

  afterEach(async () => {
    // Ensure cleanup happens even if test fails
    if (npxTest) {
      await npxTest.cleanup();
    }
  });

  test("should pass all NPX integration scenarios", async () => {
    // This runs the full end-to-end test:
    // 1. Build package using production scripts
    // 2. Create tarball and install in isolated environment  
    // 3. Test MCP protocol functionality via spawned processes
    // 4. Validate path resolution (LICENSE, prolog_server.pl, package.json)
    // 5. Ensure package works via npx (real-world simulation)
    
    const success = await npxTest.run();
    expect(success).toBe(true);
  }, 120000); // 2 minute timeout for full end-to-end test including build and install

  test("should validate package structure and path resolution", async () => {
    // Test individual components of the NPX integration
    await npxTest.setup();
    await npxTest.installPackage();

    // Test that capabilities tool works (validates package is properly loaded)
    const capabilitiesTest = {
      name: 'capabilities tool validation',
      request: {
        jsonrpc: '2.0',
        id: 1,
        method: 'tools/call',
        params: { name: 'capabilities', arguments: {} }
      },
      validate: (result: any) => {
        if (!result.result?.structuredContent?.server?.name) {
          throw new Error('Capabilities tool did not return expected server info');
        }
        if (result.result?.structuredContent?.server?.name !== 'swipl-mcp-server') {
          throw new Error('Server name mismatch - package.json path resolution may be incorrect');
        }
      }
    };

    const response = await npxTest.runMCPCommand(capabilitiesTest.request);
    capabilitiesTest.validate(response);

    expect(response.result?.structuredContent?.server?.name).toBe('swipl-mcp-server');
  }, 90000); // 90 second timeout for setup and single test

  test("should start Prolog server correctly in installed environment", async () => {
    await npxTest.setup();
    await npxTest.installPackage();

    // Test that Prolog server can start and execute basic operations
    const prologTest = {
      name: 'prolog server startup validation',
      request: {
        jsonrpc: '2.0',
        id: 2,
        method: 'tools/call',
        params: { name: 'clauses', arguments: { operation: 'assert', clauses: 'test_npx_fact(success)' } }
      },
      validate: (result: any) => {
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
          throw new Error(`clauses tool did not succeed. Response: ${responseText}`);
        }
      }
    };

    const response = await npxTest.runMCPCommand(prologTest.request);
    prologTest.validate(response);

    expect(response.result?.structuredContent?.success).toBe(1);
  }, 90000); // 90 second timeout

  test("should handle error cases gracefully in installed environment", async () => {
    await npxTest.setup();  
    await npxTest.installPackage();

    // Test error handling for non-existent files
    const errorTest = {
      name: 'error handling validation',
      request: {
        jsonrpc: '2.0',
        id: 3,
        method: 'tools/call',
        params: { name: 'files', arguments: { operation: 'import', filename: '/non/existent/file.pl' } }
      },
      validate: (result: any) => {
        const text = result.result?.content?.[0]?.text || '';
        // Check for any error indicating file access issue
        if (!text.includes('Error:') || !result.result?.isError) {
          throw new Error(`Expected error for non-existent file, got: ${text}`);
        }
      }
    };

    const response = await npxTest.runMCPCommand(errorTest.request);
    errorTest.validate(response);
    
    expect(response.result?.isError).toBe(true);
    expect(response.result?.content?.[0]?.text).toContain('Security Error');
  }, 90000); // 90 second timeout
});