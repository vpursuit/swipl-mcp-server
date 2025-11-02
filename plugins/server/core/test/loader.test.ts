import { describe, it, expect, vi, beforeEach } from "vitest";
import { loadPlugin, loadPlugins } from "../src/loader.js";
import type { Plugin } from "../src/types.js";
import { z } from "zod";

describe("Plugin Loader", () => {
  let mockServer: any;

  beforeEach(() => {
    mockServer = {
      registerTool: vi.fn(),
      registerResource: vi.fn(),
      registerPrompt: vi.fn(),
    };
  });

  describe("loadPlugin", () => {
    it("should load a basic plugin with tools", async () => {
      const plugin: Plugin = {
        name: "test-plugin",
        version: "1.0.0",
        description: "Test plugin",
        tools: {
          testTool: {
            description: "A test tool",
            inputSchema: z.object({ name: z.string() }),
            handler: async (args) => ({ success: true, data: args }),
          },
        },
      };

      await loadPlugin(mockServer, plugin);

      expect(mockServer.registerTool).toHaveBeenCalledTimes(1);
      expect(mockServer.registerTool).toHaveBeenCalledWith(
        "testTool",
        expect.objectContaining({
          description: "A test tool",
        }),
        expect.any(Function)
      );
    });

    it("should call onInit hook if provided", async () => {
      const onInit = vi.fn();
      const plugin: Plugin = {
        name: "test-plugin",
        version: "1.0.0",
        description: "Test plugin",
        onInit,
      };

      await loadPlugin(mockServer, plugin);

      expect(onInit).toHaveBeenCalledWith(mockServer);
      expect(onInit).toHaveBeenCalledTimes(1);
    });

    it("should register resources", async () => {
      const plugin: Plugin = {
        name: "test-plugin",
        version: "1.0.0",
        description: "Test plugin",
        resources: {
          testResource: {
            uri: "test://resource",
            name: "Test Resource",
            description: "A test resource",
            handler: async () => ({
              uri: "test://resource",
              name: "Test Resource",
              text: "Hello",
            }),
          },
        },
      };

      await loadPlugin(mockServer, plugin);

      expect(mockServer.registerResource).toHaveBeenCalledTimes(1);
      expect(mockServer.registerResource).toHaveBeenCalledWith(
        "Test Resource",
        "test://resource",
        expect.objectContaining({
          title: "Test Resource",
          description: "A test resource",
        }),
        expect.any(Function)
      );
    });

    it("should register prompts", async () => {
      const plugin: Plugin = {
        name: "test-plugin",
        version: "1.0.0",
        description: "Test plugin",
        prompts: {
          testPrompt: {
            name: "test-prompt",
            description: "A test prompt",
            arguments: [
              { name: "task", description: "Task description", required: true },
            ],
            handler: async (args) => ({
              messages: [
                {
                  role: "user",
                  content: { type: "text", text: `Task: ${args.task}` },
                },
              ],
            }),
          },
        },
      };

      await loadPlugin(mockServer, plugin);

      expect(mockServer.registerPrompt).toHaveBeenCalledTimes(1);

      // Verify the call was made with correct parameters
      const [[promptName, config, handler]] = mockServer.registerPrompt.mock.calls;
      expect(promptName).toBe("test-prompt");
      expect(config.title).toBe("test-prompt");
      expect(config.description).toBe("A test prompt");

      // Verify argsSchema is a proper Zod schema object
      expect(config.argsSchema).toBeDefined();
      expect(config.argsSchema.task).toBeDefined();

      // Verify the task schema has Zod schema properties
      expect(config.argsSchema.task._def).toBeDefined(); // Zod schemas have _def
      expect(config.argsSchema.task._def.description).toBe("Task description");

      expect(handler).toBeTypeOf("function");
    });

    it("should throw error if plugin fails to load by default", async () => {
      const plugin: Plugin = {
        name: "bad-plugin",
        version: "1.0.0",
        description: "Bad plugin",
        onInit: async () => {
          throw new Error("Init failed");
        },
      };

      await expect(loadPlugin(mockServer, plugin)).rejects.toThrow(
        "Failed to load plugin: bad-plugin"
      );
    });

    it("should continue on error if configured", async () => {
      const plugin: Plugin = {
        name: "bad-plugin",
        version: "1.0.0",
        description: "Bad plugin",
        onInit: async () => {
          throw new Error("Init failed");
        },
      };

      const logger = {
        info: vi.fn(),
        error: vi.fn(),
        warn: vi.fn(),
      };

      await expect(
        loadPlugin(mockServer, plugin, { continueOnError: true, logger })
      ).resolves.not.toThrow();

      expect(logger.error).toHaveBeenCalled();
    });
  });

  describe("loadPlugins", () => {
    it("should load multiple plugins", async () => {
      const plugins: Plugin[] = [
        {
          name: "plugin1",
          version: "1.0.0",
          description: "First plugin",
          tools: {
            tool1: {
              description: "Tool 1",
              inputSchema: z.object({}),
              handler: async () => ({ success: true }),
            },
          },
        },
        {
          name: "plugin2",
          version: "1.0.0",
          description: "Second plugin",
          tools: {
            tool2: {
              description: "Tool 2",
              inputSchema: z.object({}),
              handler: async () => ({ success: true }),
            },
          },
        },
      ];

      await loadPlugins(mockServer, plugins);

      expect(mockServer.registerTool).toHaveBeenCalledTimes(2);
    });

    it("should load plugins sequentially", async () => {
      const initOrder: string[] = [];

      const plugins: Plugin[] = [
        {
          name: "plugin1",
          version: "1.0.0",
          description: "First plugin",
          onInit: async () => {
            initOrder.push("plugin1");
          },
        },
        {
          name: "plugin2",
          version: "1.0.0",
          description: "Second plugin",
          onInit: async () => {
            initOrder.push("plugin2");
          },
        },
      ];

      await loadPlugins(mockServer, plugins);

      expect(initOrder).toEqual(["plugin1", "plugin2"]);
    });

    it("should use custom logger if provided", async () => {
      const logger = {
        info: vi.fn(),
        error: vi.fn(),
        warn: vi.fn(),
      };

      const plugin: Plugin = {
        name: "test-plugin",
        version: "1.0.0",
        description: "Test plugin",
      };

      await loadPlugins(mockServer, [plugin], { logger });

      expect(logger.info).toHaveBeenCalled();
    });
  });
});
