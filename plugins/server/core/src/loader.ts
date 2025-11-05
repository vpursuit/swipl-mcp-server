import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type {
  Plugin,
  PluginLoaderConfig,
  ToolDefinition,
  ResourceDefinition,
  PromptDefinition,
} from "./types.js";
import { createMcpLogger, type McpLogger } from "./logger.js";
import { z } from "zod";

/**
 * Default console logger (stderr fallback)
 */
const defaultLogger = {
  info: (message: string) => console.error(`[mcp-core] ${message}`),
  error: (message: string, error?: Error) =>
    console.error(`[mcp-core] ERROR: ${message}`, error),
  warn: (message: string) => console.error(`[mcp-core] WARNING: ${message}`),
};

/**
 * Load a single plugin into the MCP server
 * @param server - MCP server instance
 * @param plugin - Plugin to load
 * @param config - Optional configuration
 */
export async function loadPlugin(
  server: McpServer,
  plugin: Plugin,
  config: PluginLoaderConfig = {}
): Promise<void> {
  const logger = config.logger || defaultLogger;

  try {
    logger.info(
      `Loading plugin: ${plugin.name} v${plugin.version} - ${plugin.description}`
    );

    // Call initialization hook first
    if (plugin.onInit) {
      logger.info(`Initializing plugin: ${plugin.name}`);
      await plugin.onInit(server);
    }

    // Register tools
    if (plugin.tools) {
      const toolNames = Object.keys(plugin.tools);
      logger.info(
        `Registering ${toolNames.length} tool(s) from ${plugin.name}: ${toolNames.join(", ")}`
      );

      for (const [toolName, toolDef] of Object.entries(plugin.tools)) {
        registerTool(server, toolName, toolDef, logger);
      }
    }

    // Register resources
    if (plugin.resources) {
      const resourceNames = Object.keys(plugin.resources);
      logger.info(
        `Registering ${resourceNames.length} resource(s) from ${plugin.name}: ${resourceNames.join(", ")}`
      );

      for (const resourceDef of Object.values(plugin.resources)) {
        registerResource(server, resourceDef, logger);
      }
    }

    // Register prompts
    if (plugin.prompts) {
      const promptNames = Object.keys(plugin.prompts);
      logger.info(
        `Registering ${promptNames.length} prompt(s) from ${plugin.name}: ${promptNames.join(", ")}`
      );

      for (const promptDef of Object.values(plugin.prompts)) {
        registerPrompt(server, promptDef, logger);
      }
    }

    logger.info(`✓ Successfully loaded plugin: ${plugin.name}`);
  } catch (error) {
    const errorMessage = `Failed to load plugin: ${plugin.name}`;
    logger.error(errorMessage, error as Error);

    if (!config.continueOnError) {
      throw new Error(
        `${errorMessage}: ${error instanceof Error ? error.message : String(error)}`
      );
    }
  }
}

/**
 * Load multiple plugins into the MCP server
 * @param server - MCP server instance
 * @param plugins - Array of plugins to load
 * @param config - Optional configuration
 */
export async function loadPlugins(
  server: McpServer,
  plugins: Plugin[],
  config: PluginLoaderConfig = {}
): Promise<void> {
  const logger = config.logger || defaultLogger;

  logger.info(`Loading ${plugins.length} plugin(s)`);

  for (const plugin of plugins) {
    await loadPlugin(server, plugin, config);
  }

  logger.info(`✓ All plugins loaded successfully`);
}

/**
 * Register a tool with the MCP server
 *
 * Note: Minimal 'as any' used because SDK's ToolCallback has conditional types
 * (different signatures for tools with/without args), but our simplified interface
 * uses consistent (args, extra) signature for all tools. Runtime behavior is correct.
 */
function registerTool(
  server: McpServer,
  toolName: string,
  toolDef: ToolDefinition,
  logger: PluginLoaderConfig["logger"] = defaultLogger
): void {
  try {
    server.registerTool(
      toolName,
      {
        title: toolDef.title,
        description: toolDef.description,
        inputSchema: toolDef.inputSchema,
        outputSchema: toolDef.outputSchema,
      },
      toolDef.handler as any
    );
  } catch (error) {
    logger.error(
      `Failed to register tool: ${toolName}`,
      error as Error
    );
    throw error;
  }
}

/**
 * Register a resource with the MCP server
 *
 * Note: Minimal 'as any' used for handler because SDK signature expects specific
 * RequestHandlerExtra type complexity. Runtime behavior is correct.
 */
function registerResource(
  server: McpServer,
  resourceDef: ResourceDefinition,
  logger: PluginLoaderConfig["logger"] = defaultLogger
): void {
  try {
    server.registerResource(
      resourceDef.name,
      resourceDef.uri,
      {
        title: resourceDef.name,
        description: resourceDef.description,
        mimeType: resourceDef.mimeType,
      },
      resourceDef.handler as any
    );
  } catch (error) {
    logger.error(
      `Failed to register resource: ${resourceDef.name}`,
      error as Error
    );
    throw error;
  }
}

/**
 * Register a prompt with the MCP server
 *
 * Uses SDK's PromptCallback type. Note: We use 'as any' for the handler because
 * PromptCallback has complex conditional types based on whether arguments exist,
 * making it impractical to type perfectly without significant complexity.
 * The runtime behavior is correct.
 */
function registerPrompt(
  server: McpServer,
  promptDef: PromptDefinition,
  logger: PluginLoaderConfig["logger"] = defaultLogger
): void {
  try {
    // Build argsSchema from prompt arguments
    // Workaround for MCP SDK issue #400: When all arguments are optional, the SDK's z.object()
    // wrapper still requires an object (not undefined), which violates the MCP spec that allows
    // arguments to be undefined. Solution: Don't provide schema when all args are optional.
    let argsSchema: Record<string, z.ZodString | z.ZodOptional<z.ZodString>> | undefined;

    const hasArguments = promptDef.arguments && promptDef.arguments.length > 0;
    const allArgumentsOptional = hasArguments && promptDef.arguments!.every(arg => !arg.required);

    if (hasArguments && !allArgumentsOptional) {
      // Only provide schema if there are required arguments
      argsSchema = promptDef.arguments!.reduce((acc, arg) => {
        const zodSchema = arg.required ? z.string() : z.string().optional();
        const schemaWithDesc = arg.description
          ? zodSchema.describe(arg.description)
          : zodSchema;
        acc[arg.name] = schemaWithDesc;
        return acc;
      }, {} as Record<string, z.ZodString | z.ZodOptional<z.ZodString>>);
    }
    // If all arguments are optional, set argsSchema to undefined to skip SDK validation
    // The handler will still normalize undefined to {} for consistency

    // Wrap handler to normalize undefined to empty object
    const wrappedHandler = async (args: any, extra: any) => {
      // MCP spec allows arguments to be undefined when all fields are optional
      // Normalize to empty object for consistent handling
      return promptDef.handler(args || {}, extra);
    };

    server.registerPrompt(
      promptDef.name,
      {
        title: promptDef.title || promptDef.name,
        description: promptDef.description,
        argsSchema,
      },
      wrappedHandler as any
    );
  } catch (error) {
    logger.error(
      `Failed to register prompt: ${promptDef.name}`,
      error as Error
    );
    throw error;
  }
}
