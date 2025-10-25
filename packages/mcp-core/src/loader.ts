import type { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import type {
  Plugin,
  PluginLoaderConfig,
  ToolDefinition,
  ResourceDefinition,
  PromptDefinition,
} from "./types.js";
import { createMcpLogger, type McpLogger } from "./logger.js";

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
 */
function registerTool(
  server: McpServer,
  toolName: string,
  toolDef: ToolDefinition,
  logger: PluginLoaderConfig["logger"] = defaultLogger
): void {
  try {
    // MCP SDK expects Zod schemas natively (ZodRawShape), not JSON schemas
    // Extract the .shape from the Zod object schema to pass to registerTool
    server.registerTool(
      toolName,
      {
        description: toolDef.description,
        inputSchema: (toolDef.inputSchema as any).shape,
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
 */
function registerPrompt(
  server: McpServer,
  promptDef: PromptDefinition,
  logger: PluginLoaderConfig["logger"] = defaultLogger
): void {
  try {
    server.registerPrompt(
      promptDef.name,
      {
        title: promptDef.name,
        description: promptDef.description,
        argsSchema: promptDef.arguments?.reduce((acc, arg) => {
          acc[arg.name] = {
            type: "string" as const,
            description: arg.description,
          };
          return acc;
        }, {} as Record<string, any>),
      },
      promptDef.handler as any
    );
  } catch (error) {
    logger.error(
      `Failed to register prompt: ${promptDef.name}`,
      error as Error
    );
    throw error;
  }
}
