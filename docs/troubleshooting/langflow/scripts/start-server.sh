#!/usr/bin/env bash
# Simple starter script for Langflow MCP client
# This ensures node and swipl are found and logs errors

# Set PATH to include common locations for swipl and node
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

# Log startup
echo "[$(date)] Starting swipl-mcp-server" >> /tmp/swipl-startup.log 2>&1
echo "[$(date)] PATH: $PATH" >> /tmp/swipl-startup.log 2>&1

# Find node
NODE_PATH=""
if command -v node >/dev/null 2>&1; then
    NODE_PATH=$(command -v node)
elif [ -f "/Users/keeper/.nvm/versions/node/v22.18.0/bin/node" ]; then
    NODE_PATH="/Users/keeper/.nvm/versions/node/v22.18.0/bin/node"
elif [ -f "/usr/local/bin/node" ]; then
    NODE_PATH="/usr/local/bin/node"
fi

if [ -z "$NODE_PATH" ]; then
    echo "[$(date)] ERROR: Node.js not found" >> /tmp/swipl-startup.log 2>&1
    exit 1
fi

echo "[$(date)] Using Node: $NODE_PATH" >> /tmp/swipl-startup.log 2>&1

# Verify swipl is available
if command -v swipl >/dev/null 2>&1; then
    SWIPL_PATH=$(command -v swipl)
    echo "[$(date)] Found SWI-Prolog: $SWIPL_PATH" >> /tmp/swipl-startup.log 2>&1
else
    echo "[$(date)] ERROR: SWI-Prolog not found in PATH" >> /tmp/swipl-startup.log 2>&1
    echo "[$(date)] Current PATH: $PATH" >> /tmp/swipl-startup.log 2>&1
    exit 1
fi

# Run server with error logging
SERVER_JS="/Users/keeper/Developer/MacOs/model-context-lab/products/swipl-mcp-server/dist/index.js"
echo "[$(date)] Running: $NODE_PATH $SERVER_JS" >> /tmp/swipl-startup.log 2>&1

exec "$NODE_PATH" "$SERVER_JS" 2>> /tmp/swipl-startup.log
