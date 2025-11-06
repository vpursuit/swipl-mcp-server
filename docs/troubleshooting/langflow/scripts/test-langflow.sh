#!/usr/bin/env bash
# Minimal test to see if Langflow can execute this script

# Write to log IMMEDIATELY before anything else
echo "Script executed at $(date)" >> /tmp/langflow-test.log 2>&1
echo "PWD: $PWD" >> /tmp/langflow-test.log 2>&1
echo "Args: $@" >> /tmp/langflow-test.log 2>&1
echo "PATH: $PATH" >> /tmp/langflow-test.log 2>&1

# Also try stderr
echo "TEST: This script was executed!" >&2

# Try to run the actual server
exec /Users/keeper/.nvm/versions/node/v22.18.0/bin/node /Users/keeper/Developer/MacOs/model-context-lab/products/swipl-mcp-server/dist/index.js "$@" 2>&1 | tee -a /tmp/langflow-test.log
