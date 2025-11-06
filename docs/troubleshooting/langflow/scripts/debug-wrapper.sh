#!/usr/bin/env bash

# Debug wrapper for swipl-mcp-server
# Captures all output (stdout, stderr, crashes) to a log file
# Usage: ./debug-wrapper.sh [args...]
#   - Logs to /tmp/swipl-mcp-server.log
#   - Can be used in Langflow MCP client config

set -euo pipefail

LOG_FILE="${SWI_MCP_LOG_FILE:-/tmp/swipl-mcp-server.log}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SERVER_JS="$SCRIPT_DIR/dist/index.js"

# Rotate log if it exists and is large
if [ -f "$LOG_FILE" ] && [ "$(stat -f%z "$LOG_FILE" 2>/dev/null || stat -c%s "$LOG_FILE" 2>/dev/null || echo 0)" -gt 1048576 ]; then
    mv "$LOG_FILE" "$LOG_FILE.old"
fi

# Start logging
{
    echo "========================================"
    echo "swipl-mcp-server Debug Wrapper"
    echo "Started: $(date '+%Y-%m-%d %H:%M:%S')"
    echo "========================================"
    echo ""

    echo "Environment:"
    echo "  PWD: $PWD"
    echo "  SCRIPT_DIR: $SCRIPT_DIR"
    echo "  SERVER_JS: $SERVER_JS"
    echo "  NODE: $(which node)"
    echo "  NODE_VERSION: $(node --version)"
    echo "  SWI_MCP_TRACE: ${SWI_MCP_TRACE:-<not set>}"
    echo "  MCP_LOG_LEVEL: ${MCP_LOG_LEVEL:-<not set>}"
    echo "  SWI_MCP_READY_TIMEOUT_MS: ${SWI_MCP_READY_TIMEOUT_MS:-<not set>}"
    echo "  SWI_MCP_PROLOG_PATH: ${SWI_MCP_PROLOG_PATH:-<not set>}"
    echo ""

    echo "Checking server file:"
    if [ -f "$SERVER_JS" ]; then
        echo "  ✓ $SERVER_JS exists"
        ls -lh "$SERVER_JS"
    else
        echo "  ✗ $SERVER_JS NOT FOUND!"
        exit 1
    fi
    echo ""

    echo "Checking Prolog installation:"
    if command -v swipl >/dev/null 2>&1; then
        echo "  ✓ SWI-Prolog found: $(which swipl)"
        echo "  Version: $(swipl --version 2>&1 | head -1)"
    else
        echo "  ✗ SWI-Prolog NOT FOUND in PATH!"
    fi
    echo ""

    echo "Checking Prolog server script:"
    PROLOG_SCRIPT="$SCRIPT_DIR/prolog/prolog_server.pl"
    if [ -f "$PROLOG_SCRIPT" ]; then
        echo "  ✓ $PROLOG_SCRIPT exists"
    else
        echo "  ✗ $PROLOG_SCRIPT NOT FOUND!"
        echo "  Searched: $PROLOG_SCRIPT"
    fi
    echo ""

    echo "========================================"
    echo "Starting server..."
    echo "========================================"
    echo ""

    # Run the server and capture all output
    # stderr goes to both log and stderr (for MCP protocol)
    # stdout goes to both log and stdout (for MCP protocol)
    EXIT_CODE=0
    node "$SERVER_JS" "$@" 2>&1 | tee -a /dev/stderr || EXIT_CODE=$?

    echo ""
    echo "========================================"
    echo "Server exited with code: $EXIT_CODE"
    echo "Ended: $(date '+%Y-%m-%d %H:%M:%S')"
    echo "========================================"

    exit $EXIT_CODE

} >> "$LOG_FILE" 2>&1
