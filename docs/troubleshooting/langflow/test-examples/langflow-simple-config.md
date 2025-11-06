# NEW Simplified Langflow Configuration

## What Changed

The server now **automatically finds SWI-Prolog** without needing PATH configuration or wrapper scripts!

## New Configuration (Much Simpler!)

### In Langflow UI:

**Name:** `swipl-prolog`

**Command:** `node`

**Arguments:** `/Users/keeper/Developer/MacOs/model-context-lab/products/swipl-mcp-server/dist/index.js`

**Environment Variables:** (optional)
- `SWI_MCP_READY_TIMEOUT_MS` = `30000`

That's it! No PATH configuration needed.

---

## Or Using Full Node Path (if node not in PATH):

**Command:** `/Users/keeper/.nvm/versions/node/v22.18.0/bin/node`

**Arguments:** `/Users/keeper/Developer/MacOs/model-context-lab/products/swipl-mcp-server/dist/index.js`

---

## How It Works

The server searches for SWI-Prolog in these locations automatically:
- `/opt/homebrew/bin/swipl` ✅ (Your installation)
- `/usr/local/bin/swipl`
- `/usr/bin/swipl`
- `/opt/local/bin/swipl`
- System PATH

**No manual configuration needed!**

## Expected Result

After saving the configuration:
- Wait 5-10 seconds
- Langflow should show "Connected"
- 17 tools should appear in the dropdown
- Tools include: help, capabilities, query_start, knowledge_base_load, etc.

## Test It

Remove the old wrapper script configuration and use the simple one above. It should just work!
