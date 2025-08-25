// test/mcp_stdio_client_debug.cjs
const { spawn } = require('child_process');
const readline = require('readline');

const cmd = '/Users/keeper/.nvm/versions/node/v22.17.1/bin/node';
const args = ['build/index.js'];
const env = Object.assign({}, process.env, {
  SWI_MCP_READY_TIMEOUT_MS: '10000',
  SWI_MCP_QUERY_TIMEOUT_MS: '120000',
  MCP_LOG_LEVEL: 'debug',
  DEBUG: 'swipl-mcp-server'
});

console.log('Spawning', cmd, args.join(' '));
const proc = spawn(cmd, args, { cwd: '/Users/keeper/Developer/MacOs/swipl-mcp-server', env, stdio: ['pipe', 'pipe', 'pipe'] });

console.log('child pid=', proc.pid);

proc.on('exit', (code, sig) => console.log('child exit', code, sig));
proc.on('error', (err) => console.error('child error', err));

const rlOut = readline.createInterface({ input: proc.stdout });
const rlErr = readline.createInterface({ input: proc.stderr });

rlOut.on('line', line => {
  console.log('STDOUT:', line);
  try {
    const msg = JSON.parse(line);
    console.log('PARSED JSON:', JSON.stringify(msg));
  } catch (e) {
    // not JSON — still show
  }
});

rlErr.on('line', line => {
  console.error('STDERR:', line);
});

let id = 1;
function send(method, params = {}) {
  const msg = { id: id++, method, params };
  const s = JSON.stringify(msg);
  console.log('SEND ->', s);
  proc.stdin.write(s + '\n');
  return msg.id;
}

// Wait a bit for server to print ready; if no ready within 3s, still send get_capabilities
setTimeout(() => {
  console.log('Sending get_capabilities request');
  send('get_capabilities', {});
}, 1000);

// After 2s, send a simple assert + query to provoke response
setTimeout(() => {
  send('assert_clause', { fact: 'testfact(debug,1).' });
  send('start_query', { query: 'testfact(debug,1)' });
  setTimeout(() => send('next_solution', {}), 300);
}, 2000);

// stop after 8s
setTimeout(() => {
  console.log('Closing client');
  try { proc.kill(); } catch(e) {}
  process.exit(0);
}, 8000);