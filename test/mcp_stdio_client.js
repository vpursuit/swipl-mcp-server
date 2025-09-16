// test/mcp_stdio_client_debug.js
import { spawn } from 'child_process';
import readline from 'readline';

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
  send('get_capabilities');

  setTimeout(() => {
    console.log('Sending LICENSE tool request');
    send('tools/call', { name: 'LICENSE' });

    setTimeout(() => {
      console.log('Sending knowledge_base_assert test');
      send('tools/call', { 
        name: 'knowledge_base_assert', 
        arguments: { facts: ['parent(john, mary).'] }
      });

      setTimeout(() => {
        console.log('Shutting down');
        proc.kill('SIGTERM');
      }, 1000);
    }, 1000);
  }, 1000);
}, 3000);