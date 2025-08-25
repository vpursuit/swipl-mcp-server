import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";

const client = new Client({ name: "quickcheck", version: "0.0.1" });
const transport = new StdioClientTransport({
  command: "node",
  args: ["build/index.js"],
  cwd: process.cwd(),
  stderr: "pipe",
});

const stderr = transport.stderr;
if (stderr) {
  stderr.on("data", (chunk) => process.stderr.write(`[server] ${chunk}`));
}

async function run() {
  await client.connect(transport);
  const result = await client.listTools({});
  console.log("TOOLS:", result.tools.map((t) => t.name).join(", "));
  await client.close();
}

run().catch((e) => { console.error("quickcheck failed:", e); process.exit(1); });

