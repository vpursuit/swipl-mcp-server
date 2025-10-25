import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";

// Simple helper to print section headers
function header(title) {
  console.log(`\n=== ${title} ===`);
}

async function main() {
  const client = new Client({ name: "local-test", version: "0.1.0" });
  const transport = new StdioClientTransport({
    command: "node",
    args: ["build/index.js"],
    cwd: process.cwd(),
    stderr: "pipe",
  });

  // Pipe server stderr to our stderr for visibility
  const stderr = transport.stderr;
  if (stderr) {
    stderr.on("data", (chunk) => {
      process.stderr.write(`[server] ${chunk}`);
    });
  }

  header("Connecting to MCP server");
  await client.connect(transport);

  header("Listing tools");
  const tools = await client.listTools({});
  console.log(tools.tools.map((t) => t.name).join(", "));

  // Helper to call a tool and pretty print results
  async function callTool(name, params) {
    header(`tools/call: ${name}`);
    const res = await client.callTool({ name, arguments: params });
    if (res.isError) {
      console.error("Error:", res.content?.[0]?.text ?? res);
    } else {
      for (const item of res.content ?? []) {
        if (item.type === "text") console.log(item.text);
        else console.log(JSON.stringify(item, null, 2));
      }
    }
    return res;
  }

  // 1) Assert some facts and a simple rule
  await callTool("knowledge_base_assert_many", {
    facts: [
      "parent(john, mary)",
      "parent(mary, alice)",
      "parent(john, bob)",
      "male(john)",
      "female(mary)",
      "female(alice)",
      // Important: rules must be parenthesized when passed to assert/1
      "(grandparent(X,Z) :- parent(X,Y), parent(Y,Z))"
    ],
  });

  // 2) List predicates to verify dynamic DB changed
  await callTool("symbols_list", {});

  // 3) Query with call_nth/2 mode and iterate with next
  await callTool("query_start", { query: "grandparent(X, Z)" });
  let more = true;
  while (more) {
    const next = await callTool("query_next", {});
    const text = next.content?.[0]?.text ?? "";
    if (text.includes("No more solutions")) more = false;
    // stop after a few iterations to be safe
    if (text.includes("Error") || text.includes("no_more_solutions")) more = false;
  }
  await callTool("query_close", {});

  // 4) Engine mode: query_startEngine/query_next/query_close (unified)
  await callTool("query_startEngine", { query: "parent(john, C)" });
  for (let i = 0; i < 5; i++) {
    const r = await callTool("query_next", {});
    const text = r.content?.[0]?.text ?? "";
    if (text.includes("No more solutions") || text.includes("Error")) break;
  }
  await callTool("query_close", {});

  // 5) Retract one fact and verify effect
  await callTool("knowledge_base_retract", { fact: "parent(john, bob)" });
  await callTool("query_start", { query: "parent(john, C)" });
  // Expect only mary now
  for (let i = 0; i < 3; i++) {
    const r = await callTool("query_next", {});
    const text = r.content?.[0]?.text ?? "";
    if (text.includes("No more solutions")) break;
  }
  await callTool("query_close", {});

  header("Done");
  await client.close();
}

main().catch((err) => {
  console.error("Demo failed:", err);
  process.exit(1);
});
