#!/usr/bin/env node

/**
 * Production build script for @vpursuit/swipl-mcp-server
 * Creates optimized distribution package with minimal size
 */

import fs from "fs";
import path from "path";
import { execSync } from "child_process";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const ROOT_DIR = path.join(__dirname, "..");
const DIST_DIR = path.join(ROOT_DIR, "dist");
const SRC_DIR = path.join(ROOT_DIR, "src");
const BUILD_DIR = path.join(ROOT_DIR, "build");

console.log("🏗️  Building @vpursuit/swipl-mcp-server package...\n");

// Clean and create dist directory
if (fs.existsSync(DIST_DIR)) {
  fs.rmSync(DIST_DIR, { recursive: true, force: true });
}
fs.mkdirSync(DIST_DIR, { recursive: true });

// Step 1: Compile TypeScript
console.log("📦 Compiling TypeScript...");
try {
  execSync("npm run build", { cwd: ROOT_DIR, stdio: "inherit" });
} catch (error) {
  console.error("❌ TypeScript compilation failed");
  process.exit(1);
}

// Step 2: Create lib directory and copy compiled JS files
const libDir = path.join(DIST_DIR, "lib");
fs.mkdirSync(libDir, { recursive: true });

const jsFiles = ["index.js", "PrologInterface.js", "tools.js", "logger.js", "schemas.js", "meta.js", "security.js"];
jsFiles.forEach((file) => {
  const srcPath = path.join(BUILD_DIR, file);
  const destPath = path.join(libDir, file);
  if (fs.existsSync(srcPath)) {
    let content = fs.readFileSync(srcPath, "utf8");

    // Add shebang to index.js for executable binary
    if (file === "index.js") {
      content = "#!/usr/bin/env node\n" + content;
    }

    fs.writeFileSync(destPath, content);

    // Make index.js executable
    if (file === "index.js") {
      fs.chmodSync(destPath, "755");
    }

    console.log(`✅ Copied ${file}`);
  } else {
    console.warn(`⚠️  Missing ${file} in build directory`);
  }
});

// Step 3: Create prolog directory and copy server file
const prologDir = path.join(DIST_DIR, "prolog");
fs.mkdirSync(prologDir, { recursive: true });

const prologServerSrc = path.join(SRC_DIR, "prolog_server.pl");
const prologServerDest = path.join(prologDir, "prolog_server.pl");
fs.copyFileSync(prologServerSrc, prologServerDest);
console.log("✅ Copied prolog_server.pl → prolog/prolog_server.pl");

// Step 4: Create production package.json
const originalPkg = JSON.parse(fs.readFileSync(path.join(ROOT_DIR, "package.json"), "utf8"));
const productionPkg = {
  name: "@vpursuit/swipl-mcp-server",
  version: originalPkg.version || "1.0.0",
  description: "SWI-Prolog MCP Server with dual query modes and security layer",
  type: "module",
  keywords: ["mcp", "prolog", "swi-prolog", "swipl", "model-context-protocol"],
  main: "lib/index.js",
  bin: "lib/index.js",
  exports: {
    ".": "./lib/index.js",
    "./package.json": "./package.json",
  },
  files: ["lib", "prolog", "README.md", "LICENSE"],
  engines: {
    node: ">=18.0.0",
  },
  dependencies: {
    "@modelcontextprotocol/sdk": originalPkg.dependencies["@modelcontextprotocol/sdk"],
    zod: originalPkg.dependencies["zod"],
  },
  publishConfig: {
    access: "public",
  },
  repository: originalPkg.repository,
  author: originalPkg.author,
  license: originalPkg.license || "ISC",
};

fs.writeFileSync(path.join(DIST_DIR, "package.json"), JSON.stringify(productionPkg, null, 2));
console.log("✅ Created production package.json");

// Step 5: Copy essential files
const essentialFiles = [
  { src: "README-npm.md", dest: "README.md" }, // Use NPM-specific README
  { src: "LICENSE", dest: "LICENSE" },
];

essentialFiles.forEach(({ src, dest }) => {
  const srcPath = path.join(ROOT_DIR, src);
  const destPath = path.join(DIST_DIR, dest);
  if (fs.existsSync(srcPath)) {
    fs.copyFileSync(srcPath, destPath);
    console.log(`✅ Copied ${src} → ${dest}`);
  } else {
    console.warn(`⚠️  Missing ${src}`);
  }
});

// Step 6: Calculate package size
const getDirectorySize = (dir) => {
  let size = 0;
  const items = fs.readdirSync(dir);
  items.forEach((item) => {
    const fullPath = path.join(dir, item);
    const stat = fs.statSync(fullPath);
    if (stat.isDirectory()) {
      size += getDirectorySize(fullPath);
    } else {
      size += stat.size;
    }
  });
  return size;
};

const distSize = getDirectorySize(DIST_DIR);
const distSizeKB = Math.round(distSize / 1024);
const distSizeMB = (distSize / (1024 * 1024)).toFixed(2);

console.log("\n📊 Package Build Summary:");
console.log(`📁 Distribution directory: ${DIST_DIR}`);
console.log(`📏 Package size: ${distSizeKB} KB (${distSizeMB} MB)`);
console.log(
  `🎯 Target achieved: ${distSizeMB < 1 ? "✅" : "❌"} ${distSizeMB < 1 ? "Under 1MB" : "Over 1MB"}`,
);

console.log("\n🚀 Build completed successfully!");
console.log("\n💡 Next steps:");
console.log("   npm pack dist/");
console.log("   cd dist && npm link");
console.log("   npx @vpursuit/swipl-mcp-server");
