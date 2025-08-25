#!/usr/bin/env node

/**
 * GitHub Release Creator Script
 * Creates GitHub releases from git tags with automated release notes
 */

import { execSync } from "child_process";
import fs from "fs";
import { writeFileSync, unlinkSync, existsSync } from "fs";

const packageJson = JSON.parse(fs.readFileSync("package.json", "utf8"));
const currentVersion = packageJson.version;
const tagName = `v${currentVersion}`;

console.log(`🚀 Creating GitHub release for ${tagName}...`);

// Check if gh CLI is installed
try {
  execSync("gh --version", { stdio: "pipe" });
} catch (error) {
  console.error("❌ GitHub CLI (gh) is not installed.");
  console.error("Install it with: brew install gh");
  console.error("Then authenticate with: gh auth login");
  process.exit(1);
}

// Get the previous tag to generate comparison
let previousTag;
try {
  const tags = execSync("git tag --sort=-version:refname", { encoding: "utf8" })
    .trim()
    .split("\n")
    .filter(tag => tag.startsWith("v"));
  
  previousTag = tags.find(tag => tag !== tagName) || "HEAD~10";
} catch (error) {
  previousTag = "HEAD~10";
}

// Generate commit messages since last release
let changelogEntries;
try {
  const commits = execSync(
    `git log ${previousTag}..${tagName} --pretty=format:"%s" --reverse`,
    { encoding: "utf8" }
  ).trim();
  
  if (commits) {
    changelogEntries = commits
      .split("\n")
      .filter(commit => !commit.match(/^(v?\d+\.\d+\.\d+|Merge |Bump )/))
      .map(commit => `- ${commit}`)
      .join("\n");
  } else {
    changelogEntries = "- Minor improvements and bug fixes";
  }
} catch (error) {
  changelogEntries = "- Version update";
}

// Determine release type from version
const isPrerelease = currentVersion.includes("-");
const releaseType = isPrerelease ? "Pre-release" : "Release";

// Create release notes
const releaseNotes = `## What's Changed
${changelogEntries}

## Installation
\`\`\`bash
# Install globally
npm install -g @vpursuit/swipl-mcp-server

# Or run directly  
npx @vpursuit/swipl-mcp-server
\`\`\`

## Requirements
- Node.js ≥ 18.0.0
- SWI-Prolog installed and available in PATH

${previousTag !== "HEAD~10" ? `**Full Changelog**: https://github.com/vpursuit/swipl-mcp-server/compare/${previousTag}...${tagName}` : ''}`;

// Create the GitHub release
try {
  console.log(`📝 Release notes preview:`);
  console.log("─".repeat(60));
  console.log(releaseNotes);
  console.log("─".repeat(60));

  // Write notes to temporary file to avoid shell escaping issues
  const tempNotesFile = "temp-release-notes.md";
  writeFileSync(tempNotesFile, releaseNotes);

  const releaseCommand = [
    "gh", "release", "create", tagName,
    "--title", `"${releaseType} ${tagName}"`,
    "--notes-file", tempNotesFile
  ];

  if (isPrerelease) {
    releaseCommand.push("--prerelease");
  }

  execSync(releaseCommand.join(" "), { stdio: "inherit" });
  
  // Clean up temp file
  unlinkSync(tempNotesFile);
  
  console.log(`✅ GitHub release ${tagName} created successfully!`);
  console.log(`🔗 View at: https://github.com/vpursuit/swipl-mcp-server/releases/tag/${tagName}`);

} catch (error) {
  console.error(`❌ Failed to create GitHub release: ${error.message}`);
  // Clean up temp file if it exists
  try {
    if (existsSync("temp-release-notes.md")) {
      unlinkSync("temp-release-notes.md");
    }
  } catch (cleanupError) {
    // Ignore cleanup errors
  }
  process.exit(1);
}