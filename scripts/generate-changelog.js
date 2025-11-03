#!/usr/bin/env node

/**
 * Simple changelog generator from git commits
 * Filters commits by type (feat, fix, perf, breaking) and generates minimal markdown
 */

import { execSync } from 'child_process';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const CHANGELOG_FILE = path.join(__dirname, '..', 'CHANGELOG.md');

// Commit types to include in changelog
const IMPORTANT_TYPES = {
  feat: 'Features',
  fix: 'Bug Fixes',
  perf: 'Performance Improvements',
};

/**
 * Execute git command and return output
 */
function git(command) {
  try {
    return execSync(`git ${command}`, { encoding: 'utf-8' }).trim();
  } catch (error) {
    console.error(`Git command failed: ${command}`);
    return '';
  }
}

/**
 * Get all tags sorted by version
 */
function getTags() {
  const tags = git('tag -l "v*" --sort=-version:refname');
  return tags ? tags.split('\n').filter(Boolean) : [];
}

/**
 * Get commits between two refs
 */
function getCommitsBetween(from, to) {
  const range = from ? `${from}..${to}` : to;
  const separator = '###COMMIT_SEP###';
  const log = git(`log ${range} --pretty=format:"${separator}%H|%s|%b"`);

  if (!log) return [];

  return log
    .split(separator)
    .filter(Boolean)
    .map(commit => {
      const lines = commit.trim().split('\n');
      const firstLine = lines[0];
      const [hash, subject, ...bodyParts] = firstLine.split('|');
      const body = [...bodyParts, ...lines.slice(1)].join('\n');

      return {
        hash: hash ? hash.substring(0, 7) : '',
        subject: subject || '',
        body: body || ''
      };
    })
    .filter(c => c.hash && c.subject);
}

/**
 * Parse commit subject to extract type and description
 */
function parseCommit(commit) {
  const match = commit.subject.match(/^(\w+)(?:\(([^)]+)\))?: (.+)$/);

  if (!match) return null;

  const [, type, scope, description] = match;
  const isBreaking = commit.body.includes('BREAKING CHANGE') || commit.subject.includes('!');

  return {
    type,
    scope,
    description,
    isBreaking,
    hash: commit.hash,
  };
}

/**
 * Filter commits to only important ones
 */
function filterImportantCommits(commits) {
  const parsed = commits
    .map(parseCommit)
    .filter(Boolean);

  const important = parsed.filter(
    c => IMPORTANT_TYPES[c.type] || c.isBreaking
  );

  return important;
}

/**
 * Group commits by type
 */
function groupCommits(commits) {
  const groups = {};

  // Add breaking changes first if any
  const breaking = commits.filter(c => c.isBreaking);
  if (breaking.length > 0) {
    groups['Breaking Changes'] = breaking;
  }

  // Group by type
  Object.entries(IMPORTANT_TYPES).forEach(([type, label]) => {
    const typeCommits = commits.filter(c => c.type === type && !c.isBreaking);
    if (typeCommits.length > 0) {
      groups[label] = typeCommits;
    }
  });

  return groups;
}

/**
 * Format commits as markdown
 */
function formatChangelog(version, date, groups) {
  let markdown = `## [${version}] - ${date}\n\n`;

  Object.entries(groups).forEach(([label, commits]) => {
    markdown += `### ${label}\n\n`;
    commits.forEach(commit => {
      const scope = commit.scope ? `**${commit.scope}**: ` : '';
      markdown += `- ${scope}${commit.description} (${commit.hash})\n`;
    });
    markdown += '\n';
  });

  return markdown;
}

/**
 * Get date for a tag
 */
function getTagDate(tag) {
  const date = git(`log -1 --format=%ai ${tag}`);
  return date ? date.split(' ')[0] : new Date().toISOString().split('T')[0];
}

/**
 * Generate changelog for a specific version
 */
function generateVersionChangelog(currentTag, previousTag) {
  const commits = getCommitsBetween(previousTag, currentTag);
  const important = filterImportantCommits(commits);

  if (important.length === 0) {
    return null;
  }

  const groups = groupCommits(important);
  const version = currentTag.replace(/^v/, '');
  const date = getTagDate(currentTag);

  return formatChangelog(version, date, groups);
}

/**
 * Generate full changelog from all tags
 */
function generateFullChangelog() {
  const tags = getTags();

  if (tags.length === 0) {
    console.error('No tags found in repository');
    return null;
  }

  let changelog = `# Changelog\n\n`;
  changelog += `All notable changes to this project will be documented in this file.\n\n`;
  changelog += `The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).\n\n`;

  for (let i = 0; i < tags.length; i++) {
    const currentTag = tags[i];
    const previousTag = tags[i + 1] || null;

    const versionLog = generateVersionChangelog(currentTag, previousTag);
    if (versionLog) {
      changelog += versionLog;
    }
  }

  return changelog;
}

/**
 * Update CHANGELOG.md file
 */
function updateChangelogFile(content) {
  fs.writeFileSync(CHANGELOG_FILE, content, 'utf-8');
  console.log(`✓ Updated ${CHANGELOG_FILE}`);
}

/**
 * Main function
 */
function main() {
  const args = process.argv.slice(2);
  const command = args[0];

  if (command === '--help' || command === '-h') {
    console.log(`
Usage: generate-changelog.js [options]

Options:
  --version <tag>    Generate changelog for specific version
  --output          Output to stdout instead of file
  --help, -h        Show this help message

Examples:
  generate-changelog.js                    # Generate full changelog and update file
  generate-changelog.js --version v3.0.0   # Generate changelog for v3.0.0
  generate-changelog.js --output           # Output to stdout
`);
    return;
  }

  const versionIndex = args.indexOf('--version');
  const outputToStdout = args.includes('--output');

  let changelog;

  if (versionIndex !== -1) {
    const tag = args[versionIndex + 1];
    if (!tag) {
      console.error('Error: --version requires a tag argument');
      process.exit(1);
    }

    const tags = getTags();
    const currentIndex = tags.indexOf(tag);

    if (currentIndex === -1) {
      console.error(`Error: Tag ${tag} not found`);
      process.exit(1);
    }

    const previousTag = tags[currentIndex + 1] || null;
    changelog = generateVersionChangelog(tag, previousTag);

    if (!changelog) {
      console.log(`No notable changes for ${tag}`);
      return;
    }
  } else {
    changelog = generateFullChangelog();
  }

  if (!changelog) {
    console.error('Failed to generate changelog');
    process.exit(1);
  }

  if (outputToStdout) {
    console.log(changelog);
  } else {
    updateChangelogFile(changelog);
  }
}

// Run main if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export {
  generateFullChangelog,
  generateVersionChangelog,
  getTags,
};
