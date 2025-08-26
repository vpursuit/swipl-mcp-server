# Deployment & Releases

## Build from Source

```bash
git clone https://github.com/vpursuit/swipl-mcp-server.git
cd swipl-mcp-server
npm install
npm run build
node build/index.js
```

## Optimized Package (dist/)

```bash
npm run build:package
cd dist
npm pack
```

The dist package contains the compiled library under `lib/` and the Prolog server at `prolog/server.pl`.

## Publish to npm

```bash
npm run build:package
cd dist
npm publish
```

## Version & Release Scripts

- `npm run version:patch|minor|major|prerelease`
- `npm run publish:dry-run` (from project root)
- `npm run publish:npm` (publishes `dist/`)
- `npm run github:release` (create GitHub release for current tag)

Typical flow:
1) Bump version → 2) Push commits/tags → 3) Build package → 4) Publish → 5) Create GitHub release

