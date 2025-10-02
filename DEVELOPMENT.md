# Local Development Checks

This document describes how to run the same checks locally that run in CI/CD.

## Prerequisites

Install the required tools:

```bash
# Install HLint
cabal install hlint

# Install Ormolu (code formatter)
cabal install ormolu
```

## Running Checks Locally

### Format Check

Check if your code is properly formatted:

```bash
ormolu --mode check $(find src app test -name '*.hs')
```

Auto-format your code:

```bash
ormolu --mode inplace $(find src app test -name '*.hs')
```

### Lint Check

Run HLint to check for code quality issues:

```bash
hlint src/ app/ test/
```

### Build and Test

Build the project:

```bash
cabal build all
```

Run tests:

```bash
cabal test all --test-show-details=direct
```

Build documentation:

```bash
cabal haddock all
```

## Run All Checks

You can run all checks with this one-liner:

```bash
ormolu --mode check $(find src app test -name '*.hs') && \
hlint src/ app/ test/ && \
cabal build all && \
cabal test all --test-show-details=direct && \
cabal haddock all
```

## Pre-commit Hook (Optional)

You can set up a git pre-commit hook to automatically format code before committing:

```bash
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/bash
# Format Haskell files before commit
ormolu --mode inplace $(git diff --cached --name-only --diff-filter=ACM | grep '\.hs$')
git add $(git diff --cached --name-only --diff-filter=ACM | grep '\.hs$')
EOF

chmod +x .git/hooks/pre-commit
```
