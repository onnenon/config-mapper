# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

config-mapper is a Haskell utility that analyzes JSON configuration files and creates an inverted index showing which config files contain which key-value pairs. This is useful for tracking configuration usage across multiple files.

**Purpose**: Given a directory of JSON config files, produce a mapping that shows for each key-value pair which files contain it.

**Example transformation**:

- Input: `{"name": "Crescent", "title": "America"}` in C1.json
- Output: `{"name": {"Crescent": ["C1.json"]}, "title": {"America": ["C1.json"]}}`

## Build & Run Commands

This is a Cabal-based Haskell project using GHC2021.

```bash
# Build the project (library, executable, and tests)
cabal build all

# Run the executable
cabal run config-mapper

# Run tests
cabal test

# Run tests with verbose output
cabal test --test-show-details=direct

# Clean build artifacts
cabal clean
```

## Architecture

The project is organized into three main components:

1. **Library (`src/ConfigMapper.hs`)**: Core logic for reading and inverting config maps
2. **Executable (`app/Main.hs`)**: Simple CLI that uses the library to process configs
3. **Test Suite (`test/ConfigMapperSpec.hs`)**: HSpec tests for all library functions

### Pipeline

1. **buildConfigMap**: Reads all JSON files from the `configs/` directory, parsing them as `Map Text Text`
2. **invertConfigMapLens**: Transforms the config map into an inverted index using lens operations
3. **Output**: Writes the inverted map to `output.json`

### Core Types

```haskell
type ConfigMap = Map FilePath (Map Text Text)
type InvertedConfigMap = Map Text (Map Text [FilePath])
```

- `ConfigMap`: Maps each file path to its key-value pairs
- `InvertedConfigMap`: Maps each key to its values, with each value mapped to the list of files containing that key-value pair

### Implementation Notes

The library exports two implementations of the inversion logic:

- `invertConfigMap`: Original fold-based implementation
- `invertConfigMapLens`: Lens-based implementation using Control.Lens (currently used by the executable)

The lens implementation uses `at`, `_Just`, and lens operators to safely navigate and update nested map structures.

### Testing

The test suite uses HSpec with automatic test discovery. Tests cover:

- Both inversion implementations (fold-based and lens-based)
- Edge cases (empty maps, single files, multiple files)
- Real-world examples from the documentation
- Integration tests with actual JSON files

## Dependencies

Key libraries:

- `aeson`: JSON parsing/encoding
- `lens`: Lens-based map manipulation
- `containers`: Map data structures
- `directory`, `filepath`: File system operations

## Input/Output

- **Input directory**: `configs/` (hardcoded in main)
- **Output file**: `output.json` (inverted config map)
- **Expected input format**: JSON files with flat key-value pairs where values are strings
