# Changelog for config-mapper

## [0.1.0.0] - 2025-10-02

### Added

- Comprehensive HSpec test suite with 11 tests covering:
  - Both `invertConfigMap` and `invertConfigMapLens` implementations
  - Edge cases (empty maps, single files, multiple files)
  - Real-world examples from documentation
  - Integration tests with actual JSON files
- Test discovery using `hspec-discover` for easy extensibility
- REFACTORING.md documenting the project restructuring

### Changed

- **Breaking**: Moved core logic from `app/Main.hs` to new library module `src/ConfigMapper.hs`
- Updated cabal file to include library and test-suite sections
- Simplified `app/Main.hs` to use the library (reduced from 65 to 8 lines)
- Updated AGENTS.md with new project structure and test information
- Executable now only depends on necessary packages (library handles most dependencies)

### Library API

The new `ConfigMapper` library exports:

- Types: `ConfigMap`, `InvertedConfigMap`
- Functions: `buildConfigMap`, `invertConfigMap`, `invertConfigMapLens`, `decodeFile`

All functions include Haddock documentation.

## [0.1.0.0-initial] - Previous

Initial implementation as single-file executable.
