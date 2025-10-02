# config-mapper

A Haskell utility that analyzes JSON configuration files and creates an inverted index showing which config files contain which key-value pairs. This is particularly useful for tracking configuration usage across multiple files in large projects.

## 📋 Overview

Given a directory of JSON configuration files, `config-mapper` produces a mapping that shows for each key-value pair which files contain it. This inverted index makes it easy to:

- Find all configurations that use a specific key-value pair
- Track where certain configuration values are defined
- Audit configuration consistency across multiple files

### Example Transformation

**Input files:**

`configs/C1.json`:

```json
{
  "name": "Crescent",
  "title": "America"
}
```

`configs/C2.json`:

```json
{
  "name": "Crescent",
  "title": "Japanese"
}
```

**Output (`output.json`):**

```json
{
  "name": {
    "Crescent": ["C1.json", "C2.json"]
  },
  "title": {
    "America": ["C1.json"],
    "Japanese": ["C2.json"]
  }
}
```

## 🚀 Quick Start

### Prerequisites

- GHC 9.10.1 or later
- Cabal 3.4 or later

### Installation

Clone the repository and build:

```bash
git clone https://github.com/onnenon/config-mapper.git
cd config-mapper
cabal build
```

### Usage

1. Place your JSON configuration files in the `configs/` directory
2. Run the executable:

```bash
cabal run config-mapper
```

3. The inverted configuration map will be written to `output.json`

## 🏗️ Architecture

The project is organized into three main components:

### Library (`src/ConfigMapper.hs`)

Core functionality for reading and inverting config maps:

- `buildConfigMap`: Reads all JSON files from a directory and parses them
- `invertConfigMap`: Fold-based implementation for inverting the config map
- `invertConfigMapLens`: Lens-based implementation (currently used by default)

### Executable (`app/Main.hs`)

Simple CLI that:

1. Reads configs from the `configs/` directory
2. Inverts the configuration map
3. Writes the result to `output.json`

### Test Suite (`test/ConfigMapperSpec.hs`)

Comprehensive HSpec tests covering:

- Both inversion implementations
- Edge cases (empty maps, single files, multiple files)
- Real-world examples

## 📦 Core Types

```haskell
type ConfigMap = Map FilePath (Map Text Text)
type InvertedConfigMap = Map Text (Map Text [FilePath])
```

- **ConfigMap**: Maps each file path to its key-value pairs
- **InvertedConfigMap**: Maps each key to its values, with each value mapped to the list of files containing that key-value pair

## 🛠️ Development

### Building

```bash
# Build the project
cabal build all

# Clean build artifacts
cabal clean

# Build and install
cabal install
```

### Running

```bash
# Run the executable
cabal run config-mapper
```

### Testing

```bash
# Run tests
cabal test

# Run tests with verbose output
cabal test --test-show-details=direct
```

### Project Structure

```
config-mapper/
├── app/
│   └── Main.hs              # CLI executable
├── configs/                  # Input directory for JSON configs
│   ├── C1.json
│   └── C2.json
├── src/
│   └── ConfigMapper.hs      # Core library
├── test/
│   ├── Spec.hs
│   └── ConfigMapperSpec.hs  # Test suite
├── config-map.cabal         # Package configuration
└── output.json              # Generated output
```

## 📚 Key Dependencies

- **aeson**: JSON parsing and encoding
- **lens**: Lens-based map manipulation
- **containers**: Map data structures
- **directory/filepath**: File system operations

## 🔧 Implementation Notes

The library provides two implementations of the inversion logic:

1. **`invertConfigMap`**: Original fold-based implementation using pure map operations
2. **`invertConfigMapLens`**: Lens-based implementation using Control.Lens for safe nested map navigation

The lens implementation uses `at`, `_Just`, and lens operators to safely navigate and update nested map structures. Both implementations are fully tested and produce identical results.

## 📝 Input Format

Config files should be valid JSON with flat key-value pairs where values are strings:

```json
{
  "key1": "value1",
  "key2": "value2",
  "key3": "value3"
}
```

**Note**: Nested objects and non-string values are not currently supported.

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development Workflow

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`cabal test`)
5. Commit your changes (`git commit -m 'Add some amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## ✍️ Author

**Stephen Onnen** - [onnenon](https://github.com/onnenon)

## 🙏 Acknowledgments

Built with Haskell and the following excellent libraries:

- [aeson](https://hackage.haskell.org/package/aeson) - Fast JSON parsing
- [lens](https://hackage.haskell.org/package/lens) - Composable functional references
- [hspec](https://hackage.haskell.org/package/hspec) - Behavior-driven testing

---

For more detailed development information, see [DEVELOPMENT.md](DEVELOPMENT.md).
