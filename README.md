# 🐟 Monk - Bash to Fish Shell Transpiler

[![GitHub CI](https://github.com/eessmann/monk/workflows/CI/badge.svg)](https://github.com/eessmann/monk/actions)
[![Build status](https://img.shields.io/travis/eessmann/monk.svg?logo=travis)](https://travis-ci.com/eessmann/monk)
[![Hackage](https://img.shields.io/hackage/v/monk.svg?logo=haskell)](https://hackage.haskell.org/package/monk)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**Monk** is a sophisticated Haskell tool that translates Bash shell scripts into [Fish shell](https://fishshell.com/) scripts, helping you modernize your shell scripting workflow with Fish's more intuitive syntax and powerful features.

## 🎯 Why Monk?

Fish shell offers many advantages over Bash:
- **Intuitive syntax** - No more `$((arithmetic))` or `[[ conditions ]]`
- **Better error handling** - Clear error messages and fail-fast behavior
- **Modern features** - Built-in syntax highlighting, autocompletion, and more
- **Safer defaults** - No word splitting, glob expansion is explicit

But migrating existing Bash scripts by hand is time-consuming and error-prone. Monk automates this process while preserving your script's behavior and adding helpful conversion notes.

## ✨ Features

### 🔄 Comprehensive Translation
- **Control Flow**: `if`/`else`, `while`, `for`, `case` statements
- **Job Control**: Pipelines, background jobs, command substitution  
- **Functions**: Function definitions with proper scoping
- **Variables**: Environment variables, arrays, special variables (`$?`, `$!`, etc.)
- **Redirections**: File descriptors, pipes, and complex redirections

### 🛡️ Type-Safe Architecture
- **GADT-based AST** ensures semantic correctness at compile time
- **Comprehensive Fish representation** covers Fish shell's complete syntax
- **Source mapping** preserves original locations for debugging
- **Error recovery** handles partial translations gracefully

### 🔍 Smart Analysis
- **ShellCheck integration** leverages industry-standard Bash parsing
- **Semantic analysis** detects Fish-incompatible patterns
- **Conversion notes** explains behavior changes and manual review needs
- **Scope analysis** ensures variables are scoped correctly in Fish

## 🚀 Quick Start

### Installation

```bash
# From Hackage (when released)
cabal install monk

# From source
git clone https://github.com/eessmann/monk.git
cd monk
cabal build
```

### Basic Usage

```bash
# Translate a single script
monk translate script.sh > script.fish

# Translate with verbose output
monk translate --verbose script.sh

# Check translation quality
monk analyze script.sh
```

### Example Translation

**Input (Bash):**
```bash
#!/bin/bash
for file in *.txt; do
    if [[ -r "$file" ]]; then
        echo "Processing $file"
        grep -c "pattern" "$file" >> results.log
    fi
done
```

**Output (Fish):**
```fish
#!/usr/bin/env fish
for file in *.txt
    if test -r "$file"
        echo "Processing $file"
        grep -c "pattern" "$file" >> results.log
    end
end
```

## 🏗️ Architecture

Monk uses a sophisticated multi-stage translation pipeline:

```
Bash Script → ShellCheck AST → Fish AST → Fish Script
     ↓              ↓              ↓           ↓
  Original    Parsed &      Type-safe    Generated
   Source     Analyzed    Intermediate     Output
                           Representation
```

### Key Components

- **`Language.Bash.Parser`**: ShellCheck integration for robust Bash parsing
- **`Language.Fish.AST`**: Type-safe Fish shell abstract syntax tree
- **`Language.Fish.Translator`**: Core translation logic with semantic analysis
- **`Language.Fish.Pretty`**: Fish code generation with formatting preservation

## 🧪 Testing

Monk includes comprehensive test coverage:

```bash
# Run all tests
cabal test

# Run with coverage
cabal test --enable-coverage

# Property-based testing
cabal test --test-option="--quickcheck-tests=10000"
```

### Test Categories

- **Unit Tests**: Specific translation scenarios
- **Property Tests**: Semantic preservation guarantees  
- **Integration Tests**: End-to-end script translation
- **Fish Compatibility**: Generated code runs correctly in Fish

## 📊 Current Status

Monk is under active development. Current translation support:

| Feature | Status | Notes |
|---------|---------|-------|
| Basic Commands | ✅ Complete | Full argument and redirection support |
| Control Flow | ✅ Complete | if/else, while, for, case statements |
| Functions | ✅ Complete | With proper Fish scoping |
| Variables | 🚧 Partial | Basic variables, arrays in progress |
| Job Control | 🚧 Partial | Pipelines work, complex jobs in progress |
| Advanced Features | ❌ Planned | Process substitution, co-processes |

### Known Limitations

- **Bash Arrays**: Fish uses different array syntax - manual review needed
- **Process Substitution**: `<(command)` requires rewriting for Fish
- **Complex Parameter Expansion**: `${var%pattern}` needs Fish string functions
- **Arithmetic Expansion**: `$((math))` becomes `math` command calls

## 🛣️ Roadmap

### Version 0.2.0
- [ ] Complete variable and array translation
- [ ] Advanced job control features
- [ ] Interactive translation mode
- [ ] Fish compatibility checking

### Version 0.3.0  
- [ ] Process substitution handling
- [ ] Advanced parameter expansion
- [ ] Bash-specific builtin translation
- [ ] Performance optimizations

### Version 1.0.0
- [ ] Production-ready stability
- [ ] Complete Fish feature coverage
- [ ] IDE integrations
- [ ] Comprehensive documentation

## 🤝 Contributing

We welcome contributions! Monk is built with modern Haskell practices:

### Development Setup

```bash
git clone https://github.com/eessmann/monk.git
cd monk

# Install dependencies
cabal update
cabal build --only-dependencies

# Build and test
cabal build all
cabal test all

# Code formatting
hlint .
```

### Contribution Areas

- **Parser improvements**: Better Bash construct handling
- **Fish semantics**: Expanding AST coverage of Fish features  
- **Translation quality**: More idiomatic Fish output
- **Error handling**: Better error messages and recovery
- **Documentation**: Examples, tutorials, API docs
- **Testing**: More comprehensive test cases

### Code Style

- Follow [Ormolu](https://github.com/tweag/ormolu) formatting
- Use `hlint` for code quality
- Comprehensive documentation with Haddock
- Property-based tests for core functionality
- Type-driven development with GADTs

## 📚 Resources

### Learning Fish Shell
- [Fish Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Fish vs Bash Comparison](https://github.com/fish-shell/fish-shell/wiki/Fish-versus-bash-comparison)
- [Fish Design Document](https://fishshell.com/docs/current/design.html)

### Haskell & Parsing
- [ShellCheck Library](https://hackage.haskell.org/package/ShellCheck)
- [Parser Combinators](https://hackage.haskell.org/package/parsec)
- [Pretty Printing](https://hackage.haskell.org/package/prettyprinter)

## 📄 License

Monk is released under the [MIT License](LICENSE).

## 🙏 Acknowledgments

- **ShellCheck** team for excellent Bash parsing infrastructure
- **Fish Shell** team for creating a fantastic modern shell
- **Haskell** community for powerful language features and libraries

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/eessmann/monk/issues)
- **Discussions**: [GitHub Discussions](https://github.com/eessmann/monk/discussions)  
- **Email**: essmanne@gmail.com

---

**Monk**: *Because every Bash script deserves to be a Fish* 🐟