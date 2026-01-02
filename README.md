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
- **Sources**: Optional recursive translation of `source`/`.` files

### 🛡️ Type-Safe Architecture
- **GADT-based AST** ensures semantic correctness at compile time
- **Comprehensive Fish representation** covers Fish shell's complete syntax
- **Source mapping** preserves original locations for debugging
- **Error recovery** handles partial translations gracefully

### 🔍 Smart Analysis
- **ShellCheck integration** leverages industry-standard Bash parsing
- **Semantic analysis** detects Fish-incompatible patterns
- **Warnings + strict mode** surface behavior changes and allow fail-fast translation
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
# Translate a single script (stdout by default)
monk script.sh > script.fish

# Write output directly to a file
monk script.sh --output script.fish

# Fail on unsupported constructs
monk script.sh --strict

# Recursively translate sourced files (inline)
monk script.sh --recursive --sources inline

# Recursively translate sourced files (separate .fish files)
monk script.sh --recursive --sources separate
```

CLI flags:
- `--output FILE`: write output to a file instead of stdout
- `--strict`: fail on unsupported constructs
- `--quiet-warnings`: suppress warnings
- `--recursive`: follow `source`/`.` and translate referenced files
- `--sources inline|separate`: inline sourced content or emit separate `.fish` files (default `separate`)

Warnings and errors are written to stderr.

### CLI Help (Excerpt)

```text
monk - Translate bash scripts to fish

Usage: monk FILE [--output FILE] [--strict] [--quiet-warnings]
            [--recursive] [--sources MODE]

Available options:
  -o,--output FILE       Write output to a file instead of stdout
  --strict               Fail on unsupported constructs
  -q,--quiet-warnings    Suppress warnings
  --recursive            Recursively translate sourced scripts
  --sources MODE         Source handling mode when --recursive is set
                         (inline|separate)
  -h,--help              Show this help text
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

Monk covers most core Bash constructs and uses a semantic Fish IR to emit idiomatic Fish. The translator is conservative: it emits warnings for constructs that need manual review and can fail fast in strict mode.

## 🤝 Comparison with Babelfish

[Babelfish](https://github.com/bouk/babelfish) is a great Go-based translator that inspired parts of our test coverage and has a strong focus on practical conversions. Monk takes a different path:

- **Approach**: Monk builds and type-checks a semantic Fish IR (GADT-based) before rendering code. Babelfish translates more directly from the Bash AST to Fish text.
- **Diagnostics**: Monk prioritizes explicit warnings, strict mode, and source mapping to make risky translations visible. Babelfish tends to favor straightforward output.
- **Capabilities**: Monk currently emphasizes correctness safeguards, recursive source handling, and type-level modeling. Babelfish has been around longer and may handle edge cases we still miss.
- **Trade-offs**: Monk may be more conservative and sometimes verbose; Babelfish can be faster to adopt for lightweight scripts. We’re still closing gaps and learning from their real-world coverage.

If you’re evaluating both, we recommend trying your scripts with each tool and comparing behavior in Fish.

### Non-trivial Translations (Behavior Notes)

- **Arrays are 1-indexed**: Bash `arr[0]` becomes Fish `$arr[1]`.
- **Parameter expansions**: `${var:off:len}`, `${var#pat}`, `${var//old/new}`, and case mods are lowered to `string` commands; regex/pattern behavior is approximate.
- **Command substitution in strings**: `$(...)` in double-quoted contexts becomes `string join ' '` over the substitution list.
- **Process substitution**: `<(cmd)` becomes `(cmd | psub)`; `>(cmd)` uses a FIFO + background pipeline workaround.
- **Here-docs/strings**: `<<EOF`/`<<<` are lowered to `printf` into process substitution.
- **Globs/extglobs**: simple globs are native; unsupported extglob operators fall back to a bash `extglob` shim.
- **`time` prefix**: `time cmd` is emitted as a Fish timed pipeline.
- **`select` loops**: emulated with `read` and `seq`.

### Known Limitations

| Limitation | Status | Notes |
|---|---|---|
| Word splitting | [ ] manual review | Fish does not perform implicit word splitting. |
| `set -e` semantics | [ ] manual review | Fish error behavior differs from Bash. |
| Non-literal `source` paths | [ ] manual review | Recursive translation only follows literal paths. |
| Coprocesses (`coproc`) | [ ] unsupported | Warnings in normal mode; failure in `--strict`. |
 
Review warnings and test translated scripts in Fish.

## 🛣️ Roadmap

### Near Term
- [ ] Output-equivalence property tests for representative scripts
- [ ] Built-in parity checks and behavioral tests (`pushd`/`popd`, `time`)
- [ ] Improved diagnostics for unsupported constructs and lossy translations
- [ ] Expanded documentation with real-world translation examples

### Longer Term
- [ ] Confidence scoring for translations
- [ ] Performance optimizations for large scripts
- [ ] IDE/editor integrations

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
