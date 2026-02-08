# 🐟 Monk - Bash to Fish Shell Transpiler

[![GitHub CI](https://github.com/eessmann/monk/workflows/CI/badge.svg)](https://github.com/eessmann/monk/actions)
[![Build status](https://img.shields.io/travis/eessmann/monk.svg?logo=travis)](https://travis-ci.com/eessmann/monk)
[![Hackage](https://img.shields.io/hackage/v/monk.svg?logo=haskell)](https://hackage.haskell.org/package/monk)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

**Monk** is a Haskell tool that translates Bash shell scripts into [Fish shell](https://fishshell.com/) scripts, helping you modernize your shell scripting workflow with Fish's more intuitive syntax and powerful features.

## 🎯 Why Fish and Monk?

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
- **Arithmetic**: `((...))` status, assignments, postfix/prefix `++/--`, short-circuit and ternary lowering
- **Read flags**: `-n`, `-t`, `-u`, `-a` mapped to Fish equivalents (with notes for lossy cases)

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
- **Inline notes** for lossy translations (subshell isolation, `read -r`, `set -e`, etc.)

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
Monk also emits a translation confidence score and warning summary.

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

### Library API

Monk exposes a small, focused API via the `Monk` module:

- `translateBashFile` / `translateBashScript`: parse + translate
- `translateParseResult`: translate an existing ShellCheck `ParseResult`
- `TranslationResult`: contains the translated AST plus `TranslateState`
- `renderTranslation` / `translationStatements`: render or inspect output

Example:

```haskell
import Monk

main :: IO ()
main = do
  result <- translateBashFile defaultConfig "script.sh"
  case result of
    Left (ParseErrors errs) -> print errs
    Left (TranslateFailure err) -> print err
    Right tr -> do
      putStrLn (toString (renderTranslation tr))
      print (warnings (translationState tr))
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

Monk uses a multi-stage translation pipeline:

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

# Run benchmarks (uses benchmark/fixtures)
cabal bench
```

## Docs

- `docs/design/translator-todo.md`: translation semantics notes and open items

### Test Categories

- **Unit Tests**: Specific translation scenarios
- **Property Tests**: Semantic preservation guarantees  
- **Integration Tests**: End-to-end script translation
- **Fish Compatibility**: Generated code runs correctly in Fish

## 📊 Current Status

Monk covers most core Bash constructs and uses a semantic Fish IR to emit idiomatic Fish. The translator is conservative: it emits warnings and inline notes for constructs that need manual review and can fail fast in strict mode. We also hoist side-effecting expansions across arguments, redirections, and case patterns, and lower short-circuit arithmetic into conditional evaluation to preserve side effects. Recent additions include `read` flag mapping (`-n/-t/-u/-a`) with IFS splitting notes and best-effort `set -e`/`pipefail` emulation.

### Non-trivial Translations (Behavior Notes)

- **Arrays are 1-indexed**: Bash `arr[0]` becomes Fish `$arr[1]`.
- **Parameter expansions**: `${var:off:len}`, `${var#pat}`, `${var//old/new}`, and case mods are lowered to `string` commands; regex/pattern behavior is approximate.
- **Command substitution in strings**: `$(...)` in double-quoted contexts becomes `string join ' '` over the substitution list.
- **Process substitution**: `<(cmd)` becomes `(cmd | psub)`; `>(cmd)` uses a FIFO + background pipeline workaround.
- **Here-docs/strings**: `<<EOF`/`<<<` are lowered to `printf` into process substitution.
- **Globs/extglobs**: simple globs are native; unsupported extglob operators fall back to a bash `extglob` shim.
- **`time` prefix**: `time cmd` is emitted as a Fish timed pipeline.
- **`select` loops**: emulated with `read` and `seq`.
- **Arithmetic short-circuit**: `a && b` and ternary arithmetic use temp vars and `if test` to preserve side effects.
- **`read` parity**: `-n/-t/-u/-a` map to fish flags; IFS splitting differences emit notes.
- **Errexit/pipefail**: `set -e`/`set -o pipefail` are emulated (details below).

### Errexit / Pipefail Emulation

Monk emulates `set -e` by wrapping top-level commands and pipelines as `cmd; or exit $status`. This is best-effort and intentionally does **not** wrap condition lists (e.g., `if`, `while`, `until`) to match bash’s errexit exceptions. Caveats include nuanced bash rules around command substitutions, `!`, and compound lists that aren’t perfectly modeled.

For `set -o pipefail`, Monk injects a helper at the top of the output:

```fish
function __monk_pipefail
  ...
end
```

Pipelines are wrapped to call `__monk_pipefail $pipestatus`, which returns the first non-zero status. This is best-effort and may differ from bash in complex cases (e.g., nested pipelines, background jobs, or when `pipestatus` is modified).

### Known Limitations

| Limitation | Status | Notes |
|---|---|---|
| Word splitting | [ ] manual review | Fish does not perform implicit word splitting. |
| `set -e` / `pipefail` semantics | [ ] best-effort | Emulated via `cmd; or exit $status` and `__monk_pipefail $pipestatus`; see caveats above. |
| Non-literal `source` paths | [ ] manual review | Recursive translation only follows literal paths (notes emitted). |
| Coprocesses (`coproc`) | [ ] unsupported | Warnings in normal mode; failure in `--strict`. |
 
Review warnings and test translated scripts in Fish.

## 🛣️ Roadmap

### Near Term
- [ ] Deeper parity for `read` (`-d`, delimiter/IFS behaviors) and errexit/pipefail edge cases
- [ ] Expanded documentation with real-world translation examples

### Longer Term
- [ ] Performance optimizations for large scripts

## 🤝 Contributing

We welcome contributions! Monk is built with modern Haskell practices:

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
- 
---

**Monk**: *Because every Bash script deserves to be a Fish* 🐟
