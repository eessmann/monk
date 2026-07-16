# Changelog

`monk` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## Unreleased

* Breaking API: replace the raw-backed Fish facade with a structural DSL and
  remove public raw constructors, lowering modules, translator state, raw inline
  results, callback warnings, and confidence scores.
* Public results: expose structural scripts, ordered stable diagnostics,
  nonempty failures, review risk, and deduplicated runtime requirements.
* Sources/output: make Monk's typed source graph authoritative and add typed
  combined/separate output bundles with one optional shared runtime file;
  resolve nested imports from each generated file rather than the caller's CWD,
  reject duplicate output targets, and traverse nested typed source positions
  during inlining. Inlined source argv/redirections preserve exact source
  status after argv restoration. CLI inline output now uses the combined planner.
* Semantics: support standalone negation and covered compound status positions;
  make unsupported statements fail closed; add dedicated here-string and
  extglob fallback diagnostics with strict-mode outcomes.
* Generated runtime: combine exact delimiter capture/assignment into one Python
  process, remove nested Fish status restoration, and add a proven Fish-native
  raw single-variable delimiter path.
* Generated output: bound multiline command-substitution indentation, preserve
  trailing backslashes safely, and keep parameter-operator path separators as
  literal word parts. Full neofetch output now measures a 1.7652 expansion ratio.
* Bake-off: report translated bytes, expansion ratio, helper footprint and
  invocations, external requirements, diagnostic counts, review risk, and
  Hyperfine medians for original Bash versus Monk-generated Fish runtime;
  runtime plans require successful translations and valid Bash/Fish syntax.
* Requirements: retain operation-specific reasons and source ranges for every
  deduplicated helper-backed runtime dependency.
* CI/evidence: add bounded GHC 9.12.2/9.14.1 and Fish 4.6.0/moving-Fish-4 jobs,
  formatting/lint/Haddock gates, full integrations, Linux process-substitution
  evidence, syntax validation, and parity manifests.

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/eessmann/monk/releases
