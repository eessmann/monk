#!/usr/bin/env bash
set -euo pipefail

tracked=$(git ls-files -- '*.py')
if [[ -n $tracked ]]; then
  printf 'Tracked Python files remain:\n%s\n' "$tracked" >&2
  exit 1
fi

if git grep -n -E 'python|[.]py' -- .github/workflows devenv.nix monk.cabal nix scripts runtime-test ':!scripts/check-tooling-language.sh'; then
  printf 'Active tooling still refers to Python.\n' >&2
  exit 1
fi
