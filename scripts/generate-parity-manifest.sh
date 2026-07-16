#!/usr/bin/env bash
set -euo pipefail

monk_bin=${1:?usage: generate-parity-manifest.sh MONK_BIN OUTPUT}
manifest=${2:?usage: generate-parity-manifest.sh MONK_BIN OUTPUT}
manifest_dir=$(dirname "$manifest")
mkdir -p "$manifest_dir"

tmp_dir=$(mktemp -d)
trap 'rm -rf "$tmp_dir"' EXIT

printf 'fixture\ttranslation_success\tfish_syntax\trendered_sha256\tfish_bytes\tdiagnostic_codes\thelper_count\texternal_requirements\n' >"$manifest"

translation_failures=0
syntax_failures=0

while IFS= read -r fixture; do
  safe_name=$(printf '%s' "$fixture" | tr '/ ' '__')
  fish_output="$tmp_dir/$safe_name.fish"
  diagnostics="$tmp_dir/$safe_name.diagnostics"
  success=true
  if ! "$monk_bin" "$fixture" --output "$fish_output" 2>"$diagnostics"; then
    success=false
    translation_failures=$((translation_failures + 1))
  fi

  syntax_ok=false
  if [[ -f "$fish_output" ]]; then
    if fish --no-execute "$fish_output" 2>>"$diagnostics"; then
      syntax_ok=true
    else
      syntax_failures=$((syntax_failures + 1))
    fi
    if command -v sha256sum >/dev/null 2>&1; then
      rendered_hash=$(sha256sum "$fish_output" | cut -d' ' -f1)
    else
      rendered_hash=$(shasum -a 256 "$fish_output" | cut -d' ' -f1)
    fi
    fish_bytes=$(wc -c <"$fish_output" | tr -d ' ')
    helper_count=$(grep -c '^function __monk_' "$fish_output" || true)
  else
    rendered_hash=''
    fish_bytes=0
    helper_count=0
  fi

  diagnostic_codes=$(grep -oE '(error|warning|note)\[[^]]+\]' "$diagnostics" | sed -E 's/^[^[]+\[|\]$//g' | sort -u | paste -sd, - || true)
  external_requirements=$(grep -oE 'runtime requirement: [^ ]+' "$diagnostics" | cut -d' ' -f3 | sort -u | paste -sd, - || true)
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
    "$fixture" "$success" "$syntax_ok" "$rendered_hash" "$fish_bytes" "$diagnostic_codes" "$helper_count" "$external_requirements" >>"$manifest"
done < <(find test/fixtures -type f -name '*.bash' -print | LC_ALL=C sort)

if ((translation_failures > 0 || syntax_failures > 0)); then
  printf 'parity manifest failed: %d translation failure(s), %d Fish syntax failure(s)\n' "$translation_failures" "$syntax_failures" >&2
  exit 1
fi
