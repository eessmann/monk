#!/usr/bin/env bash
# Compatibility entry point: obtain every runtime from the locked Nix inputs.
set -euo pipefail
runtime_root=${1:?usage: install-ci-runtimes.sh ABSOLUTE_ROOT pinned-or-moving}
fish_channel=${2:?usage: install-ci-runtimes.sh ABSOLUTE_ROOT pinned-or-moving}
case "$runtime_root" in /*) ;; *) echo 'runtime root must be absolute' >&2; exit 2 ;; esac
case "$fish_channel" in pinned|moving) ;; *) echo 'unknown Fish channel' >&2; exit 2 ;; esac
script_directory=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
devenv -O monk.fishChannel:string "$fish_channel" shell -- bash -c '
  set -euo pipefail
  destination=$1
  scripts=$2
  mkdir -p "$destination/bin"
  ln -s "$(command -v bash)" "$destination/bin/bash"
  ln -s "$(command -v fish)" "$destination/bin/fish"
  python3 "$scripts/reference-runtime-profile.py" > "$destination/runtime-evidence.json"
' runtime-install "$runtime_root" "$script_directory"
cat "$runtime_root/runtime-evidence.json"
if [[ -n ${GITHUB_PATH:-} ]]; then
  printf '%s\n' "$runtime_root/bin" >> "$GITHUB_PATH"
fi
