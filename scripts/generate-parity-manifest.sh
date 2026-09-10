#!/usr/bin/env bash
set -euo pipefail
script_directory=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
exec python3 "$script_directory/generate-parity-manifest.py" "$@"
