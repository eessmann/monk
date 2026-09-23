#!/usr/bin/env bash
set -euo pipefail
exec cabal run monk-tool -- parity manifest "$@"
