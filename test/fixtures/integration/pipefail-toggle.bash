#!/usr/bin/env bash
set -o pipefail
if false | true; then
  echo "pipe-then"
else
  echo "pipe-else"
fi
set +o pipefail
if false | true; then
  echo "nopipe-then"
else
  echo "nopipe-else"
fi
