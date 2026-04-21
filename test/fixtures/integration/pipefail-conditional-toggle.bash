#!/usr/bin/env bash
set -e
if true; then
  set -o pipefail
fi
if false | true; then
  echo "pipe-then"
else
  echo "pipe-else"
fi
if true; then
  set +o pipefail
fi
if false | true; then
  echo "nopipe-then"
else
  echo "nopipe-else"
fi
