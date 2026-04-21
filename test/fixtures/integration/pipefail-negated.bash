#!/usr/bin/env bash
set -e
set -o pipefail
if ! false | true; then
  echo "negated-pipe"
else
  echo "bad"
fi
echo "after-negated-pipe"
