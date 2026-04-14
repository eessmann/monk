#!/usr/bin/env bash
set -e
set -o pipefail
false | cat >/dev/null &
bg_pipe=$!
if wait "$bg_pipe"; then
  echo "wait:ok"
else
  echo "wait:$?"
fi
echo "after"
