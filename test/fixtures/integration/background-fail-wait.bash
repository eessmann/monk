#!/usr/bin/env bash
set -e
false &
bg_fail=$!
if wait "$bg_fail"; then
  echo "wait:ok"
else
  echo "wait:$?"
fi
echo "after"
