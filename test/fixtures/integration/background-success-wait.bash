#!/usr/bin/env bash
set -e
true &
bg_ok=$!
if wait "$bg_ok"; then
  echo "wait:ok"
else
  echo "wait:$?"
fi
echo "after"
