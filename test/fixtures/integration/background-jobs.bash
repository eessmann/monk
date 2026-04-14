#!/usr/bin/env bash
set -e
false &
bg_fail=$!
if wait "$bg_fail"; then
  echo "wait1:ok"
else
  echo "wait1:$?"
fi
echo "after1"

set -o pipefail
(printf "left\n"; exit 0) | (cat >/dev/null; exit 7) &
bg_pipe=$!
if wait "$bg_pipe"; then
  echo "wait2:ok"
else
  echo "wait2:$?"
fi
echo "after2"
