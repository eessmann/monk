#!/usr/bin/env bash
target=$(mktemp)
rm -f "$target"
unset OUT
printf "hello\n" > "${OUT:=$target}"
if [ "$OUT" = "$target" ]; then
  echo "var:match"
else
  echo "var:mismatch"
fi
printf "file:%s\n" "$(cat "$target")"
rm -f "$target"
