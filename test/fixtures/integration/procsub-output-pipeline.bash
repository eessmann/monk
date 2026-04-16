#!/usr/bin/env bash
tmp=$(mktemp)
printf 'alpha\nbeta\n' > >(wc -l | tr -d '[:space:]' > "$tmp")
for _ in 1 2 3 4 5 6 7 8 9 10; do
  [ -s "$tmp" ] && break
  sleep 0.1
done
printf "lines:%s\n" "$(cat "$tmp")"
rm -f "$tmp"
