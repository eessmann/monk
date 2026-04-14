#!/usr/bin/env bash
tmp=$(mktemp)
printf "alpha" > >(wc -c > "$tmp")
for _ in 1 2 3 4 5 6 7 8 9 10; do
  [ -s "$tmp" ] && break
  sleep 0.1
done
printf "count:%s\n" "$(tr -d '[:space:]' < "$tmp")"
rm -f "$tmp"
