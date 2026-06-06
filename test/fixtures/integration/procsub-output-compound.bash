#!/usr/bin/env bash
tmp=$(mktemp)
printf 'alpha\nbeta\n' > >(while IFS= read -r line; do printf '<%s>\n' "$line"; done > "$tmp")
for _ in 1 2 3 4 5 6 7 8 9 10; do
  grep -q '<beta>' "$tmp" && break
  sleep 0.1
done
printf 'compound:%s\n' "$(tr '\n' '|' < "$tmp")"
rm -f "$tmp"
