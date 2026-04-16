#!/usr/bin/env bash
tmp=$(mktemp)
printf 'left\nright\0' > "$tmp"
IFS=$'\n'
{
  read -u 3 -d '' -r first second
  printf "vars:%s|%s\n" "$first" "$second"
} 3<"$tmp"
rm -f "$tmp"
