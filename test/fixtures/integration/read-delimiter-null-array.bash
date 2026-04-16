#!/usr/bin/env bash
tmp=$(mktemp)
printf 'alpha\nbeta\0' > "$tmp"
IFS=$'\n'
{
  read -u 3 -d '' -ra items
  printf "count:%s\n" "${#items[@]}"
  printf "items:%s|%s\n" "${items[0]}" "${items[1]}"
} 3<"$tmp"
rm -f "$tmp"
