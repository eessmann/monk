#!/usr/bin/env bash
tmp=$(mktemp)
printf 'abcdef:rest' > "$tmp"
{
  read -u 3 -d : -n 3 -r chunk
  printf "chunk:%s\n" "$chunk"
  read -u 3 -d : -r next
  printf "next:%s\n" "$next"
  read -u 3 -r tail
  printf "tail:%s\n" "$tail"
} 3<"$tmp"
rm -f "$tmp"
