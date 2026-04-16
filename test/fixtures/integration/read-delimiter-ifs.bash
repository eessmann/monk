#!/usr/bin/env bash
tmp=$(mktemp)
printf 'a::b;a:b:c;' > "$tmp"
IFS=:
{
  read -u 3 -d ';' -r one two three
  printf "triple:%s|%s|%s\n" "$one" "$two" "$three"
  read -u 3 -d ';' -r first second
  printf "pair:%s|%s\n" "$first" "$second"
} 3<"$tmp"
rm -f "$tmp"
