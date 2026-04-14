#!/usr/bin/env bash
unset COLOR
case "${COLOR:=blue}" in
  blue)
    echo "case:match"
    ;;
  *)
    echo "case:miss"
    ;;
esac
printf "color:%s\n" "$COLOR"
