#!/usr/bin/env bash

# Curated fixture: PATH-style walking with glob cases and pattern trimming.
path_args=("$@")
path_input=${path_args[0]-}
index=0
matches=0

while [[ -n $path_input ]]; do
  entry=${path_input%%:*}
  case $path_input in
    *:*) path_input=${path_input#*:} ;;
    *) path_input="" ;;
  esac

  entry=${entry%/}
  case $entry in
    */bin | */sbin)
      printf '%s:%s\n' "$index" "$entry"
      ((matches++))
      ;;
  esac

  ((index++))
done

printf 'matches:%s\n' "$matches"
