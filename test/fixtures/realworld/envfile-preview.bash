#!/usr/bin/env bash

# Curated fixture: dotenv-like parsing with read loops and parameter expansion.
count=0
while IFS= read -r line; do
  case $line in
    '' | '#'*) continue ;;
  esac

  key=${line%%=*}
  value=${line#*=}
  key=${key// /_}
  key=${key^^}

  printf '%s=%s\n' "$key" "$value"
  ((count++))
done

printf 'count:%s\n' "$count"
