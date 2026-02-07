#!/usr/bin/env bash
set -euo pipefail

function greet() {
  local name=$1
  echo "Hello, $name"
}

names=(alice bob carol)
for n in "${names[@]}"; do
  greet "$n"
  case $n in
    a*) echo "starts with a" ;;
    b*) echo "starts with b" ;;
    *) echo "other" ;;
  esac
  ((i++))
done

read -n 1 -t 2 key
