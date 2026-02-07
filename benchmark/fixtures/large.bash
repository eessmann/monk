#!/usr/bin/env bash
set -euo pipefail

sum=0
for ((i=0; i<100; i++)); do
  sum=$((sum + i))
  if [[ $i -eq 50 ]]; then
    echo "halfway"
  fi
  if ((i % 10 == 0)); then
    printf '%s\n' "$i" | tee /tmp/bench_out >/dev/null
  fi
done

case $sum in
  0) echo "zero" ;;
  *) echo "sum=$sum" ;;
esac

read -a items
for item in "${items[@]}"; do
  echo "$item"
done
