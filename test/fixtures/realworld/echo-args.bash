#!/usr/bin/env bash
# Source: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/echo-args
# Fixture note: use SCRIPT_NAME default to avoid relying on $0 in tests.
set -eEuo pipefail

SCRIPT_NAME=${SCRIPT_NAME:-script}

digitCount() {
  # argument 1(num) is always a non-negative integer in this script usage,
  # so NO argument validation logic.
  local num=$1 count=0
  while ((num != 0)); do
    ((++count))
    ((num = num / 10))
  done
  echo "$count"
}

digit_count=$(digitCount $#)
readonly arg_count=$# digit_count

readonly RED='\e[1;31m' BLUE='\e[1;36m' COLOR_RESET='\e[0m'
printArg() {
  local idx=$1 value=$2

  # if stdout is a terminal, turn on color output.
  #   '-t' check: is a terminal?
  #   check isatty in bash https://stackoverflow.com/questions/10022323
  if [ -t 1 ]; then
    printf "%${digit_count}s/%s: ${RED}[${BLUE}%s${RED}]${COLOR_RESET}\n" "$idx" "$arg_count" "$value"
  else
    printf "%${digit_count}s/%s: [%s]\n" "$idx" "$arg_count" "$value"
  fi
}

printArg 0 "$SCRIPT_NAME"
idx=1
for a; do
  printArg $((idx++)) "$a"
done
