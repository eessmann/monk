#!/usr/bin/env bash

# Curated fixture: common long-option parser pattern with arrays and `--`.
mode="summary"
name="world"
color="auto"
tags=()
rest=()
args=("$@")

join_with_bar() {
  local first=1 value
  for value; do
    if ((first)); then
      printf '%s' "$value"
      first=0
    else
      printf '|%s' "$value"
    fi
  done
}

parse_args() {
  local idx=0
  local next_idx=0
  local current=""
  local next=""
  local arg_count=${#args[@]}
  while ((idx < arg_count)); do
    current=${args[idx]}
    next_idx=$((idx + 1))
    next=""
    if ((next_idx < arg_count)); then
      next=${args[next_idx]}
    fi

    case $current in
      --name=*)
        name=${current#*=}
        ;;
      --name)
        name=${next-default}
        ((idx++))
        ;;
      --mode=*)
        mode=${current#*=}
        ;;
      --mode)
        mode=${next-summary}
        ((idx++))
        ;;
      --color=*)
        color=${current#*=}
        ;;
      --tag=*)
        tags+=("${current#*=}")
        ;;
      --tag)
        tags+=("${next-}")
        ((idx++))
        ;;
      --)
        ((idx++))
        while ((idx < arg_count)); do
          rest+=("${args[idx]}")
          ((idx++))
        done
        break
        ;;
      -*)
        rest+=("$current")
        ;;
      *)
        rest+=("$current")
        ;;
    esac
    ((idx++))
  done
}

parse_args

printf 'mode:%s\n' "$mode"
printf 'name:%s\n' "$name"
printf 'color:%s\n' "$color"
printf 'tags:%s\n' "$(join_with_bar "${tags[@]}")"
printf 'rest:%s\n' "$(join_with_bar "${rest[@]}")"
