#!/usr/bin/env bash

run_slice() {
  local msg="outer-scope"
  {
    printf "bg:%s\n" "$msg"
  } &
  bg_job=$!
  wait "$bg_job"
  printf "after:%s\n" "$msg"
}

run_slice
