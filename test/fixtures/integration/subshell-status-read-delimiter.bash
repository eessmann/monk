#!/usr/bin/env bash
if (
  read -rd: value
  test "$value" = "one"
); then
  echo "ok"
else
  echo "bad"
fi
