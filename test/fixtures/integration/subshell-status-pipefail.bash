#!/usr/bin/env bash
set -o pipefail
if (false | true); then
  echo "bad"
else
  echo "ok"
fi
