#!/usr/bin/env bash
prefix=root
source test/fixtures/integration/source-recursive-child.bash left right
printf "status:%s\n" "$?"
if source test/fixtures/integration/source-recursive-child.bash left right; then
  echo unexpected-success
else
  echo expected-failure
fi
printf "after:%s\n" "$prefix"
