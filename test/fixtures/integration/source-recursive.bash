#!/usr/bin/env bash
prefix=root
source test/fixtures/integration/source-recursive-child.bash left right
printf "after:%s\n" "$prefix"
