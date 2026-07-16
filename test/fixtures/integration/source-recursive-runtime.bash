#!/usr/bin/env bash
read -r -d : root rest
printf 'root:%s:%s\n' "$root" "$rest"
source test/fixtures/integration/source-recursive-runtime-child.bash
