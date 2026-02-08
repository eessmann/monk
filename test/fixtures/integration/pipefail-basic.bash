#!/usr/bin/env bash
# exit
set -e
set -o pipefail
echo "pipe-start"
true | false | true
echo "after-pipe"
