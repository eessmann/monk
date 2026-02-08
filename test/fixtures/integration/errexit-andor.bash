#!/usr/bin/env bash
# exit
set -e
echo "start"
false || true
echo "after-or-true"
true && false
echo "after-and-false"
