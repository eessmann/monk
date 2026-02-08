#!/usr/bin/env bash
# exit
set -e
false && echo "no"
echo "after-and"
false
echo "after-false"
