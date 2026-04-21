#!/usr/bin/env bash
set -e
false && {
  echo "and-branch"
}
echo "after-and-group"
false || {
  echo "or-branch"
}
echo "after-or-group"
