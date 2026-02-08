#!/usr/bin/env bash
set -e
if false; then
  echo "if-then"
fi
echo "after-if"
while false; do
  echo "while"
done
echo "after-while"
until true; do
  echo "until"
done
echo "after-until"
! false
echo "after-not"
