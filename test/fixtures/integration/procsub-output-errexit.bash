#!/usr/bin/env bash
set -e
false > >(true)
printf 'after\n'
