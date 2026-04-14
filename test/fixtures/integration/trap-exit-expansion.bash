#!/usr/bin/env bash
x=before
trap 'printf "%s\n" "$x"' EXIT
x=after
printf "body\n"
