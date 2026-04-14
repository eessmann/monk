#!/usr/bin/env bash
unset FOO
printf "arg:%s\n" "x${FOO:=bar}y"
printf "var:%s\n" "$FOO"
