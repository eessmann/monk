#!/usr/bin/env bash
IFS=:
read -r line
printf "line:%s\n" "$line"
read -a arr
printf "arr:%s|%s|%s\n" "${arr[0]}" "${arr[1]}" "${arr[2]}"
read -n 3 chunk
printf "chunk:%s\n" "$chunk"
