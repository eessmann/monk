#!/usr/bin/env bash
read -rd: field
printf "field:%s\n" "$field"
read -r rest
printf "rest:%s\n" "$rest"
