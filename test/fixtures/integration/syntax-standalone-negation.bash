#!/usr/bin/env bash
! false
printf 'false:%s\n' "$?"
! true
printf 'true:%s\n' "$?"
