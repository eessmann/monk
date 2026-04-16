#!/usr/bin/env bash

for version_arg in "$@"; do
  raw=${version_arg#v}
  case $raw in
    *-*)
      core=${raw%%-*}
      suffix=${raw#*-}
      ;;
    *)
      core=$raw
      suffix="release"
      ;;
  esac
  major=${core%%.*}
  rest=${core#*.}
  case $rest in
    *.*)
      minor=${rest%%.*}
      patch=${rest#*.}
      ;;
    *)
      minor=$rest
      patch=0
      ;;
  esac

  printf '%s -> %s|%s|%s|%s\n' "$version_arg" "$major" "$minor" "$patch" "$suffix"
done
