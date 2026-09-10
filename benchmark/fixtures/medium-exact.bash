#!/usr/bin/env bash
set -e
say() { local value="$1"; printf '<%s>\n' "$value"; }
for item in alpha 'two words' ''; do say "$item"; done
IFS=:
words='one::three:'
printf '[%s]\n' $words
IFS=' 	
'
x=7
printf 'integer:%s\n' "$((x / 3 * 3))"
case alpha in a*) printf 'match\n' ;& z) printf 'fallthrough\n' ;; esac
