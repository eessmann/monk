#!/usr/bin/env fish
# Source: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/echo-args
# Fixture note: use SCRIPT_NAME default to avoid relying on $0 in tests.

function digitCount --argument-names num
  set -l count 0
  while test $num -ne 0
    set count (math --scale 0 $count + 1)
    set num (math --scale 0 $num / 10)
  end
  echo $count
end

set -l script_name script
if set -q SCRIPT_NAME
  set script_name $SCRIPT_NAME
end

set -l arg_count (count $argv)
set -l digit_count (digitCount $arg_count)

function printArg --argument-names idx value digit_count arg_count
  if test -t 1
    printf "%*s/%s: \e[1;31m[\e[1;36m%s\e[1;31m]\e[0m\n" $digit_count $idx $arg_count $value
  else
    printf "%*s/%s: [%s]\n" $digit_count $idx $arg_count $value
  end
end

printArg 0 $script_name $digit_count $arg_count
set -l idx 1
for a in $argv
  printArg $idx $a $digit_count $arg_count
  set idx (math --scale 0 $idx + 1)
end
