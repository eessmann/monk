#!/usr/bin/env fish
# Source: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/coat

if not test -t 1
  exec cat $argv
end

set -g rotate_colors 33 35 36 31 32 37 34
set -g color_index 0

function rotateColorPrint --argument-names content
  if string match -qr '^[[:space:]]*$' -- $content
    printf '%s' "$content"
  else
    set -l palette_count (count $rotate_colors)
    set -l idx (math --scale 0 "$color_index % $palette_count")
    set -l color $rotate_colors[(math --scale 0 "$idx + 1")]
    set color_index (math --scale 0 $color_index + 1)
    printf '\e[1;%sm%s\e[0m' $color "$content"
  end
end

function rotateColorPrintln --argument-names content
  rotateColorPrint "$content"
  printf '\n'
end

function colorLines
  set -l line ''
  while read -l line
    rotateColorPrintln "$line"
  end
  if test -n "$line"
    rotateColorPrint "$line"
  end
end

if test (count $argv) -eq 0
  colorLines
else
  cat $argv | colorLines
end
