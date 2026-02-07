#!/usr/bin/env fish
# Manual translation of version-compare.bash

set -l VERSION (if test (count $argv) -ge 1; printf '%s\n' $argv[1]; else printf '%s\n' 1.2.3; end)
set -l VERSION2 (if test (count $argv) -ge 2; printf '%s\n' $argv[2]; else printf '%s\n' 1.2.10; end)

function version_gt
    set -l first (printf '%s\n' $argv | sort -V | head -n 1)
    test "$first[1]" != "$argv[1]"
end

function version_le
    set -l first (printf '%s\n' $argv | sort -V | head -n 1)
    test "$first[1]" = "$argv[1]"
end

function version_lt
    set -l first (printf '%s\n' $argv | sort -rV | head -n 1)
    test "$first[1]" != "$argv[1]"
end

function version_ge
    set -l first (printf '%s\n' $argv | sort -rV | head -n 1)
    test "$first[1]" = "$argv[1]"
end

if version_gt $VERSION $VERSION2
    echo "$VERSION is greater than $VERSION2"
end

if version_le $VERSION $VERSION2
    echo "$VERSION is less than or equal to $VERSION2"
end

if version_lt $VERSION $VERSION2
    echo "$VERSION is less than $VERSION2"
end

if version_ge $VERSION $VERSION2
    echo "$VERSION is greater than or equal to $VERSION2"
end
