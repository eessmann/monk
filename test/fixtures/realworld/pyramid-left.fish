#!/usr/bin/env fish
# Manual translation of pyramid-left.bash

set rows 5
for i in (seq 0 (math "$rows - 1"))
    set spaces (math "$rows - $i - 1")
    for j in (seq 1 $spaces)
        printf ' '
    end
    for j in (seq 0 $i)
        printf '*'
    end
    echo
end
