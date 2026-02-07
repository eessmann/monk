#!/usr/bin/env fish
# Manual translation of pyramid-right.bash

set rows 5
for i in (seq 0 (math "$rows - 1"))
    for j in (seq 0 $i)
        printf '*'
    end
    echo
end
