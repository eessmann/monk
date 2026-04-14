#!/usr/bin/env bash
i=0
j=0
(( i++ && j++ ))
echo "and1:$i,$j"
i=1
j=0
(( i++ && j++ ))
echo "and2:$i,$j"
i=0
j=0
(( i++ || j++ ))
echo "or1:$i,$j"
i=1
j=0
(( i++ || j++ ))
echo "or2:$i,$j"
i=0
j=0
i_else=0
(( i++ ? j++ : i_else++ ))
echo "tern1:$i,$j,$i_else"
i=1
j=0
i_else=0
(( i++ ? j++ : i_else++ ))
echo "tern2:$i,$j,$i_else"
