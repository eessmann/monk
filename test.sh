#! /bin/bash
grep -v '^#' < test.sh | sed '/^$/d' | wc -l

until true; do
    until true; do
        true;
    done;
done;
while true; do
    while true; do
        true;
    done;
done;