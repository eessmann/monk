#!/usr/bin/env bash
set -euo pipefail
runtime=${1:?pass monk-runtime executable}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
printf '%s\0' 'warning from source' 7 'printf "a\\0b\\n\\n"; exit 7' 2 > "$work/input"
"$runtime" --abi 1 child-capture 3</dev/null < "$work/input" > "$work/actual" 2> "$work/error"
printf 'ok\0007\000ab\000' > "$work/expected"
cmp "$work/expected" "$work/actual"
printf %s 'warning from source' > "$work/expected"
cmp "$work/expected" "$work/error"
printf '%s\0' '' 7 'read -l line; printf "%s" "$line"; exit 9' 2 > "$work/input"
printf 'original stdin\n' > "$work/stdin"
set +e
"$runtime" --abi 1 child-run 3< "$work/stdin" < "$work/input" > "$work/actual"
status=$?
set -e
[[ $status == 9 ]]
printf %s 'original stdin' > "$work/expected"
cmp "$work/expected" "$work/actual"
printf 'malformed' | "$runtime" --abi 1 child-capture 3</dev/null > "$work/actual"
printf 'error\000125\000child-transport-failure\000' > "$work/expected"
cmp "$work/expected" "$work/actual"
echo 'child transport checks passed'
# Child stdin may intentionally be closed; the metadata remains on fd0.
printf '%s\0' '' 6 'true' 2 > "$work/input"
"$runtime" --abi 1 child-run 3<&- < "$work/input"
# Exact bytes, trailing newlines, and numeric signal status survive framing.
printf '%s\0' '' 7 'printf "\\377\\376\\n\\n"' 2 > "$work/input"
"$runtime" --abi 1 child-capture 3</dev/null < "$work/input" > "$work/actual"
printf 'ok\0000\000\377\376\000' > "$work/expected"
cmp "$work/expected" "$work/actual"
printf '%s\0' '' 7 'command kill -TERM $fish_pid' 2 > "$work/input"
"$runtime" --abi 1 child-capture 3</dev/null < "$work/input" > "$work/actual"
printf 'ok\000143\000\000' > "$work/expected"
cmp "$work/expected" "$work/actual"
# Children cannot inherit private descriptors from a runtime provider.
printf '%s\0' '' 7 'test ! -e /proc/self/fd/9' 2 > "$work/input"
"$runtime" --abi 1 child-run 9< "$work/input" 3</dev/null < "$work/input"
# Metadata SHLVL supplies the original Fish depth without an extra shell level.
printf '%s\0' '' 7 'printf "%s" $SHLVL' 27 > "$work/input"
"$runtime" --abi 1 child-capture 3</dev/null < "$work/input" > "$work/actual"
printf 'ok\0000\00027\000' > "$work/expected"
cmp "$work/expected" "$work/actual"
echo 'extended child transport checks passed'
# Closed standard output/error must stay closed through runtime startup and exec.
printf '%s\0' '' 5 "command bash -c 'test ! -e /proc/self/fd/1'" 2 > "$work/input"
"$runtime" --abi 1 child-run 3</dev/null < "$work/input" 1>&-
printf '%s\0' '' 3 "command bash -c 'test ! -e /proc/self/fd/2'" 2 > "$work/input"
"$runtime" --abi 1 child-run 3</dev/null < "$work/input" 2>&-
# A real external producer must terminate with SIGPIPE, not loop or become 125.
printf '%s\0' '' 7 'command yes' 2 > "$work/input"
set +e
"$runtime" --abi 1 child-run 3</dev/null < "$work/input" | head -c 1 > "$work/actual"
status=${PIPESTATUS[0]}
set -e
[[ $status == 141 ]]
# Compare a literal script with Bash independently of materialization.
bash --noprofile --norc -c 'printf "a\n\nb\n\n"; exit 7' > "$work/reference" || [[ $? == 7 ]]
printf '%s\0' '' 7 'printf "a\n\nb\n\n"; exit 7' 2 > "$work/input"
set +e
"$runtime" --abi 1 child-run 3</dev/null < "$work/input" > "$work/actual"
status=$?
set -e
[[ $status == 7 ]]
cmp "$work/reference" "$work/actual"
echo 'signal and descriptor checks passed'
# Descriptor observations run before GHC can recycle closed inherited slots.
set +e
"$runtime" --abi 1 descriptor-state
all_open=$?
"$runtime" --abi 1 descriptor-state 0<&-
closed_in=$?
"$runtime" --abi 1 descriptor-state 1>&-
closed_out=$?
"$runtime" --abi 1 descriptor-state 2>&-
closed_err=$?
"$runtime" --abi 1 descriptor-state 0<&- 1>&- 2>&-
all_closed=$?
set -e
[[ $all_open == 7 && $closed_in == 6 && $closed_out == 5 && $closed_err == 3 && $all_closed == 0 ]]
echo 'initial descriptor snapshot checks passed'
# Emit the one NUL warning while draining, before later child stderr effects.
printf '%s\0' 'warning-first' 7 'printf "\0"; sleep 0.1; printf later >&2; printf "\0"' 2 > "$work/input"
"$runtime" --abi 1 child-capture 3</dev/null < "$work/input" > "$work/actual" 2> "$work/error"
printf %s 'warning-firstlater' > "$work/expected"
cmp "$work/expected" "$work/error"
echo 'streaming warning check passed'
# A declared open stdin without the owned fd3 must not read an RTS-reused fd.
printf '%s\0' '' 7 'true' 2 > "$work/input"
set +e
"$runtime" --abi 1 child-run 3<&- < "$work/input" 2> "$work/error"
status=$?
set -e
[[ $status == 125 ]]
echo 'missing original stdin check passed'
