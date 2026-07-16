#!/usr/bin/env bash

printf x | if true; then
  printf 'if-stage\n'
  false
else
  true
fi
printf 'if-status:%s\n' "$?"

printf x | case x in
  x)
    printf 'case-stage\n'
    false
    ;;
esac
printf 'case-status:%s\n' "$?"

printf x | for item in one; do
  printf 'for-stage:%s\n' "$item"
  false
done
printf 'for-status:%s\n' "$?"

printf 'line\n' | while IFS= read -r line; do
  printf 'while-stage:%s\n' "$line"
  false
done
printf 'while-status:%s\n' "$?"

printf x | pipeline_function() {
  false
}
printf 'function-status:%s\n' "$?"
