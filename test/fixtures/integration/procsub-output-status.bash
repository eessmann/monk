#!/usr/bin/env bash
bash -c 'exit 42' > >(true)
printf 'producer-exact:%s\n' "$?"

true > >(bash -c 'exit 7')
printf 'consumer-ignored:%s\n' "$?"

if false > >(true); then
  printf 'if:false\n'
else
  printf 'if:true\n'
fi

false > >(true) && printf 'and:bad\n' || printf 'or:producer\n'
true > >(false) && printf 'and:producer\n' || printf 'or:bad\n'
