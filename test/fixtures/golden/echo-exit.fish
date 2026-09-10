if builtin functions '--all' '--names' | builtin string 'match' '--quiet' '--' '__monk_plan_0_*'
  builtin printf '%s'\n'' 'monk: runtime contract failed: private function namespace is occupied' >&2
  builtin exit '125'
else
  if builtin set '--names' | builtin string 'match' '--invert' '--regex' '--' '^()$' | builtin string 'match' '--quiet' '--' '__monk_plan_0_*'
    builtin printf '%s'\n'' 'monk: runtime contract failed: private variable namespace is occupied' >&2
    builtin exit '125'
  else
    builtin set --global __monk_plan_0_status '0'
    builtin printf '%s'\n'' 'hi'
    builtin set __monk_plan_0_status "$status"
    builtin exit '2'
    builtin exit "$__monk_plan_0_status"
  end
end