if builtin functions '--all' '--names' | builtin string 'match' '--quiet' '--' '__monk_plan_0_*'
  builtin printf '%s'\n'' 'monk: runtime contract failed: private function namespace is occupied' >&2
  builtin exit '125'
else
  if builtin set '--names' | builtin string 'match' '--invert' '--regex' '--' '^()$' | builtin string 'match' '--quiet' '--' '__monk_plan_0_*'
    builtin printf '%s'\n'' 'monk: runtime contract failed: private variable namespace is occupied' >&2
    builtin exit '125'
  else
    builtin set --local __monk_plan_0_native_path "$__monk_plan_0_native_path"
    if builtin test '-n' "$__monk_plan_0_native_path"
      builtin true
    else
      builtin set __monk_plan_0_native_path "$(builtin command '--search' '--' 'monk-runtime')"
    end
    builtin set --local __monk_plan_0_native_path (builtin path 'resolve' '--null-out' '--' "$__monk_plan_0_native_path" | builtin string 'split0')
    builtin set --local __monk_plan_0_native_description (command "$__monk_plan_0_native_path" '--describe' < '/dev/null' 2> '/dev/null')
    if builtin test "$status" '=' '0'
      builtin true
    else
      builtin printf '%s'\n'' 'monk.runtime: missing or incompatible native runtime' >&2
      builtin exit '125'
    end
    if builtin test "$__monk_plan_0_native_description[1]" '=' 'monk-runtime 1 bash53-i64-linux64'
      builtin true
    else
      builtin printf '%s'\n'' 'monk.runtime: missing or incompatible native runtime' >&2
      builtin exit '125'
    end
    builtin set --local __monk_plan_0_native_capabilities (builtin string 'split' ' ' '--' "$__monk_plan_0_native_description[2]")
    if builtin contains '--' 'echo' $__monk_plan_0_native_capabilities
      builtin true
    else
      builtin printf '%s'\n'' 'monk.runtime: missing or incompatible native runtime' >&2
      builtin exit '125'
    end
    if builtin contains '--' 'glob' $__monk_plan_0_native_capabilities
      builtin true
    else
      builtin printf '%s'\n'' 'monk.runtime: missing or incompatible native runtime' >&2
      builtin exit '125'
    end
    builtin set --global __monk_plan_0_status '0'
    function __monk_plan_0_native --no-scope-shadowing
      command "$__monk_plan_0_native_path" $argv
    end
    builtin set --local __monk_plan_0_pathname_fields_0
    begin
      builtin set --local fish_read_limit '0'
      builtin set __monk_plan_0_pathname_fields_0 (builtin printf "%s\\0" '0' 'foo' '1' '*' '0' '.txt' | __monk_plan_0_native '--abi' '1' 'glob' | builtin string 'split0')
    end
    builtin printf "%s\\0" $__monk_plan_0_pathname_fields_0 | __monk_plan_0_native '--abi' '1' 'echo'
    builtin set __monk_plan_0_status "$status"
    builtin exit "$__monk_plan_0_status"
  end
end