if builtin functions '--all' '--names' | builtin string 'match' '--quiet' '--' '__monk_plan_0_*'
  builtin printf '%s'\n'' 'monk: runtime contract failed: private function namespace is occupied' >&2
  builtin exit '125'
else
  if builtin set '--names' | builtin string 'match' '--invert' '--regex' '--' '^()$' | builtin string 'match' '--quiet' '--' '__monk_plan_0_*'
    builtin printf '%s'\n'' 'monk: runtime contract failed: private variable namespace is occupied' >&2
    builtin exit '125'
  else
    if builtin set '--query' '--universal' 'x'
      builtin printf '%s'\n'' 'monk: runtime contract failed: universal binding x' >&2
      builtin exit '125'
    else
      if builtin set '--query' '--path' 'x'
        builtin printf '%s'\n'' 'monk: runtime contract failed: path binding x' >&2
        builtin exit '125'
      else
        if builtin set '--query' 'x'
          if builtin test "$(builtin count $x)" '=' '1'
            if builtin set '--query' '--local' 'x'
              builtin printf '%s'\n'' 'monk: runtime contract failed: local binding x' >&2
              builtin exit '125'
            else
              if builtin set '--query' '--export' 'x'
                builtin true
              else
                builtin printf '%s'\n'' 'monk: runtime contract failed: non-environment binding x' >&2
                builtin exit '125'
              end
            end
          else
            builtin printf '%s'\n'' 'monk: runtime contract failed: non-scalar binding x' >&2
            builtin exit '125'
          end
        else
          builtin true
        end
      end
    end
    and begin
      builtin set --global __monk_plan_0_status '0'
      builtin set --local __monk_plan_0_pattern_subject_0 "$x"
      builtin test "$__monk_plan_0_pattern_subject_0" '=' '''foo'
      builtin set __monk_plan_0_status "$status"
      if builtin test "$__monk_plan_0_status" '=' '0'
        builtin printf '%s'\n'' 'ok'
        builtin set __monk_plan_0_status "$status"
      else
        builtin set __monk_plan_0_status '0'
      end
      builtin exit "$__monk_plan_0_status"
    end
  end
end