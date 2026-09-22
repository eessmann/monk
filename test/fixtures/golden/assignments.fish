if builtin functions --all --names | builtin string match --quiet -- '__monk_plan_0_*'
  builtin printf '%s'\n 'monk: runtime contract failed: private function namespace is occupied' >&2
  builtin exit 125
end
if builtin set --names | builtin string match --quiet -- '__monk_plan_0_*'
  builtin printf '%s'\n 'monk: runtime contract failed: private variable namespace is occupied' >&2
  builtin exit 125
end
if builtin set --query --universal foo
  builtin printf '%s'\n 'monk: runtime contract failed: universal binding foo' >&2
  builtin exit 125
end
if builtin set --query --path foo
  builtin printf '%s'\n 'monk: runtime contract failed: path binding foo' >&2
  builtin exit 125
end
if builtin set --query foo
  if builtin test "$(builtin count $foo)" '=' 1
    builtin true
  else
    builtin printf '%s'\n 'monk: runtime contract failed: non-scalar binding foo' >&2
    builtin exit 125
  end
  if builtin set --query --local foo
    builtin printf '%s'\n 'monk: runtime contract failed: local binding foo' >&2
    builtin exit 125
  end
  if builtin set --query --export foo
    builtin true
  else
    builtin printf '%s'\n 'monk: runtime contract failed: non-environment binding foo' >&2
    builtin exit 125
  end
end
if builtin set --query arr
  builtin printf '%s'\n 'monk: runtime contract failed: preexisting array binding arr' >&2
  builtin exit 125
end
builtin set --global __monk_plan_0_status 0
if builtin set --query --global __monk_plan_0_binding_export_foo
  builtin set --global "$__monk_plan_0_binding_export_foo" foo bar
  builtin set --global __monk_plan_0_binding_environment_foo
else
  if builtin set --query --global foo
    builtin set --global foo bar
  else
    builtin set --global --unpath foo bar
  end
end
builtin set __monk_plan_0_status 0
builtin set --global --unexport --unpath arr a b
builtin set __monk_plan_0_status 0
builtin exit "$__monk_plan_0_status"