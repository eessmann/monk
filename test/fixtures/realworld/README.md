# Real-world fixtures

These fixtures are sourced from public repositories and curated for safe, non-destructive validation.

## Sources

- `hello-world.bash`: https://github.com/ruanyf/simple-bash-scripts/blob/master/scripts/hello-world.sh
- `version-compare.bash`: https://github.com/ruanyf/simple-bash-scripts/blob/master/scripts/versioncompare.sh
- `pyramid-right.bash`: https://github.com/wolandark/BASH_Scripts_For_Everyone/blob/master/ANSI-Printing/Pyramid-Patterns/1-right-half-pyramid.sh
- `pyramid-left.bash`: https://github.com/wolandark/BASH_Scripts_For_Everyone/blob/master/ANSI-Printing/Pyramid-Patterns/2-left-half-pyramid.sh
- `echo-args.bash`: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/echo-args
- `a2l.bash`: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/a2l
- `coat.bash`: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/coat
- `taoc.bash`: https://github.com/oldratlee/useful-scripts/blob/dev-3.x/bin/taoc
- `neofetch.bash`: https://github.com/dylanaraps/neofetch/blob/master/neofetch

## Metadata

Fixtures can include optional sidecar files:

- `<name>.args`: whitespace-separated arguments passed as $1/$2 (bash) or $argv (fish)
- `<name>.stdin`: content piped to stdin when running the fixture

Example:

- `version-compare.args` contains `1.2.3 1.2.10`

## Fixture notes

- `echo-args.bash` is modified to use a `SCRIPT_NAME` default instead of `$0` for deterministic output under test harnesses.
- `taoc.bash` depends on `tac` being available (coreutils on Linux).
- `neofetch.bash` is a large real-world script used for bake-off and integration checks only; manual fish comparison is skipped.
