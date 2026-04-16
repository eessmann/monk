# Real-world fixtures

These fixtures are sourced from public repositories and curated for safe, non-destructive validation.

The directory also contains a few curated fixtures built from common Bash script patterns when a full upstream script would be too noisy, too large, or too environment-dependent for automated parity testing.

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
- `neofetch-mini.bash`: reduced slice derived from `neofetch`'s `get_args()` image backend handling
- `argparse-mini.bash`: curated option-parser slice exercising long options, array accumulation, and `--` handling
- `envfile-preview.bash`: curated dotenv-like reader slice exercising `while read`, `continue`, and parameter expansion cleanup
- `path-filter.bash`: curated PATH-walker slice exercising pattern trimming, glob cases, and arithmetic counters
- `semver-normalize.bash`: curated version-normalizer slice exercising here-strings, local variables, and default expansions

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
- `neofetch-mini.bash` is the small generated-output regression slice derived from `neofetch`; it stays in the normal integration suite while full `neofetch.bash` remains bake-off-only.
- The curated fixtures are intentionally self-contained and deterministic so they can stay in the normal integration suite without network, filesystem mutation, or terminal-only behavior.
