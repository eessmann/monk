#!/usr/bin/env bash

set -u -o pipefail

out_dir=${1:-/tmp/monk-babelfish-$(date +%Y%m%d%H%M%S)}
mkdir -p "$out_dir"

report="$out_dir/report.tsv"
meta="$out_dir/meta.txt"

monk_sha=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
monk_dirty=""
if ! git diff --quiet --ignore-submodules -- 2>/dev/null; then
  monk_dirty="-dirty"
fi
monk_desc="${monk_sha}${monk_dirty}"

fish_version=$(fish --version 2>/dev/null || echo "unknown")

babelfish_version=""
if [ -n "${BABELFISH_VERSION_OVERRIDE:-}" ]; then
  babelfish_version="$BABELFISH_VERSION_OVERRIDE"
elif command -v brew >/dev/null 2>&1; then
  babelfish_version=$(brew list --versions babelfish 2>/dev/null | awk '{print $2}')
fi
if [ -z "$babelfish_version" ]; then
  babelfish_version="unknown"
fi

{
  echo "date: $(date -Iseconds)"
  echo "monk: $monk_desc"
  echo "fish: $fish_version"
  echo "babelfish: $babelfish_version"
  echo "cwd: $(pwd)"
  echo "out_dir: $out_dir"
} > "$meta"

printf "script\tmonk_rc\tmonk_warns\tmonk_notes\tbabelfish_rc\tout_diff\terr_diff\trc_diff\n" > "$report"

shopt -s nullglob
files=(
  test/fixtures/corpus/*.bash
  benchmark/fixtures/*.bash
  test/fixtures/integration/*.bash
  test/fixtures/golden/*.bash
  test/fixtures/realworld/*.bash
)

normalize_err() {
  sed -e "s|$out_dir|<out_dir>|g" \
      -e "s|\\.monk\\.fish|.fish|g" \
      -e "s|\\.babelfish\\.fish|.fish|g"
}

for f in "${files[@]}"; do
  base=$(basename "$f" .bash)
  monk_fish="$out_dir/$base.monk.fish"
  monk_err="$out_dir/$base.monk.err"
  bab_fish="$out_dir/$base.babelfish.fish"
  bab_err="$out_dir/$base.babelfish.err"
  args_file="${f%.bash}.args"
  stdin_file="${f%.bash}.stdin"
  args=()
  if [ -f "$args_file" ]; then
    # shellcheck disable=SC2207
    args=($(cat "$args_file"))
  fi

  cabal run -v0 monk -- "$f" > "$monk_fish" 2> "$monk_err"
  monk_rc=$?

  babelfish < "$f" > "$bab_fish" 2> "$bab_err"
  bab_rc=$?

  monk_warns=$(rg -c "warning:" "$monk_err" 2>/dev/null || true)
  monk_notes=$(rg -c "^note:" "$monk_err" 2>/dev/null || true)
  if [ -z "$monk_warns" ]; then monk_warns=0; fi
  if [ -z "$monk_notes" ]; then monk_notes=0; fi

  if [ "$monk_rc" -eq 0 ]; then
    if [ -f "$stdin_file" ]; then
      fish "$monk_fish" "${args[@]}" < "$stdin_file" > "$out_dir/$base.monk.out" 2>"$out_dir/$base.monk.runerr"
    else
      fish "$monk_fish" "${args[@]}" > "$out_dir/$base.monk.out" 2>"$out_dir/$base.monk.runerr"
    fi
    echo $? > "$out_dir/$base.monk.rc"
  fi

  if [ "$bab_rc" -eq 0 ]; then
    if [ -f "$stdin_file" ]; then
      fish "$bab_fish" "${args[@]}" < "$stdin_file" > "$out_dir/$base.babelfish.out" 2>"$out_dir/$base.babelfish.runerr"
    else
      fish "$bab_fish" "${args[@]}" > "$out_dir/$base.babelfish.out" 2>"$out_dir/$base.babelfish.runerr"
    fi
    echo $? > "$out_dir/$base.babelfish.rc"
  fi

  out_diff="n/a"
  err_diff="n/a"
  rc_diff="n/a"

  if [ -f "$out_dir/$base.monk.out" ] && [ -f "$out_dir/$base.babelfish.out" ]; then
    if diff -u "$out_dir/$base.babelfish.out" "$out_dir/$base.monk.out" > "$out_dir/$base.out.diff"; then
      out_diff="none"
    else
      out_diff="diff"
    fi
  fi

  if [ -f "$out_dir/$base.monk.runerr" ] && [ -f "$out_dir/$base.babelfish.runerr" ]; then
    normalize_err < "$out_dir/$base.babelfish.runerr" > "$out_dir/$base.babelfish.runerr.norm"
    normalize_err < "$out_dir/$base.monk.runerr" > "$out_dir/$base.monk.runerr.norm"
    if diff -u "$out_dir/$base.babelfish.runerr.norm" "$out_dir/$base.monk.runerr.norm" > "$out_dir/$base.err.diff"; then
      err_diff="none"
    else
      err_diff="diff"
    fi
  fi

  if [ -f "$out_dir/$base.monk.rc" ] && [ -f "$out_dir/$base.babelfish.rc" ]; then
    if diff -u "$out_dir/$base.babelfish.rc" "$out_dir/$base.monk.rc" > "$out_dir/$base.rc.diff"; then
      rc_diff="none"
    else
      rc_diff="diff"
    fi
  fi

  printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" "$f" "$monk_rc" "$monk_warns" "$monk_notes" "$bab_rc" "$out_diff" "$err_diff" "$rc_diff" >> "$report"
  echo "done: $base"
done

echo "$out_dir"
