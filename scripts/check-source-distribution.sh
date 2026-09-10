#!/usr/bin/env bash
set -euo pipefail
artifact_root=${1:?usage: check-source-distribution.sh ARTIFACT_DIRECTORY}
mkdir -p "$artifact_root"
artifact_root=$(cd "$artifact_root" && pwd -P)
work_directory=$(mktemp -d "$artifact_root/unpacked-XXXXXXXX")
cabal_options=()
if [[ -n ${MONK_CABAL_CONFIG:-} ]]; then
  cabal_config_directory=$(cd "$(dirname "$MONK_CABAL_CONFIG")" && pwd -P)
  cabal_options+=("--config-file=$cabal_config_directory/$(basename "$MONK_CABAL_CONFIG")")
fi
build_options=(-fdevelopment)
if [[ -n ${MONK_GHC:-} ]]; then
  build_options+=("--with-compiler=$MONK_GHC")
fi
if [[ ${MONK_CABAL_OFFLINE:-0} == 1 ]]; then
  build_options+=(--offline)
fi
cabal "${cabal_options[@]}" sdist --builddir="$work_directory/sdist-build" --output-directory="$work_directory"
tar -xzf "$work_directory/monk-0.4.0.tar.gz" -C "$work_directory"
(
  cd "$work_directory/monk-0.4.0"
  printf 'packages: .\n' > cabal.project
  cabal check
  cabal "${cabal_options[@]}" build all --builddir="$work_directory/unpacked-build" --enable-tests --enable-benchmarks "${build_options[@]}"
  cabal "${cabal_options[@]}" install exe:monk exe:monk-runtime --builddir="$work_directory/unpacked-build" --installdir="$work_directory/bin" --install-method=copy "${build_options[@]}"
  export PATH="$work_directory/bin:$PATH"
  printf '%s\n' "printf '%s:%s\\n' 'éλ' \"\$1\"; set -- '' 'two words'; printf '<%s>\\n' \"\$@\"" > 'smoke-éλ.bash'
  printf '%s\n' 'x=value; echo "$x"' >> 'smoke-éλ.bash'
  LC_ALL=C LANG=C "$work_directory/bin/monk" 'smoke-éλ.bash' --strict --output 'combined-éλ.fish'
  LC_ALL=C LANG=C "$work_directory/bin/monk" 'smoke-éλ.bash' --strict --recursive --sources separate --output 'managed-éλ.fish'
  LC_ALL=C LANG=C bash --noprofile --norc 'smoke-éλ.bash' value > bash.stdout
  LC_ALL=C LANG=C fish --no-config 'combined-éλ.fish' value > combined.stdout
  LC_ALL=C LANG=C fish --no-config 'managed-éλ.fish' value > managed.stdout
  cmp bash.stdout combined.stdout
  cmp bash.stdout managed.stdout
  "$work_directory/bin/monk" --help > cli-help.txt
  "$work_directory/bin/monk-runtime" --describe > native-runtime.txt
) > "$work_directory/verification.log" 2>&1
printf '%s\n' "$work_directory" > "$artifact_root/latest-verification.txt"
cat "$work_directory/verification.log"
