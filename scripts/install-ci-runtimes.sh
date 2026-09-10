#!/usr/bin/env bash
# Build the declared Bash profile and install the official standalone Fish asset.
# Downloads and build logs remain available for the CI evidence artifact.
set -euo pipefail
runtime_root=${1:?usage: install-ci-runtimes.sh ABSOLUTE_ROOT pinned-or-moving}
fish_channel=${2:?usage: install-ci-runtimes.sh ABSOLUTE_ROOT pinned-or-moving}
case "$runtime_root" in /*) ;; *) echo 'runtime root must be absolute' >&2; exit 2 ;; esac
case "$fish_channel" in pinned|moving) ;; *) echo 'unknown Fish channel' >&2; exit 2 ;; esac
mkdir -p "$runtime_root/downloads" "$runtime_root/bin"
curl --fail --location --retry 3 https://ftp.gnu.org/gnu/bash/bash-5.3.tar.gz \
  --output "$runtime_root/downloads/bash-5.3.tar.gz"
for patch_number in 001 002 003 004 005 006 007 008 009; do
  patch_file="$runtime_root/downloads/bash53-$patch_number"
  curl --fail --location --retry 3 "https://ftp.gnu.org/gnu/bash/bash-5.3-patches/bash53-$patch_number" --output "$patch_file"
done
if [[ "$fish_channel" == pinned ]]; then
  curl --fail --location --retry 3 \
    https://github.com/fish-shell/fish-shell/releases/download/4.6.0/fish-4.6.0-linux-x86_64.tar.xz \
    --output "$runtime_root/downloads/fish-4.6.0-linux-x86_64.tar.xz"
else
  sudo add-apt-repository --yes ppa:fish-shell/release-4
  sudo apt-get update
  sudo apt-get install --yes fish
fi
script_directory=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
(cd "$runtime_root/downloads" && sha256sum --check --ignore-missing "$script_directory/runtime-sha256.txt")
tar -xzf "$runtime_root/downloads/bash-5.3.tar.gz" -C "$runtime_root"
for patch_number in 001 002 003 004 005 006 007 008 009; do
  patch --directory="$runtime_root/bash-5.3" --strip=0 < "$runtime_root/downloads/bash53-$patch_number"
done
(
  cd "$runtime_root/bash-5.3"
  ./configure --prefix="$runtime_root" --without-bash-malloc
  make -j 4
  make install
) > "$runtime_root/bash-build.log" 2>&1
if [[ "$fish_channel" == pinned ]]; then
  mkdir -p "$runtime_root/fish-unpacked"
  tar -xJf "$runtime_root/downloads/fish-4.6.0-linux-x86_64.tar.xz" -C "$runtime_root/fish-unpacked"
  install -m 755 "$runtime_root/fish-unpacked/fish" "$runtime_root/bin/fish"
fi
export PATH="$runtime_root/bin:$PATH"
profile_path="$runtime_root/bin:/usr/bin:/bin"
bash_version=$(env -i PATH="$profile_path" LC_ALL=C LANG=C bash --noprofile --norc --version)
[[ "$bash_version" == 'GNU bash, version 5.3.9('* ]]
if [[ "$fish_channel" == pinned ]]; then
  [[ $(env -i PATH="$profile_path" LC_ALL=C LANG=C fish --no-config --version) == 'fish, version 4.6.0' ]]
fi
{
  printf '%s\n' 'profile: bash-5.3-fish-4.6'
  printf 'fish channel: %s\n' "$fish_channel"
  printf '%s\n' 'locale: LC_ALL=C LANG=C'
  printf '%s\n' 'profile environment: allowlisted PATH and C locale only; no BASH_ENV, ENV, SHELLOPTS, BASHOPTS, or imported functions'
  printf '%s\n' 'bash invocation: env -i PATH=<runtime-bin:/usr/bin:/bin> LC_ALL=C LANG=C bash --noprofile --norc SCRIPT [ARG ...]'
  printf '%s\n' 'fish invocation: env -i PATH=<runtime-bin:/usr/bin:/bin> LC_ALL=C LANG=C fish --no-config SCRIPT [ARG ...]'
  env -i PATH="$profile_path" LC_ALL=C LANG=C bash --noprofile --norc --version
  env -i PATH="$profile_path" LC_ALL=C LANG=C fish --no-config --version
  python3 --version
  uname -a
  sha256sum "$runtime_root"/downloads/*
  printf '%s\n' 'bash default set +o:'
  env -i PATH="$profile_path" LC_ALL=C LANG=C bash --noprofile --norc -c 'set +o'
  printf '%s\n' 'bash default shopt -p:'
  env -i PATH="$profile_path" LC_ALL=C LANG=C bash --noprofile --norc -c 'shopt -p'
} > "$runtime_root/runtime-evidence.txt"
cat "$runtime_root/runtime-evidence.txt"
if [[ -n ${GITHUB_PATH:-} ]]; then
  printf '%s\n' "$runtime_root/bin" >> "$GITHUB_PATH"
fi
