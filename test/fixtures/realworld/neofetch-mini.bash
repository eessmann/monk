#!/usr/bin/env bash

# Reduced slice derived from neofetch's get_args() image backend handling.
image_backend="auto"
image_source=""
ascii_distro=""

parse_neofetch_args() {
  while [[ "$1" ]]; do
    case $1 in
      "--backend") image_backend="$2" ;;
      "--source") image_source="$2" ;;
      "--ascii" | "--off")
        image_backend="${1/--}"
        case $2 in
          "-"* | "") ;;
          *) image_source="$2" ;;
        esac
      ;;
      "--ascii_distro")
        image_backend="ascii"
        ascii_distro="$2"
      ;;
    esac

    shift
  done
}

parse_neofetch_args "$@"

printf "backend:%s\n" "$image_backend"
printf "source:%s\n" "$image_source"
printf "ascii_distro:%s\n" "$ascii_distro"
