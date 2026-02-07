#!/usr/bin/env bash
# Source: https://github.com/ruanyf/simple-bash-scripts/blob/master/scripts/versioncompare.sh
# Test fixture note: provide defaults to avoid empty arguments.

VERSION=${1:-1.2.3}
VERSION2=${2:-1.2.10}

function version_gt() { test "$(echo "$@" | tr " " "\n" | sort -V | head -n 1)" != "$1"; }
function version_le() { test "$(echo "$@" | tr " " "\n" | sort -V | head -n 1)" == "$1"; }
function version_lt() { test "$(echo "$@" | tr " " "\n" | sort -rV | head -n 1)" != "$1"; }
function version_ge() { test "$(echo "$@" | tr " " "\n" | sort -rV | head -n 1)" == "$1"; }

if version_gt $VERSION $VERSION2; then
	echo "$VERSION is greater than $VERSION2"
fi

if version_le $VERSION $VERSION2; then
	echo "$VERSION is less than or equal to $VERSION2"
fi

if version_lt $VERSION $VERSION2; then
	echo "$VERSION is less than $VERSION2"
fi

if version_ge $VERSION $VERSION2; then
	echo "$VERSION is greater than or equal to $VERSION2"
fi
