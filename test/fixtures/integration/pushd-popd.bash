start=$(pwd)
tmp="/tmp/monk_pushd_test"
rm -rf "$tmp"
mkdir -p "$tmp/one" "$tmp/two"

pushd "$tmp/one" > /dev/null
pwd

pushd "$tmp/two" > /dev/null
pwd

popd > /dev/null
pwd

popd > /dev/null
pwd

rm -rf "$tmp"
