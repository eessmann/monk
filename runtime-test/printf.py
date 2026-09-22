#!/usr/bin/env python3
"""Byte oracle for the compiled printf writer; source formats are admitted first."""
import os
import subprocess
import sys

runtime = sys.argv[1]
env = {**os.environ, "LC_ALL": "C", "LANG": "C"}
cases = [
    [b"%s", b""], [b"<%s>", b"a b", b"", b"\xff"],
    [b"%s:%d:%%\n", b"hello", b"-9223372036854775808", b"last"],
    [b"%d:%d", b"9223372036854775807"], [b"literal", b"ignored"],
    [b"--", b"-x:%s", b"ok"], [br"\a\b\e\f\n\r\t\v\\"],
    [br"\0\000\1\12\123\777\x1\x12", b"unused"], [b""],
]
for frames in cases:
    oracle = subprocess.run(["bash", "--noprofile", "--norc", "-c", 'printf "$@"', "printf-case", *frames], env=env, capture_output=True, check=True)
    actual = subprocess.run([runtime, "--abi", "2", "printf"], input=b"".join(value + b"\0" for value in frames), env=env, capture_output=True)
    assert (actual.returncode, actual.stdout, actual.stderr) == (oracle.returncode, oracle.stdout, oracle.stderr), (frames, actual, oracle)
value = b"x" * 1048576
actual = subprocess.run([runtime, "--abi", "2", "printf"], input=b"%s\0" + value + b"\0", capture_output=True, env=env)
assert (actual.returncode, actual.stdout, actual.stderr) == (0, value, b""), "large writer payload"
print("printf raw-byte and large-payload Bash oracle passed")
