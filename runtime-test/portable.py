#!/usr/bin/env python3
"""Portable ABI and inherited descriptor/byte transport regressions."""
import os
import platform
import subprocess
import sys

runtime = os.path.abspath(sys.argv[1])
target = {('Darwin', 'arm64'): 'aarch64-darwin', ('Linux', 'aarch64'): 'aarch64-linux', ('Linux', 'x86_64'): 'x86_64-linux'}[(platform.system(), platform.machine())]
r = subprocess.run([runtime, '--describe'], capture_output=True)
assert r.returncode == 0, ('native target must be admitted', r.returncode, r.stderr)
lines = r.stdout.splitlines()
assert len(lines) == 3 and lines[0] == b'monk-runtime 2 bash53-i64' and lines[2] == ('target ' + target).encode(), lines
r = subprocess.run([runtime, '--abi', '1', 'echo'], input=b'hello\0', capture_output=True)
assert r.returncode == 125, ('ABI 1 must be rejected by the ABI 2 provider', r)
r = subprocess.run([runtime, '--abi', '2', 'echo'], input=b'hello\0', capture_output=True, env=dict(os.environ, GHCRTS='--definitely-invalid'))
assert (r.returncode, r.stdout, r.stderr) == (0, b'hello\n', b''), ('GHCRTS must not change the protocol', r)


def capture(script, state=b'', mask=7, original=True):
    payload = b'warning\0' + str(mask).encode() + b'\0' + script + b'\0' + b'2\0' + state
    return subprocess.run(['bash', '-c', 'exec "$1" --abi 2 child-capture ' + ('3</dev/null' if original else '3<&-'), 'bash', runtime], input=payload, capture_output=True)


# Both script and scalar state must begin at offset zero on all native targets.
r = capture(b'command cat $argv[1]; printf "\\377\\376\\n\\n"', b'prefix\0state\0')
assert (r.returncode, r.stdout, r.stderr) == (0, b'ok\0' + b'0\0prefixstate\xff\xfe\0', b'warning'), r
# Large output must drain before waiting, preserve bytes, and remove trailing LF.
r = capture(b'command head -c 1048576 /dev/zero; printf x')
assert (r.returncode, r.stdout, r.stderr) == (0, b'ok\0' + b'0\0x\0', b'warning'), r
r = capture(b'true', original=False)
assert r.stdout == b'error\0' + b'125\0child-transport-failure\0', r
r = capture(b'true', mask=6, original=False)
assert (r.returncode, r.stdout) == (0, b'ok\0' + b'0\0\0'), r
r = capture(b'command kill -TERM $fish_pid')
assert (r.returncode, r.stdout) == (0, b'ok\0' + b'143\0\0'), r
print('portable ABI 2 metadata, offsets, byte transport, missing descriptors and signals passed')

# Inherited fds above a subsequently lowered soft limit must not escape. This
# catches range scans that only inspect sysconf(_SC_OPEN_MAX).
import resource
import tempfile
with tempfile.TemporaryDirectory() as workspace:
    probe = os.path.join(workspace, 'probe.py')
    with open(probe, 'w') as stream:
        stream.write('import os,sys\ntry: os.fstat(200)\nexcept OSError: sys.exit(0)\nelse: sys.exit(1)\n')
    fd = os.open(os.devnull, os.O_RDONLY)
    os.dup2(fd, 200)
    os.close(fd)
    def lower_limit():
        resource.setrlimit(resource.RLIMIT_NOFILE, (64, resource.getrlimit(resource.RLIMIT_NOFILE)[1]))
    script = ('command "' + sys.executable + '" "' + probe + '"').encode()
    payload = b'\0' + b'6\0' + script + b'\0' + b'2\0'
    r = subprocess.run([runtime, '--abi', '2', 'child-capture'], input=payload, capture_output=True, pass_fds=(200,), preexec_fn=lower_limit)
    os.close(200)
    assert (r.returncode, r.stdout) == (0, b'ok\0' + b'0\0\0'), ('high inherited descriptor leaked', r)
print('inherited descriptor above lowered soft limit check passed')

# A ready notification establishes ordering before owner termination; no sleep
# is used to infer child readiness. The blocked read is a Fish builtin.
import signal
with tempfile.TemporaryDirectory() as directory:
    workspace = os.path.join(directory, 'temporary')
    os.mkdir(workspace)
    fifo = os.path.join(directory, 'input')
    os.mkfifo(fifo)
    keep_open = os.open(fifo, os.O_RDWR)
    script = ('printf ready >&2; read -l item < "' + fifo + '"').encode()
    payload = b'\0' + b'6\0' + script + b'\0' + b'2\0'
    process = subprocess.Popen([runtime, '--abi', '2', 'child-capture'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=dict(os.environ, TMPDIR=workspace))
    process.stdin.write(payload)
    process.stdin.close()
    process.stdin = None
    assert process.stderr.read(5) == b'ready'
    owned = os.listdir(workspace)
    assert len(owned) == 1, owned
    child_workspace = os.path.join(workspace, owned[0])
    assert os.stat(child_workspace).st_mode & 0o777 == 0o700
    assert all(os.stat(os.path.join(child_workspace, name)).st_mode & 0o777 == 0o600 for name in os.listdir(child_workspace))
    process.send_signal(signal.SIGTERM)
    output, error = process.communicate(timeout=10)
    os.close(keep_open)
    assert process.returncode == 143, (process.returncode, output, error)
    assert os.listdir(workspace) == [], os.listdir(workspace)
print('owner signal cleanup and private file permissions passed')
