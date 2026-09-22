#!/usr/bin/env python3
"""Bounded direct writers preserve raw bytes, source diagnostics and SIGPIPE."""
import os
import signal
import subprocess
import sys

runtime = os.path.abspath(sys.argv[1])
env = dict(os.environ, LC_ALL='C', LANG='C')

def frames(values):
    return b''.join(value + b'\0' for value in values)

def invoke(name, arguments, *, close=(), stdout=subprocess.PIPE):
    def prepare():
        for fd in close:
            os.close(fd)
    return subprocess.run([runtime, '--abi', '2', 'write-builtin'],
                          input=frames([b'source.sh', b'12', name, *arguments]),
                          stdout=stdout, stderr=subprocess.PIPE, env=env,
                          preexec_fn=prepare, timeout=10)

for name, args, expected in [
    (b'echo', [b'one', b'', b'three'], b'one  three\n'),
    (b'echo-bytes', [b'already expanded\n'], b'already expanded\n'),
    (b'echo', [b'-ne', br'a\0b\xff'], b'a\0b\xff'),
    (b'printf', [b'<%s>:%d\\n', b'\xff', b'-42'], b'<\xff>:-42\n'),
    (b'printf', [b'%s', b'x' * 1048576], b'x' * 1048576),
]:
    p = invoke(name, args)
    assert (p.returncode, p.stdout, p.stderr) == (0, expected, b''), (name, p)
    p = invoke(name, args, close=(1,))
    assert (p.returncode, p.stdout, p.stderr) == (1, b'', b'source.sh: line 12: ' + (b'echo' if name == b'echo-bytes' else name) + b': write error: Bad file descriptor\n'), (name, p)
    p = invoke(name, args, close=(1, 2))
    assert (p.returncode, p.stdout, p.stderr) == (1, b'', b''), (name, p)
    reader, writer = os.pipe()
    os.close(reader)
    try:
        p = invoke(name, args, stdout=writer)
    finally:
        os.close(writer)
    assert (p.returncode, p.stderr) == (-signal.SIGPIPE, b''), (name, p)

# A reader that consumes a prefix then closes exercises a partial successful
# raw write followed by SIGPIPE, rather than only a pipe with no initial reader.
reader, writer = os.pipe()
p = subprocess.Popen([runtime, '--abi', '2', 'write-builtin'],
                     stdin=subprocess.PIPE, stdout=writer, stderr=subprocess.PIPE, env=env)
try:
    p.stdin.write(frames([b'source.sh', b'12', b'printf', b'%s', b'z' * 1048576]))
    p.stdin.close()
    p.stdin = None
    received = os.read(reader, 64)
finally:
    os.close(reader)
    os.close(writer)
_, errors = p.communicate(timeout=10)
assert received == b'z' * 64, received
assert (p.returncode, errors) == (-signal.SIGPIPE, b''), (p.returncode, errors)

# Describe must flush its complete metadata before immediate success, and
# preserve actual SIGPIPE when that flush reaches a closed reader.
p = subprocess.run([runtime, '--describe'], capture_output=True, env=env, timeout=10)
assert p.returncode == 0 and len(p.stdout.splitlines()) == 3 and p.stderr == b'', p
reader, writer = os.pipe()
os.close(reader)
try:
    p = subprocess.run([runtime, '--describe'], stdout=writer, stderr=subprocess.PIPE, env=env, timeout=10)
finally:
    os.close(writer)
assert (p.returncode, p.stderr) == (-signal.SIGPIPE, b''), p

for name, args in [(b'printf', [b'']), (b'echo', [b'-n'])]:
    p = invoke(name, args, close=(1,))
    assert (p.returncode, p.stdout, p.stderr) == (0, b'', b''), (name, p)

for payload in [b'unterminated', b'', frames([b'source.sh', b'0', b'echo']),
                frames([b'source.sh', b'01', b'echo']),
                frames([b'source.sh', b'1', b'eval', b'printf x']),
                frames([b'source.sh', b'1', b'echo-bytes', b'a', b'b']),
                frames([b'source.sh', b'1', b'printf', b'%q', b'x'])]:
    p = subprocess.run([runtime, '--abi', '2', 'write-builtin'], input=payload,
                       capture_output=True, env=env, timeout=10)
    assert p.returncode == 125 and p.stdout == b'', p

for arguments, expected in [(['13'], -signal.SIGPIPE), (['9'], 125), ([], 125)]:
    p = subprocess.run([runtime, '--abi', '2', 'raise-signal', *arguments],
                       stdin=subprocess.DEVNULL, capture_output=True, env=env, timeout=10)
    assert p.returncode == expected and p.stdout == b'', p
print('direct byte writers, empty outputs, metadata, source errors and actual SIGPIPE passed')

# Optional compiler exercises the emitted helper and propagation to its owning
# Fish process. The native standalone entry records descriptors before Fish
# normalizes missing stdio to /dev/null. Source filenames stay identical.
if len(sys.argv) > 2:
    import tempfile
    from pathlib import Path
    monk = os.path.abspath(sys.argv[2])
    with tempfile.TemporaryDirectory(prefix='monk-direct-output-') as directory:
        directory = Path(directory)
        source_path = directory / 'input.bash'
        target_path = directory / 'input.fish'
        for source in ["printf x", "echo x", "printf ''", "echo -n", "printf '%s' $'\\xff'", "echo 'a b'"]:
            source_path.write_text(source)
            translated = subprocess.run([monk, str(source_path), '--strict'], capture_output=True, env=env, timeout=15)
            assert translated.returncode == 0, translated
            target_path.write_bytes(translated.stdout)
            for mode in ('ordinary', 'closed-stdout', 'closed-both', 'no-reader'):
                observed = []
                for command in [['bash', str(source_path)], [runtime, '--abi', '2', 'launch', str(target_path)]]:
                    descriptor = None
                    if mode == 'no-reader':
                        reader, descriptor = os.pipe()
                        os.close(reader)
                    def prepare():
                        if mode in ('closed-stdout', 'closed-both'):
                            os.close(1)
                        if mode == 'closed-both':
                            os.close(2)
                    try:
                        result = subprocess.run(command, stdin=subprocess.DEVNULL,
                                                stdout=descriptor if descriptor is not None else subprocess.PIPE,
                                                stderr=subprocess.PIPE, env=env, preexec_fn=prepare, timeout=15)
                        observed.append((result.returncode, result.stdout or b'', result.stderr))
                    finally:
                        if descriptor is not None:
                            os.close(descriptor)
                assert observed[0] == observed[1], (source, mode, observed)
    print('compiled direct writers match Bash bytes, empty outputs, closed streams and signal termination')
