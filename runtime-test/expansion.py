#!/usr/bin/env python3
"""Bash byte oracle for composed quote activity, splitting and pathname expansion."""
import errno
import itertools
import os
from pathlib import Path
import random
import subprocess
import sys
import tempfile

runtime = os.path.abspath(sys.argv[1])
bash = os.environ.get('MONK_REFERENCE_BASH', 'bash')
env = dict(os.environ, LC_ALL='C', LANG='C')
for key in ('BASH_ENV', 'ENV', 'SHELLOPTS', 'BASHOPTS'):
    env.pop(key, None)


def literal_source(value):
    # Literal test fragments contain only shell glob syntax and safe literal
    # bytes. Protect every other byte from Bash source parsing.
    return b''.join(bytes([c]) if c in b'*?[]!-^' else b'\\' + bytes([c]) for c in value)


def check(ifs, parts, directory):
    frames = [ifs] + [item for pair in parts for item in pair]
    assignments = b'IFS=$1; shift; '
    expression = b''
    values = []
    for index, (mode, value) in enumerate(parts):
        if mode == b'l':
            expression += literal_source(value)
        else:
            variable = b'v' + str(index).encode()
            assignments += variable + b'=$1; shift; '
            values.append(value)
            reference = b'${' + variable + b'}'
            expression += b'"' + reference + b'"' if mode == b'q' else reference
    script = assignments + b'set -- ' + expression + b'; if (( $# )); then printf "%s\\0" "$@"; fi'
    oracle = subprocess.run([os.fsencode(bash), b'--noprofile', b'--norc', b'-c', script, b'word-oracle', ifs, *values],
                            cwd=directory, env=env, capture_output=True, timeout=10)
    actual = subprocess.run([runtime, '--abi', '2', 'expansion'], input=b''.join(frame + b'\0' for frame in frames),
                            cwd=directory, env=env, capture_output=True, timeout=10)
    assert oracle.returncode == 0, (script, oracle)
    assert (actual.returncode, actual.stdout, actual.stderr) == (0, oracle.stdout, b''), (ifs, parts, actual, oracle)


with tempfile.TemporaryDirectory() as temporary:
    directory = Path(temporary)
    # No glob characters here: exhaustive split boundaries, quoted-empty
    # positions, invalid UTF-8, all C IFS whitespace and non-whitespace joining.
    values = [b'', b' ', b'a', b' a ', b':', b'::', b'a:', b':b', b' : a:: b : ', b'\t\na\t', b'\xff:a\xff']
    count = 0
    for ifs in [b'', b' \t\n', b':', b' :\t\n', b'\xff']:
        for left, right in itertools.product(values, repeat=2):
            for middle in [[], [(b'q', b'')]]:
                check(ifs, [(b'e', left), *middle, (b'e', right)], directory)
                count += 1
        for value in values:
            check(ifs, [(b'q', b''), (b'e', value), (b'q', b'')], directory)
            count += 1
    rng = random.Random(220926)
    for _ in range(250):
        parts = [(rng.choice([b'e', b'q', b'l']), rng.choice(values[:9])) for _ in range(rng.randrange(1, 6))]
        check(rng.choice([b'', b' \t\n', b':', b' :\t\n']), parts, directory)
        count += 1
    print('composed IFS/quote byte oracle passed:', count, 'cases', flush=True)

    names = [b'a', b'b', b'z', b'A', b'0', b'9', b'-', b']', b'^', b'!', b':', b'*', b'?', b'[a]',
             b'a.txt', b'b.txt', b'star*', b'.hidden', b'\xff', b'\xfe.txt', b'\\a', b'\\*', b'\\abc']
    for name in names:
        try:
            fd = os.open(os.fsencode(directory) + b'/' + name, os.O_CREAT | os.O_WRONLY, 0o600)
        except OSError as error:
            if error.errno != errno.EILSEQ or all(c < 128 for c in name):
                raise
            print('platform pathname gap: filesystem rejects invalid UTF-8 filename', repr(name), flush=True)
        else:
            os.close(fd)
    (directory / 'dir').mkdir()
    (directory / 'dir' / 'leaf').touch()
    patterns = [b'*', b'?', b'[ab]', b'[a-z]', b'[!a-z]', b'[^a-z]', b'[]a]', b'[-a]', b'[a-]',
                b'[a/]', b'[a-[:digit:]]', b'[!a-[:digit:]]', b'[a[:digit:]-z]', b'[0-9]', b'[[:alpha:]]', b'[[:digit:]]', b'[[:punct:]]', b'[[:space:]]',
                b'[[:print:]]', b'[[:xdigit:]]', b'[[:upper:]]', b'[[:unknown:]]',
                b'[[=a=]]', b'[[.a.]]', b'[', b'[abc', b'[.]hidden', b'.*', b'*.txt',
                b'dir/*', b'dir//*', b'*///', b'missing*', b'\\a*', b'\\*', b'\\*?', b'[\xfe-\xff]']
    for pattern in patterns:
        check(b' \t\n', [(b'e', pattern)], directory)
        check(b' \t\n', [(b'q', pattern)], directory)
    for cls in [b'alnum',b'alpha',b'ascii',b'blank',b'cntrl',b'digit',b'graph',b'lower',b'print',b'punct',b'space',b'upper',b'word',b'xdigit']:
        check(b'', [(b'e', b'[[:' + cls + b':]]')], directory)
    for parts in [[(b'l',b'*'),(b'q',b'.txt')], [(b'q',b'*'),(b'l',b'*')],
                  [(b'l',b'['),(b'q',b'a-z'),(b'l',b']')], [(b'e',b'['),(b'q',b'a-z]')],
                  [(b'e',b'\\'),(b'q',b'*'),(b'l',b'*')], [(b'e',b'\\'),(b'q',b'a'),(b'l',b'*')],
                  [(b'e',b'a* b*'),(b'q',b'.txt')], [(b'q',b'a'),(b'e',b'* b*')],
                  [(b'e',b' * '),(b'q',b''),(b'e',b' ? ')]]:
        check(b' \t\n', parts, directory)
    print('pathname/quote/C byte-class oracle passed', flush=True)

for frames in [[], [b' ' , b'q'], [b' ', b'invalid', b'x']]:
    result = subprocess.run([runtime, '--abi', '2', 'expansion'], input=b''.join(frame+b'\0' for frame in frames), capture_output=True)
    assert result.returncode == 125, (frames, result)
print('invalid expansion frame rejection passed')

# Parameter-pattern matching supplies byte-class coverage even on filesystems
# that cannot store invalid UTF-8 pathnames. Slash is ordinary outside globbing.
for pattern in [b'[a/]', b'[a-z]', b'[!a-z]', b'[\xfe-\xff]', b'[[:alpha:]]', b'[[:punct:]]',
                b'[![:bogus:]]', b'[a-[:digit:]]', b'[!a-[:digit:]]', b'[a[:digit:]-z]']:
    for subject in [b'a', b'Z', b'0', b'/', b'-', b'\xfe', b'\xff']:
        oracle = subprocess.run([os.fsencode(bash), b'--noprofile', b'--norc', b'-c', b'[[ $1 == $2 ]]', b'pattern-oracle', subject, pattern], env=env, capture_output=True)
        actual = subprocess.run([runtime, '--abi', '2', 'pattern'], input=b'match\0' + subject + b'\0' + b'1\0' + pattern + b'\0', capture_output=True)
        assert (actual.returncode, actual.stdout, actual.stderr) == (oracle.returncode, b'', b''), (subject, pattern, actual, oracle)
print('raw-byte bracket matcher oracle passed')
