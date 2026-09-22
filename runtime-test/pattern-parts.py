#!/usr/bin/env python3
"""Byte parameter-removal oracle preserving each fragment's quote activity."""
import os, random, subprocess, sys
runtime = os.path.abspath(sys.argv[1])
bash = os.fsencode(os.environ.get('MONK_REFERENCE_BASH', 'bash'))
environment = dict(os.environ, LC_ALL='C', LANG='C')
for key in ('BASH_ENV', 'ENV', 'BASHOPTS', 'SHELLOPTS'):
    environment.pop(key, None)
operations = [(b'trim-prefix-short', b'#'), (b'trim-prefix-long', b'##'), (b'trim-suffix-short', b'%'), (b'trim-suffix-long', b'%%')]
subjects = [b'', b'a', b'ababa', b'a*b?x', b'[ab]', b'a-]b', b'\\abc\\', b'\xfe\xffa', b'a\xff\xfe', b'abc123', b'a\n']
patterns = [[(True, p)] for p in (b'', b'*', b'?', b'a*', b'*a', b'[ab]*', b'[!a-z]', b'[[:alpha:]]*', b'[[:digit:]]', b'[\xfe-\xff]*', b'\\*')]
patterns += [[(False, p)] for p in (b'*', b'?', b'[ab]', b'\\', b'\xff')]
patterns += [[(False, b'a*'), (True, b'?*')], [(True, b'*'), (False, b'?x')], [(True, b'['), (False, b'a-z'), (True, b']*')], [(True, b'[[:'), (False, b'alpha'), (True, b':]]*')], [(False, b'['), (True, b'*'), (False, b']')], [(True, b'a'), (False, b'\\'), (True, b'*')]]

def check(operation, spelling, subject, parts):
    assignments = b'subject=$1; ' + b''.join(b'p%d=${%d}; ' % (n, n + 2) for n in range(len(parts)))
    pattern = b''.join((b'${p%d}' if active else b'"${p%d}"') % n for n, (active, _) in enumerate(parts))
    source = assignments + b'printf "%s\\0" "${subject' + spelling + pattern + b'}"'
    oracle = subprocess.run([bash, b'--noprofile', b'--norc', b'-c', source, b'parameter-oracle', subject] + [value for _, value in parts], env=environment, capture_output=True)
    frames = [operation, subject] + [frame for active, value in parts for frame in (b'1' if active else b'0', value)]
    actual = subprocess.run([runtime, '--abi', '2', 'pattern-parts'], input=b'\0'.join(frames) + b'\0', capture_output=True, env=environment)
    assert (actual.returncode, actual.stdout, actual.stderr) == (oracle.returncode, oracle.stdout, oracle.stderr), (operation, subject, parts, actual, oracle)

count = 0
for operation, spelling in operations:
    for subject in subjects:
        for parts in patterns:
            check(operation, spelling, subject, parts)
            count += 1
randomizer = random.Random(431)
for _ in range(120):
    operation, spelling = randomizer.choice(operations)
    subject = bytes(randomizer.choice(b'a*?[]-\\\xff') for _ in range(randomizer.randrange(9)))
    parts = [(bool(randomizer.randrange(2)), bytes(randomizer.choice(b'a*?[]-\\\xff') for _ in range(randomizer.randrange(5)))) for _ in range(randomizer.randrange(1, 4))]
    check(operation, spelling, subject, parts)
    count += 1
for malformed in [b'trim-prefix-short\0a\0bad\0a\0', b'\0'.join([b'trim-prefix-short', b'a', b'1', b'']), b'\0'.join([b'unknown', b'a', b'1', b'a', b''])]:
    actual = subprocess.run([runtime, '--abi', '2', 'pattern-parts'], input=malformed, capture_output=True)
    assert actual.returncode == 125, actual
print('quote-aware parameter-removal Bash byte oracle passed:', count, 'cases')
