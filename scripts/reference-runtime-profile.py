#!/usr/bin/env python3
"""Identify reference executables by bytes and observable build behavior."""
import argparse
import base64
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess


def profile(bash='bash', fish='fish'):
    env = dict(os.environ, LC_ALL='C', LANG='C')
    for key in ('BASH_ENV', 'ENV', 'SHELLOPTS', 'BASHOPTS'):
        env.pop(key, None)
    result = {'platform': platform.platform(), 'locale': 'C', 'executables': {}, 'bash_probes': {}}
    for name, command in [('bash', bash), ('fish', fish)]:
        path = Path(shutil.which(command) or command).resolve(strict=True)
        version = subprocess.run([str(path), '--version'], env=env, capture_output=True, check=True)
        result['executables'][name] = {'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
                                       'version': version.stdout.decode().splitlines()[0]}
    for value in [r'a\uD800b', r'a\U00110000b', r'a\U7fffffffb', r'a\U80000000b', r'a\Uffffffffb']:
        observed = subprocess.run([result['executables']['bash']['path'], '--noprofile', '--norc', '-c',
                                   'echo -e "$1"', 'reference-probe', value], env=env, capture_output=True, check=True)
        result['bash_probes'][value] = {'stdout_base64': base64.b64encode(observed.stdout).decode(),
                                       'stderr_base64': base64.b64encode(observed.stderr).decode(), 'exit': observed.returncode}
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--bash', default='bash')
    parser.add_argument('--fish', default='fish')
    args = parser.parse_args()
    print(json.dumps(profile(args.bash, args.fish), indent=2))
