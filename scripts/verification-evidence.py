#!/usr/bin/env python3
"""Record a local command, source identity before/after, and binary hashes.

A successful command is build provenance only when build inputs remain unchanged.
The receipt never asserts that other commands, platforms, or CI were verified.
"""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
import stat
import platform
import shutil
from pathlib import Path
import subprocess
import time

PRODUCTION_DIRS = ('app', 'src', 'runtime-app', 'runtime-src', 'runtime-cbits', 'nix')
PRODUCTION_FILES = ('monk.cabal', 'cabal.project', 'cabal.project.freeze', 'cabal.project.local',
                    'devenv.nix', 'devenv.yaml', 'devenv.lock', 'devenv.local.nix',
                    'devenv.local.yaml', 'Setup.hs', 'scripts/verify-runtime-package.py')
BUILD_DIRS = (*PRODUCTION_DIRS, 'test', 'runtime-test', 'benchmark', 'scripts',
              'harness-support', 'docs', '.github')
BUILD_FILES = (*PRODUCTION_FILES, 'README.md', 'CHANGELOG.md', 'LICENSE',
               'LICENSE.md', '.hlint.yaml', '.ormolu', '.gitignore')
IGNORED_DIRS = {'__pycache__', '.git', '.devenv', '.direnv', 'artifacts', 'dist', 'result'}
GENERATED_SUFFIXES = {'.pyc', '.pyo', '.o', '.hi', '.dyn_o', '.dyn_hi', '.hie'}


def digest(data):
    return hashlib.sha256(data).hexdigest()


def identity(files):
    return {'sha256': digest(json.dumps(files, sort_keys=True).encode()), 'files': files}


def source_identity(root):
    paths = set()
    for folder in BUILD_DIRS:
        for directory, folders, names in os.walk(root / folder):
            folders[:] = [name for name in folders if name not in IGNORED_DIRS
                          and not name.startswith(('dist-', 'result-'))]
            for name in names:
                path = Path(directory) / name
                if path.suffix not in GENERATED_SUFFIXES and path.is_file():
                    paths.add(path)
    paths.update(root / name for name in BUILD_FILES if (root / name).is_file())
    files = [{'path': str(p.relative_to(root)), 'sha256': digest(p.read_bytes()),
              'mode': stat.S_IMODE(p.stat().st_mode),
              **({'symlink_target': os.readlink(p)} if p.is_symlink() else {})}
             for p in sorted(paths)]
    return identity(files)


def production_identity(manifest):
    return identity([row for row in manifest['files'] if row['path'] in PRODUCTION_FILES
                     or row['path'].split('/')[0] in PRODUCTION_DIRS])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--binary', action='append', default=[], type=Path)
    parser.add_argument('command', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.command[1:] if args.command[:1] == ['--'] else args.command
    if not command:
        parser.error('command required after --')
    output = args.output.resolve()
    output.mkdir(parents=True)
    root = Path.cwd().resolve()
    before = source_identity(root)
    collector_sha = digest(Path(__file__).read_bytes())
    toolchain = {name: str(Path(found).resolve()) for name in ('ghc', 'cabal', 'bash', 'fish', 'python3')
                 if (found := shutil.which(name))}
    environment = {key: os.environ.get(key) for key in
                   ('NIX_GHC', 'NIX_GHCPKG', 'NIX_GHC_LIBDIR', 'CABAL_CONFIG', 'LC_ALL', 'LANG', 'TMPDIR')}
    started = datetime.now(timezone.utc).isoformat()
    start = time.monotonic()
    with (output / 'command.log').open('wb') as log:
        result = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT)
    after = source_identity(root)
    binaries = []
    for path in args.binary:
        resolved = path.resolve()
        if resolved.is_file():
            binaries.append({'path': str(resolved), 'sha256': digest(resolved.read_bytes()),
                             'bytes': resolved.stat().st_size})
        else:
            binaries.append({'path': str(resolved), 'missing': True})
    stable = before['sha256'] == after['sha256']
    binaries_complete = all(not row.get('missing') for row in binaries)
    production_before = production_identity(before)
    production_after = production_identity(after)
    receipt = {'schema': 2, 'timestamp': started, 'cwd': str(root), 'command': command,
               'exit': result.returncode, 'elapsed_seconds': time.monotonic() - start,
               'before': before, 'after': after, 'build_inputs_unchanged': stable,
               'production_before': production_before, 'production_after': production_after,
               'production_inputs_unchanged': production_before['sha256'] == production_after['sha256'],
               'successful_stable_command': result.returncode == 0 and stable and binaries_complete,
               'requested_binaries_present': binaries_complete,
               'binaries': binaries, 'log_sha256': digest((output / 'command.log').read_bytes()),
               'collector_sha256': collector_sha, 'toolchain_paths': toolchain,
               'environment': environment, 'platform': platform.platform()}
    (output / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps({k: receipt[k] for k in ('exit','build_inputs_unchanged','successful_stable_command')}))
    raise SystemExit(result.returncode if result.returncode else (3 if not stable else (0 if binaries_complete else 4)))


if __name__ == '__main__':
    main()
