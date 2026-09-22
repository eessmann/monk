#!/usr/bin/env python3
"""Bounded exec PID, byte argv, closed stdio and source-aware errno results."""
import os
import subprocess
import sys
import tempfile
runtime = os.path.abspath(sys.argv[1])
env = dict(os.environ, LC_ALL='C', LANG='C')
probe = 'import os,sys; os.write(1,str(os.getpid()).encode()+b":"+os.fsencode(sys.argv[1]))'
p = subprocess.Popen([runtime, '--abi', '2', 'exec-site', 'source.sh', '1', sys.executable, '-c', probe, b'\xff'], env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
out, err = p.communicate(timeout=10)
assert (p.returncode, out, err) == (0, str(p.pid).encode()+b':\xff', b''), (p.returncode, out, err)
closed = 'import os,sys\ntry: os.fstat(int(sys.argv[1]))\nexcept OSError: sys.exit(0)\nelse: sys.exit(1)\n'
for descriptor in (0, 1, 2):
    p = subprocess.run([runtime, '--abi', '2', 'exec-site', 'source.sh', '1', sys.executable, '-c', closed, str(descriptor)], capture_output=True, env=env, preexec_fn=lambda: os.close(descriptor), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'', b''), (descriptor, p)
with tempfile.TemporaryDirectory() as directory:
    command = os.path.join(directory, 'program')
    open(command, 'w').write('#!/bin/sh\nexit 0\n')
    for mode, expected, reason in [(0o600, 126, b'Permission denied'), (None, 127, b'No such file or directory')]:
        if mode is None: os.unlink(command)
        else: os.chmod(command, mode)
        p = subprocess.run([runtime, '--abi', '2', 'exec-site', 'source.sh', '12', command], capture_output=True, env=env, timeout=10)
        assert (p.returncode, p.stdout, p.stderr) == (expected, b'', b'source.sh: line 12: '+os.fsencode(command)+b': '+reason+b'\n'), p
        for descriptor in (0, 1, 2):
            p = subprocess.run([runtime, '--abi', '2', 'exec-site', 'source.sh', '12', command], capture_output=True, env=env, preexec_fn=lambda: os.close(descriptor), timeout=10)
            diagnostic = b'' if descriptor == 2 else b'source.sh: line 12: '+os.fsencode(command)+b': '+reason+b'\n'
            assert (p.returncode, p.stdout, p.stderr) == (expected, b'', diagnostic), (descriptor, p)
print('exec-site keeps actual PID, byte argv, closed stdio and source errno status/diagnostics passed')
with tempfile.TemporaryDirectory() as directory:
    os.mkdir(directory+'/directory')
    open(directory+'/plain','w').close()
    os.symlink('loop',directory+'/loop')
    open(directory+'/binary','wb').write(b'\xff\0garbage')
    os.chmod(directory+'/binary',0o700)
    for command, code, reason in [('directory',126,b'Is a directory'),('plain/child',126,b'Not a directory'),('loop',126,b'Too many levels of symbolic links'),('absent',127,b'No such file or directory'),('binary',126,b'cannot execute binary file: Exec format error')]:
        p=subprocess.run([runtime,'--abi','2','exec-site','source.sh','3','./'+command],cwd=directory,capture_output=True,env=env,timeout=10)
        assert (p.returncode,p.stdout,p.stderr)==(code,b'',b'source.sh: line 3: ./'+command.encode()+b': '+reason+b'\n'),p
print('directory, ENOTDIR, ELOOP, absent and invalid-binary exec failures match Bash passed')
