#!/usr/bin/env python3
"""Native-parent signal identity for owned directory writes and trap boundary."""
import os
import select
import subprocess
import sys
import tempfile

monk, runtime = map(os.path.abspath, sys.argv[1:3])
environment = dict(os.environ, LC_ALL='C', LANG='C')
for key in ('BASH_ENV', 'ENV', 'SHELLOPTS', 'BASHOPTS', 'MONK_SESSION_SOCKET', 'MONK_SESSION_TOKEN', 'MONK_SESSION_REPLY', 'MONK_SESSION_FDS'):
    environment.pop(key, None)

cases = [('', 'pwd'), ('', 'pwd -P'), ('', 'pushd /tmp'), ('OLDPWD=/tmp\n', 'cd -'), ('pushd /tmp >/dev/null\n', 'popd'), ('', 'popd 2>&1'), ('', 'cd /definitely-missing-monk-directory 2>&1')]
for prepare, operation in cases:
    with tempfile.TemporaryDirectory() as temporary:
        directory = os.path.realpath(temporary)
        ready, release = directory + '/ready', directory + '/release'
        continued = directory + '/continued'
        os.mkfifo(ready)
        os.mkfifo(release)
        source = prepare + "printf 'ready\\n' >" + ready + '\nread -r gate <' + release + '\nfalse\n' + operation + '\nprintf continued >' + continued + '\n'
        bash_path, fish_path = directory + '/source.sh', directory + '/output.fish'
        open(bash_path, 'w').write(source)
        env = dict(environment, PWD=directory, OLDPWD='/tmp')
        translated = subprocess.run([monk, bash_path, '--strict', '--directory-contract', 'stable', '--runtime', runtime, '-o', fish_path], cwd=directory, env=env, capture_output=True)
        assert translated.returncode == 0, (operation, translated)
        outcomes = []
        for executable, arguments in [('bash', ['--noprofile', '--norc', bash_path]), ('fish', ['--no-config', fish_path])]:
            if os.path.exists(continued): os.unlink(continued)
            ready_fd = os.open(ready, os.O_RDWR | os.O_NONBLOCK)
            release_fd = os.open(release, os.O_RDWR)
            reader, writer = os.pipe()
            process = subprocess.Popen([executable] + arguments, cwd=directory, env=env, stdout=writer, stderr=subprocess.PIPE)
            os.close(writer)
            seen = b''
            while b'ready\n' not in seen and select.select([ready_fd], [], [], 5)[0]:
                seen += os.read(ready_fd, 4096)
            # The source cannot reach the tested write until this parent has removed the final pipe reader.
            os.close(reader)
            os.write(release_fd, b'go\n')
            _, error = process.communicate(timeout=10)
            os.close(ready_fd)
            os.close(release_fd)
            assert seen == b'ready\n', (operation, executable, seen, process.returncode, error)
            outcomes.append((process.returncode, error, os.path.exists(continued)))
        assert outcomes[0][0] == -13 and not outcomes[0][2], (operation, outcomes)
        assert outcomes[1] == outcomes[0], (operation, outcomes)
with tempfile.TemporaryDirectory() as directory:
    source = directory + '/trap-directory.sh'
    open(source, 'w').write("trap 'printf callback' EXIT\npwd\n")
    rejected = subprocess.run([monk, source, '--strict', '--directory-contract', 'stable', '--runtime', runtime], capture_output=True, env=environment)
    assert rejected.returncode != 0, rejected
print('directory writes preserve native SIGPIPE; directory/trap composition is rejected before effects')
