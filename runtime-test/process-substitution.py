#!/usr/bin/env python3
"""Real-pipe descriptor-path process substitutions; no synchronized data files."""
import os, subprocess, sys, tempfile
runtime = os.path.abspath(sys.argv[1])
probe = subprocess.run([runtime, '--abi', '2', 'pipe-paths'], capture_output=True, timeout=5)
assert (probe.returncode, probe.stdout, probe.stderr) == (0, b'', b''), probe
header = 'function rpc\n begin\n printf "%s\\0" $argv | command "$MONK_RUNTIME" --abi 2 session-client --reply\n end 3<&0 4>&1 5>&2\n set -g response (string split0 < "$MONK_SESSION_REPLY")\nend\n'
with tempfile.TemporaryDirectory() as directory:
    destination = os.path.join(directory, 'output')
    body = """rpc substitution 7 input external /usr/bin/printf 'producer\n'
set job $response[3]
set endpoint $response[4]
# Waiting first forces producer completion before the consumer opens its path.
rpc wait 7 source.sh 1 $job
rpc run 7 external /bin/cat "$endpoint"
printf 'input=%s\n' $response[2]
rpc substitution 7 output external /bin/sh -c 'cat > "$1"' child DESTINATION
set job $response[3]
set endpoint $response[4]
rpc run 7 external /bin/sh -c 'printf consumer > "$1"' child "$endpoint"
rpc wait 7 source.sh 1 $job
printf 'output=%s\n' $response[2]
set payload (string repeat -n 300000 x)
rpc substitution 7 input builtin source.sh 1 echo "$payload"
set job $response[3]
set endpoint $response[4]
rpc run 7 external /usr/bin/head -c 1 "$endpoint"
rpc wait 7 source.sh 1 $job
printf ':signal=%s\n' $response[2]
""".replace('DESTINATION', "'" + destination + "'")
    path = os.path.join(directory, 'evaluator.fish')
    open(path, 'w').write(header + body)
    p = subprocess.run([runtime, '--abi', '2', 'session-run', path], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'producer\ninput=0\noutput=0\nx:signal=141\n', b''), p
    assert open(destination, 'rb').read() == b'consumer'
print('input/output pipe endpoints, producer-before-open EOF, actual PID wait and early-reader SIGPIPE passed')

with tempfile.TemporaryDirectory() as directory:
    body = """rpc fd-push 7
rpc substitution 7 input external /usr/bin/printf 'redirected\n'
set job $response[3]
rpc fd-endpoint 7 source.sh 1 0 $response[5]
rpc read 7 source.sh 1 0 1 '\n' -1 '' scalar 1
printf 'read=%s\n' $response[4]
rpc fd-pop 7
rpc wait 7 source.sh 1 $job
printf 'wait=%s\n' $response[2]
"""
    path = os.path.join(directory, 'evaluator.fish')
    open(path, 'w').write(header + body)
    p = subprocess.run([runtime, '--abi', '2', 'session-run', path], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'read=redirected\nwait=0\n', b''), p
print('endpoint lease installs into scoped stdin for native read passed')

# wait with no operands selects normal background jobs and only the process
# substitution still denoted by $!. Older substitution PIDs remain explicit.
import select
for next_kind in ('substitution', 'background'):
    with tempfile.TemporaryDirectory() as directory:
        fifo = os.path.join(directory, 'release')
        os.mkfifo(fifo)
        release = os.open(fifo, os.O_RDWR)
        later = "rpc substitution 7 output external /usr/bin/printf 'second\\n'" if next_kind == 'substitution' else 'rpc spawn 7 external /usr/bin/true'
        body = "rpc substitution 7 output external /bin/sh -c 'printf first-start\\n; read value < \"$1\"; printf first-end\\n; exit 7' source '" + fifo + "'\nset first $response[3]\n" + later + "\nrpc run 7 external /usr/bin/true\nrpc wait 7 source.sh 1\nprintf 'parent\\n'\nrpc wait 7 source.sh 1 $first\nprintf 'first-status=%s\\n' $response[2]\n"
        # Preserve newline escapes inside the literal shell command.
        body = body.replace('printf first-start\\n;', 'printf "first-start\\n";').replace('printf first-end\\n;', 'printf "first-end\\n";')
        path = os.path.join(directory, 'evaluator.fish')
        open(path, 'w').write(header + body)
        p = subprocess.Popen([runtime, '--abi', '2', 'session-run', path], stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=dict(os.environ, MONK_RUNTIME=runtime))
        observed = b''
        while b'parent\n' not in observed and select.select([p.stdout], [], [], 5)[0]:
            chunk = os.read(p.stdout.fileno(), 4096)
            if not chunk: break
            observed += chunk
        os.write(release, b'release\n')
        out, err = p.communicate(timeout=10)
        os.close(release)
        assert b'parent\n' in observed, ('wait0 incorrectly blocked on older substitution', next_kind, observed, out, err)
        assert p.returncode == 0 and err == b'' and b'first-status=7\n' in observed + out, (next_kind, p.returncode, observed, out, err)
print('wait-all selects only latest process substitution and preserves explicit older PID status passed')
