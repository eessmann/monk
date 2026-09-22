#!/usr/bin/env python3
"""Native session process ownership and RPC byte-stream regression tests."""
import os, subprocess, sys, tempfile
runtime = os.path.abspath(sys.argv[1])
with tempfile.TemporaryDirectory() as directory:
    script = os.path.join(directory, 'evaluator.fish')
    with open(script, 'w') as stream:
        stream.write('''begin
    set -l response (printf '%s\\0' ping 0 | command "$MONK_RUNTIME" --abi 2 session-client | string split0)
    test "$response[1]" = ok; or exit 99
    printf 'owner=%s\\n' "$response[3]"
end 3<&0 4>&1 5>&2
''')
    process = subprocess.Popen([runtime, '--abi', '2', 'session-run', script], stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=dict(os.environ, MONK_RUNTIME=runtime))
    out, err = process.communicate(timeout=10)
    assert (process.returncode, out, err) == (0, ('owner=%s\n' % process.pid).encode(), b''), (process.returncode, out, err)
print('session owner ping and real owner PID passed')

def evaluator(directory, body):
    path = os.path.join(directory, 'evaluate.fish')
    with open(path, 'w') as stream:
        stream.write('function rpc\n begin\n printf "%s\\0" $argv | command "$MONK_RUNTIME" --abi 2 session-client --reply\n end 3<&0 4>&1 5>&2\n set -g response (string split0 < "$MONK_SESSION_REPLY")\nend\n' + body + '\n')
    return path

with tempfile.TemporaryDirectory() as directory:
    script = evaluator(directory, '''rpc run 7 external /bin/sh -c 'printf output; exit 7'
set r $response
printf 'status=%s\\n' $r[2]
rpc spawn 7 external /bin/sh -c 'exit 9'
set p $response
rpc wait 7 source.sh 12 $p[3]
set w $response
rpc wait 7 source.sh 12 $p[3]
set w2 $response
printf 'wait=%s,%s\\n' $w[2] $w2[2]
rpc wait 7 source.sh 12
set all $response
rpc wait 7 source.sh 12 $p[3]
set missing $response
printf 'all=%s;after=%s\\n' $all[2] $missing[2]''')
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert p.returncode == 0 and p.stdout == b'outputstatus=7\nwait=9,9\nall=0;after=127\n', p
    assert b'source.sh: line 12: wait: pid ' in p.stderr and b'is not a child of this shell\n' in p.stderr, p
print('foreground streams/status, cached repeated wait and wait-all consumption passed')

# The outer native owner must exit without waiting for a blocked background
# region. Only release the region after observing owner exit.
with tempfile.TemporaryDirectory() as directory:
    fifo = os.path.join(directory, 'release')
    done = os.path.join(directory, 'done')
    os.mkfifo(fifo)
    hold = os.open(fifo, os.O_RDWR)
    body = 'read -l release < "' + fifo + '"; printf survived > "' + done + '"'
    script = evaluator(directory, "rpc spawn 7 body '" + body + "'\nset p $response\nprintf '%s\\n' $p[3]")
    p = subprocess.Popen([runtime, '--abi', '2', 'session-run', script], stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=dict(os.environ, MONK_RUNTIME=runtime))
    pid = int(p.stdout.readline())
    assert pid > 0 and pid != p.pid
    assert p.wait(timeout=10) == 0
    os.write(hold, b'release\n')
    out, err = p.communicate(timeout=10)
    os.close(hold)
    assert open(done, 'rb').read() == b'survived', (out, err)
print('background region survives normal outer owner completion passed')

with tempfile.TemporaryDirectory() as directory:
    script = evaluator(directory, '''rpc run 7 pipeline 1 2 external 3 /bin/sh -c 'printf abc; exit 7' external 3 /bin/sh -c 'cat; exit 0'
printf 'pipeline=%s\\n' $response[2]
rpc run 7 pipeline 0 2 external 3 /bin/sh -c 'printf abc; exit 7' external 3 /bin/sh -c 'cat; exit 0'
printf 'pipeline=%s\\n' $response[2]
rpc spawn 7 pipeline 1 2 external 3 /bin/sh -c 'exit 4' external 3 /bin/sh -c 'exit 3'
set job $response[3]
rpc wait 7 source.sh 1 $job
printf 'background=%s\\n' $response[2]''')
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'abcpipeline=7\nabcpipeline=0\nbackground=3\n', b''), p
print('concurrent pipeline output, pipefail and background aggregate wait passed')

with tempfile.TemporaryDirectory() as directory:
    script = b'#' + b'x' * 300000 + b'\nprintf "%s|%s" "$argv[1]" "$argv[2]"\n'
    prepared = subprocess.run([runtime, '--abi', '2', 'session-prepare'], input=script + b'\0', capture_output=True, timeout=10)
    fields = prepared.stdout.split(b'\0')
    assert prepared.returncode == 0 and fields[:2] == [b'ok', b'0'] and len(fields) == 5, prepared
    capsule, token = map(os.fsdecode, fields[2:4])
    p = subprocess.run([runtime, '--abi', '2', 'session-run', '--capsule', capsule, token, '', 'value'], capture_output=True, timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'|value', b''), p
print('large capsule exec launch and empty argv passed')

with tempfile.TemporaryDirectory() as directory:
    script = evaluator(directory, '''rpc spawn 7 external /bin/cat
set job $response[3]
rpc wait 7 source.sh 1 $job
read -l original
printf 'stdin=%s\\n' "$original"
set payload (string repeat -n 300000 x)
rpc run 7 snapshot 'set state (string split0 < $argv[1]); printf "%s:%s" "$state[1]" (string length -- "$state[2]")' 4 '' "$payload"
printf ':snapshot=%s\\n' $response[2]
rpc run 7 pipeline 1 2 builtin 4 source.sh 1 echo "$payload" external 3 /usr/bin/head -c 1
printf ':pipe=%s\\n' $response[2]''')
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], input=b'original\n', capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'stdin=original\n:300000:snapshot=0\nx:pipe=141\n', b''), p
print('background null stdin, large snapshot frames and native builtin SIGPIPE passed')

# Evaluator startup must not turn a caller's missing standard fd into /dev/null.
with tempfile.TemporaryDirectory() as directory:
    probe = os.path.join(directory, 'probe.py')
    with open(probe, 'w') as stream:
        stream.write('import os,sys\ntry: os.fstat(int(sys.argv[1]))\nexcept OSError: sys.exit(0)\nelse: sys.exit(1)\n')
    for missing in (0, 1, 2):
        script = os.path.join(directory, 'closed.fish')
        with open(script, 'w') as stream:
            stream.write('command "' + sys.executable + '" "' + probe + '" ' + str(missing) + '\nexit $status\n')
        def close_standard():
            os.close(missing)
        p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, preexec_fn=close_standard, timeout=10)
        assert p.returncode == 0, ('closed evaluator descriptor repaired', missing, p)
print('closed evaluator standard descriptors stay closed passed')

with tempfile.TemporaryDirectory() as directory:
    script = evaluator(directory, '''rpc wait 7 source.sh 23 ''
printf 'empty=%s\\n' $response[2]
rpc wait 7 source.sh 23 0
printf 'zero=%s\\n' $response[2]
rpc wait 7 source.sh 23 2147483648
printf 'large=%s\\n' $response[2]''')
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout) == (0, b'empty=1\nzero=127\nlarge=1\n'), p
    assert p.stderr == b"source.sh: line 23: wait: `': not a pid or valid job spec\nsource.sh: line 23: wait: pid 0 is not a child of this shell\nsource.sh: line 23: wait: `2147483648': not a pid or valid job spec\n", p
print('empty, zero and out-of-range wait operand diagnostics passed')

# A prepared capsule guardian must not retain arbitrary source pipe writers.
# EOF is the synchronization event; no sleep estimates process readiness.
import select
reader, writer = os.pipe()
os.dup2(writer, 200)
os.close(writer)
prepared = subprocess.run([runtime, '--abi', '2', 'session-prepare'], input=b'true\0', capture_output=True, pass_fds=(200,), timeout=10)
os.close(200)
assert select.select([reader], [], [], 5)[0], 'capsule guardian retained source fd200'
assert os.read(reader, 1) == b''
os.close(reader)
fields = prepared.stdout.split(b'\0')
assert prepared.returncode == 0 and fields[:2] == [b'ok', b'0'], prepared
capsule, token = map(os.fsdecode, fields[2:4])
p = subprocess.run([runtime, '--abi', '2', 'session-run', '--capsule', capsule, token], capture_output=True, timeout=10)
assert p.returncode == 0, p
print('capsule guardian drops high inherited source-pipe descriptors passed')

with tempfile.TemporaryDirectory() as directory:
    script = evaluator(directory, "rpc finish-signal 0 13\nexit 9")
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (-13, b'', b''), p
print('pending SIGPIPE overrides evaluator exit with native signal identity passed')

with tempfile.TemporaryDirectory() as directory:
    script = evaluator(directory, "rpc fd-push 7\nrpc fd-close 7 1\nrpc run 7 builtin source.sh 12 echo value\nset code $response[2]\nrpc fd-pop 7\nprintf '%s' $code")
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'1', b'source.sh: line 12: echo: write error: Bad file descriptor\n'), p
print('closed builtin stdout has Bash write-error status and source diagnostic passed')

# POSIX spawn must search the request environment PATH after applying its cwd;
# posix_spawnp's ambient owner PATH is not the command-prefix environment.
with tempfile.TemporaryDirectory() as directory:
    os.mkdir(os.path.join(directory, 'bin'))
    executable = os.path.join(directory, 'bin', 'owned-command')
    with open(executable, 'w') as stream:
        stream.write('#!/bin/sh\nprintf prefix-path; exit 6\n')
    os.chmod(executable, 0o700)
    script = evaluator(directory, "builtin cd '" + directory + "'\nset -lx PATH bin\nrpc run 7 external owned-command\nprintf ':%s\\n' $response[2]\nrpc spawn 7 external absent-command\nset job $response[3]\nrpc wait 7 source.sh 1 $job\nprintf 'missing=%s\\n' $response[2]")
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'prefix-path:6\nmissing=127\n', b''), p
print('request PATH/cwd executable search and missing-command waitable child passed')

with tempfile.TemporaryDirectory() as directory:
    vanished = os.path.join(directory, 'removed')
    os.mkdir(vanished)
    script = evaluator(directory, "builtin cd '" + vanished + "'\nrpc run 7 external /bin/rmdir '" + vanished + "'\nprintf 'removed=%s\\n' $response[2]\nrpc run 7 builtin source.sh 1 printf after\nprintf ':%s\\n' $response[2]\nrpc run 7 external /usr/bin/true\nprintf 'external=%s\\n' $response[2]")
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'removed=0\nafter:0\nexternal=0\n', b''), p
print('working directory identity survives its pathname being removed passed')

with tempfile.TemporaryDirectory() as directory:
    command = os.path.join(directory, 'program')
    open(command, 'w').write('#!/bin/sh\nexit 0\n')
    os.chmod(command, 0o600)
    script = evaluator(directory, "rpc run 7 external-site source.sh 12 '" + command + "'\nprintf 'denied=%s\\n' $response[2]\nrpc spawn 7 external-site source.sh 13 '" + command + "-missing'\nset job $response[3]\nrpc wait 7 source.sh 13 $job\nprintf 'missing=%s\\n' $response[2]")
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'denied=126\nmissing=127\n', ('source.sh: line 12: '+command+': Permission denied\nsource.sh: line 13: '+command+'-missing: No such file or directory\n').encode()), p
print('owned failed exec retains source diagnostic and actual waitable child status passed')
with tempfile.TemporaryDirectory() as directory:
    os.mkdir(directory+'/directory')
    open(directory+'/plain','w').close()
    os.symlink('loop',directory+'/loop')
    open(directory+'/binary','wb').write(b'\xff\0garbage')
    os.chmod(directory+'/binary',0o700)
    for command, code, reason in [('directory',126,b'Is a directory'),('plain/child',126,b'Not a directory'),('loop',126,b'Too many levels of symbolic links'),('absent',127,b'No such file or directory'),('binary',126,b'cannot execute binary file: Exec format error')]:
        script=evaluator(directory,"builtin cd '"+directory+"'\nrpc run 7 external-site source.sh 3 ./"+command+"\nprintf '%s' $response[2]")
        p=subprocess.run([runtime,'--abi','2','session-run',script],capture_output=True,env=dict(os.environ,MONK_RUNTIME=runtime),timeout=10)
        assert (p.returncode,p.stdout,p.stderr)==(0,str(code).encode(),b'source.sh: line 3: ./'+command.encode()+b': '+reason+b'\n'),p
print('owned exec classifies directory, ENOTDIR, ELOOP, missing and invalid binary in client cwd passed')
