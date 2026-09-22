#!/usr/bin/env python3
"""Owned descriptor scopes, shared offsets and private capture handoff."""
import os, subprocess, sys, tempfile
runtime = os.path.abspath(sys.argv[1])
header = 'function rpc\n begin\n printf "%s\\0" $argv | command "$MONK_RUNTIME" --abi 2 session-client --reply\n end 3<&0 4>&1 5>&2\n set -g response (string split0 < "$MONK_SESSION_REPLY")\nend\n'
with tempfile.TemporaryDirectory() as directory:
    script = os.path.join(directory, 'scope.fish')
    body = """rpc fd-push 7
rpc fd-data 7 3 'first
second
third
'
rpc fd-dup 7 source.sh 1 4 3
rpc read 7 source.sh 1 3 1 '' 5 '' scalar 1
printf 'first=%s\n' $response[4]
rpc read 7 source.sh 1 4 1 '\n' -1 '' scalar 1
printf 'tail=%s\n' $response[4]
rpc read 7 source.sh 1 3 1 '\n' -1 '' scalar 1
printf 'second=%s\n' $response[4]
rpc fd-push 7
rpc fd-close 7 3
rpc read 7 source.sh 5 3 1 '\n' -1 '' scalar 1
printf 'closed=%s,%s\n' $response[2] $response[3]
rpc fd-pop 7
rpc read 7 source.sh 1 3 1 '\n' -1 '' scalar 1
printf 'third=%s\n' $response[4]
rpc fd-open 7 source.sh 9 2 write ERRORS
rpc fd-open 7 source.sh 9 0 read MISSING
printf 'failure=%s\n' $response[2]
rpc fd-pop 7
""".replace('ERRORS', "'" + directory + "/errors'").replace('MISSING', "'" + directory + "/missing'")
    # A literal newline is the Bash read default delimiter.
    body = body.replace("'\\n'", "'\n'")
    open(script, 'w').write(header + body)
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'first=first\ntail=\nsecond=second\nclosed=1,0\nthird=third\nfailure=1\n', b'source.sh: line 5: read: 3: invalid file descriptor: Bad file descriptor\n'), p
    assert open(directory + '/errors', 'rb').read() == ('source.sh: line 9: ' + directory + '/missing: No such file or directory\n').encode()
print('descriptor aliases share offsets, scopes restore, and failed opens use virtual stderr passed')

with tempfile.TemporaryDirectory() as directory:
    nested = header + "rpc read 7 source.sh 1 3 1 '\n' -1 '' scalar 1\nprintf '%s\\n\\n' $response[4]\n"
    nested_file = os.path.join(directory, 'nested.fish')
    open(nested_file, 'w').write(nested)
    packet = os.path.join(directory, 'packet')
    body = """rpc fd-push 7
rpc fd-data 7 3 'captured
remaining
'
set body (string collect < NESTED)
begin
 printf '%s\\0' warning 7 "$body" 4 | command "$MONK_RUNTIME" --abi 2 child-capture-session > PACKET
end 3<&0
set packet (string split0 < PACKET)
printf 'capture=%s,%s\n' $packet[2] $packet[3]
rpc read 7 source.sh 1 3 1 '\n' -1 '' scalar 1
printf 'after=%s\n' $response[4]
rpc fd-pop 7
""".replace('NESTED', "'" + nested_file + "'").replace('PACKET', "'" + packet + "'")
    script = os.path.join(directory, 'capture.fish')
    open(script, 'w').write(header + body)
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'capture=0,captured\nafter=remaining\n', b''), p
print('nested native capture inherits scoped fd3 and shares its parent offset passed')

with tempfile.TemporaryDirectory() as directory:
    output = os.path.join(directory, 'output')
    errors = os.path.join(directory, 'errors')
    script = os.path.join(directory, 'directory-output.fish')
    body = """rpc fd-push 7
rpc fd-open 7 source.sh 1 1 write OUTPUT
rpc fd-open 7 source.sh 1 2 write ERRORS
rpc run 7 directory-output source.sh 12 pwd 1 'current-directory'
rpc run 7 directory-output source.sh 13 cd 2 'directory-diagnostic'
rpc fd-close 7 1
rpc run 7 directory-output source.sh 14 pwd 1 bytes
set closedout $response[2]
rpc fd-close 7 2
rpc run 7 directory-output source.sh 15 cd 2 ignored
set closederr $response[2]
rpc fd-pop 7
printf 'closed=%s,%s\\n' $closedout $closederr
""".replace('OUTPUT', "'" + output + "'").replace('ERRORS', "'" + errors + "'")
    open(script, 'w').write(header + body)
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'closed=1,0\n', b'ignored'), p
    assert open(output, 'rb').read() == b'current-directory'
    assert open(errors, 'rb').read() == b'directory-diagnosticsource.sh: line 14: pwd: write error: Bad file descriptor\n'
print('directory bytes use virtual output/error streams and closed descriptors retain builtin diagnostics passed')

with tempfile.TemporaryDirectory() as directory:
    script = os.path.join(directory, 'read-errors.fish')
    body = """rpc fd-push 7
rpc fd-open 7 source.sh 1 3 write OUT
rpc read 7 source.sh 9 3 1 '\n' -1 '' scalar 1
printf 'write-only=%s,%s\n' $response[2] $response[3]
rpc fd-open 7 source.sh 1 3 read DIRECTORY
rpc read 7 source.sh 10 3 1 '\n' -1 '' scalar 1
printf 'directory=%s,%s\n' $response[2] $response[3]
rpc fd-pop 7
""".replace('OUT', "'" + directory + "/out'").replace('DIRECTORY', "'" + directory + "'")
    open(script, 'w').write(header + body)
    p = subprocess.run([runtime, '--abi', '2', 'session-run', script], capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (p.returncode, p.stdout, p.stderr) == (0, b'write-only=1,0\ndirectory=1,0\n', b'source.sh: line 9: read: 3: read error: Bad file descriptor\nsource.sh: line 10: read: 3: read error: Is a directory\n'), p
print('read IO errors preserve assignment state and emit source-aware errno diagnostics passed')

with tempfile.TemporaryDirectory() as directory:
    script = directory + '/diagnostic-fallback.fish'
    nested = header.replace(" 5>&2", "") + "rpc read 3 source.sh 18 9 1 '' -1 '' scalar 1\n"
    open(directory+'/nested.fish','w').write(nested)
    probe = directory+'/closed.py'
    open(probe,'w').write('import os,sys\ntry: os.fstat(2)\nexcept OSError: sys.exit(0)\nelse: sys.exit(99)\n')
    body = """rpc fd-push 7
rpc fd-open 7 source.sh 1 2 write ERRORS
rpc fd-push 7
rpc fd-close 7 2
rpc read 7 source.sh 11 9 1 '' -1 '' scalar 1
rpc wait 7 source.sh 12 0
rpc fd-open 7 source.sh 13 0 read MISSING
rpc fd-push 7
rpc fd-close 7 1
rpc run 7 builtin source.sh 14 printf value
rpc run 7 builtin source.sh 15 echo value
rpc run 7 external-site source.sh 16 MISSING
rpc spawn 7 builtin source.sh 17 printf value
rpc wait 7 source.sh 17 $response[3]
rpc run 7 pipeline 0 1 builtin 4 source.sh 19 printf value
rpc fd-pop 7
rpc run 7 external PYTHON PROBE
printf 'external=%s\\n' $response[2]
set body (string collect < NESTED)
rpc run 7 body "$body"
printf 'region=%s\\n' $response[2]
rpc fd-pop 7
rpc fd-pop 7
"""
    for key,value in [('ERRORS',directory+'/errors'),('MISSING',directory+'/missing'),('PYTHON',sys.executable),('PROBE',probe),('NESTED',directory+'/nested.fish')]:
        body=body.replace(key,"'"+value+"'")
    open(script,'w').write(header+body)
    p=subprocess.run([runtime,'--abi','2','session-run',script],capture_output=True,env=dict(os.environ,MONK_RUNTIME=runtime),timeout=10)
    expected=("source.sh: line 11: read: 9: invalid file descriptor: Bad file descriptor\n"
              "source.sh: line 12: wait: pid 0 is not a child of this shell\n"
              "source.sh: line 13: "+directory+"/missing: No such file or directory\n"
              "source.sh: line 14: printf: write error: Bad file descriptor\n"
              "source.sh: line 15: echo: write error: Bad file descriptor\n").encode()
    assert (p.returncode,p.stdout,p.stderr)==(0,b'external=0\nregion=0\n',expected),p
    assert open(directory+'/errors','rb').read()==b''
print('closed stderr retains same-owner diagnostics, resets at child entry, and stays closed for external fd2 passed')
