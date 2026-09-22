#!/usr/bin/env python3
"""FIFO handshakes prove background and inherited INT/QUIT dispositions."""
import os
import select
import signal
import subprocess
import sys
import tempfile

runtime = os.path.abspath(sys.argv[1])
helper = '''function rpc
 begin
 printf '%s\\0' $argv | command "$MONK_RUNTIME" --abi 2 session-client --reply
 end 3<&0 4>&1 5>&2
 set -g response (string split0 < "$MONK_SESSION_REPLY")
end
'''
def quote(value):
    return "'" + value.replace('\\', '\\\\').replace("'", "\\'") + "'"

for kind in ('external', 'pipeline', 'body', 'substitution'):
    for inherited in (False, True):
        with tempfile.TemporaryDirectory() as directory:
            ready, release = directory+'/ready', directory+'/release'
            os.mkfifo(ready); os.mkfifo(release)
            ready_fd = os.open(ready, os.O_RDWR | os.O_NONBLOCK)
            release_fd = os.open(release, os.O_RDWR)
            probe = directory+'/probe.py'
            open(probe, 'w').write('import os,signal\nwith open('+repr(ready)+', "w") as f: f.write(str(os.getpid())+"\\n")\nwith open('+repr(release)+') as f: f.readline()\nprint("survived")\n')
            args = [sys.executable, probe]
            stage = 'external ' + ' '.join(map(quote, args))
            if kind == 'pipeline':
                stage = 'pipeline 0 2 external 1 /usr/bin/true external 2 ' + ' '.join(map(quote, args))
            elif kind == 'body':
                stage = 'body ' + quote(helper+'rpc run 7 external '+ ' '.join(map(quote,args))+'\nexit $response[2]\n')
            # A foreground child of an initially ignored owner inherits ignore;
            # otherwise the source command is explicitly asynchronous.
            mode = 'run' if inherited else 'spawn'
            body = 'rpc '+mode+' 7 '+stage+'\n'
            if kind == 'substitution':
                body = 'rpc substitution 7 input '+stage+'\nset child $response[3]\nset endpoint $response[4]\nprintf "job=%s\\n" $child\nrpc run 7 external /bin/cat $endpoint\n'
            if not inherited and kind != 'substitution':
                body += 'set child $response[3]\nprintf "job=%s\\n" $child\nrpc wait 7 signal.sh 1 $child\n'
            body += 'printf "status=%s\\n" $response[2]\n'
            script = directory+'/evaluate.fish'; open(script,'w').write(helper+body)
            def ignore_signals():
                signal.signal(signal.SIGINT, signal.SIG_IGN)
                signal.signal(signal.SIGQUIT, signal.SIG_IGN)
            p = subprocess.Popen([runtime,'--abi','2','session-run',script], stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=dict(os.environ,MONK_RUNTIME=runtime), preexec_fn=ignore_signals if inherited else None)
            job = None
            if not inherited or kind == 'substitution':
                line=p.stdout.readline(); assert line.startswith(b'job='), (kind,line)
                job=int(line[4:])
            assert select.select([ready_fd],[],[],10)[0], (kind,inherited,'no readiness')
            child=int(os.read(ready_fd,100).strip())
            for target in {child, job if job is not None else p.pid}:
                os.kill(target,signal.SIGINT); os.kill(target,signal.SIGQUIT)
            os.write(release_fd,b'go\n')
            out,err=p.communicate(timeout=10)
            os.close(ready_fd);os.close(release_fd)
            assert (p.returncode,out,err)==(0,b'survived\nstatus=0\n',b''), (kind,inherited,p.returncode,out,err)

# A ordinary foreground user process still starts with default dispositions.
for number in (signal.SIGINT,signal.SIGQUIT):
    with tempfile.TemporaryDirectory() as directory:
        probe=directory+'/probe.py'
        open(probe,'w').write('import os,signal\nassert signal.getsignal('+str(number)+') != signal.SIG_IGN\nsignal.signal('+str(number)+',signal.SIG_DFL)\nos.kill(os.getpid(),'+str(number)+')\n')
        script=directory+'/evaluate.fish'
        open(script,'w').write(helper+'rpc run 7 external '+quote(sys.executable)+' '+quote(probe)+'\nprintf "%s" $response[2]\n')
        p=subprocess.run([runtime,'--abi','2','session-run',script],capture_output=True,env=dict(os.environ,MONK_RUNTIME=runtime),timeout=10)
        assert (p.returncode,p.stdout,p.stderr)==(0,str(128+number).encode(),b''), p
print('FIFO-handshaked external, pipeline, process-substitution and nested-region INT/QUIT ignore; foreground signal statuses passed')
