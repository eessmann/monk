#!/usr/bin/env python3
"""Native entry preserves pre-Fish descriptors and owns launch cleanup."""
import os
import select
import signal
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
runtime = os.path.abspath(sys.argv[1])
env = dict(os.environ, LC_ALL='C', LANG='C', MONK_TEST_RUNTIME=runtime)
for key in ('MONK_LAUNCH_ORIGINAL','MONK_LAUNCH_WRAPPER'):
    env.pop(key,None)
def quote(value):
    return "'"+str(value).replace('\\','\\\\').replace("'","\\'")+"'"
with tempfile.TemporaryDirectory() as temporary:
    root=Path(temporary).resolve()
    workspace=root/'workspace';workspace.mkdir()
    script=root/'entry.fish';result=root/'result'
    for mask in range(8):
        script.write_text('command "$MONK_TEST_RUNTIME" --abi 2 descriptor-state\nset mask $status\nset --global argv replacement $argv\nprintf "%s\\0" $mask $argv > '+quote(result)+'\n')
        def close():
            for fd in range(3):
                if not mask & (1<<fd): os.close(fd)
        p=subprocess.run([runtime,'--abi','2','launch',str(script),'','two words'],capture_output=True,env=dict(env,TMPDIR=str(workspace)),preexec_fn=close,timeout=10)
        assert (p.returncode,p.stdout,p.stderr)==(0,b'',b''),(mask,p)
        assert result.read_bytes()==str(mask).encode()+b'\0replacement\0\0two words\0',(mask,result.read_bytes())
        assert list(workspace.iterdir())==[]
    # The generated writer's runtime sees the real originally closed stdout.
    script.write_text('printf "%s\\0" source.sh 7 printf x | command "$MONK_TEST_RUNTIME" --abi 2 write-builtin\nexit $status\n')
    p=subprocess.run([runtime,'--abi','2','launch',str(script)],capture_output=True,env=dict(env,TMPDIR=str(workspace)),preexec_fn=lambda:os.close(1),timeout=10)
    assert (p.returncode,p.stdout,p.stderr)==(1,b'',b'source.sh: line 7: printf: write error: Bad file descriptor\n'),p
    # The launcher itself reports the child's real signal, after file cleanup.
    script.write_text('exec "$MONK_TEST_RUNTIME" --abi 2 raise-signal 13\n')
    p=subprocess.run([runtime,'--abi','2','launch',str(script)],capture_output=True,env=dict(env,TMPDIR=str(workspace)),preexec_fn=lambda:os.close(0),timeout=10)
    assert (p.returncode,p.stdout,p.stderr)==(-signal.SIGPIPE,b'',b''),p
    assert list(workspace.iterdir())==[]
    for key in ('MONK_LAUNCH_ORIGINAL','MONK_LAUNCH_WRAPPER'):
        p=subprocess.run([runtime,'--abi','2','launch',str(script)],capture_output=True,env=dict(env,**{key:''}),timeout=10)
        assert p.returncode==125,p
    # Wrapper metadata identifies the original artifact but stays out of
    # explicit user external environments.
    script.write_text('printf "%s\\0" "$MONK_LAUNCH_ORIGINAL" "$MONK_LAUNCH_WRAPPER" (status filename) > '+quote(result)+'\ncommand "$MONK_TEST_RUNTIME" --abi 2 exec-site source.sh 1 '+quote(sys.executable)+' -c '+quote('import os; print(os.getenv("MONK_LAUNCH_ORIGINAL")); print(os.getenv("MONK_LAUNCH_WRAPPER"))')+'\n')
    p=subprocess.run([runtime,'--abi','2','launch',str(script)],capture_output=True,env=dict(env,TMPDIR=str(workspace)),preexec_fn=lambda:os.close(0),timeout=10)
    assert (p.returncode,p.stdout,p.stderr)==(0,b'None\nNone\n',b''),p
    original,wrapper,actual,_=result.read_bytes().split(b'\0')
    assert original==os.fsencode(script) and wrapper==actual,(original,wrapper,actual)
    assert not os.path.exists(wrapper)
    # Inspect private ownership while blocked behind a FIFO, then terminate.
    fifo=root/'release';os.mkfifo(fifo);release=os.open(fifo,os.O_RDWR)
    script.write_text('printf ready >&2\nread -l item < '+quote(fifo)+'\n')
    p=subprocess.Popen([runtime,'--abi','2','launch',str(script)],stdout=subprocess.PIPE,stderr=subprocess.PIPE,env=dict(env,TMPDIR=str(workspace)),preexec_fn=lambda:os.close(0))
    assert p.stderr.read(5)==b'ready'
    owned=list(workspace.iterdir());assert len(owned)==1,owned
    assert owned[0].stat().st_mode & 0o777 == 0o700
    assert all(path.stat().st_mode & 0o777 == 0o600 for path in owned[0].iterdir())
    p.send_signal(signal.SIGTERM);out,error=p.communicate(timeout=10)
    assert (p.returncode,out,error)==(-signal.SIGTERM,b'',b''),(p.returncode,out,error)
    assert list(workspace.iterdir())==[]
    # Ordinary completion leaves a background child alive; wait-before-release
    # gives a deterministic liveness witness without readiness sleeps.
    done=root/'done'
    script.write_text('command python3 -c '+quote('import os; f=open('+repr(str(fifo))+'); f.readline(); open('+repr(str(done))+',"w").write("survived")')+' &\nexit 0\n')
    p=subprocess.Popen([runtime,'--abi','2','launch',str(script)],stdout=subprocess.PIPE,stderr=subprocess.PIPE,env=dict(env,TMPDIR=str(workspace)),preexec_fn=lambda:os.close(0))
    assert p.wait(timeout=10)==0
    os.write(release,b'go\n');out,error=p.communicate(timeout=10)
    assert done.read_bytes()==b'survived' and out==error==b''
    os.close(release)
    assert list(workspace.iterdir())==[]
print('native launcher preserves all stdio masks, global argv, signal identity, cleanup and surviving background jobs')

# Optional compiler path proves captured providers and relative source modules.
if len(sys.argv)>2:
    monk=os.path.abspath(sys.argv[2])
    for source_spelling in ('absolute','relative'):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary).resolve()/'artifact directory\n'
            root.mkdir()
            source=root/'source.bash';target=root/'entry.fish'
            module=root/'module.bash'
            module.write_text('f() { printf "<%s>" "$1"; }; f "$1"\n')
            source.write_text('. '+(quote(module) if source_spelling=='absolute' else './module.bash')+' "$1"; (printf "%s" "$2")\n')
            provider=root/'provider';shutil.copy2(runtime,provider)
            translated=subprocess.run([monk,str(source),'--strict','--recursive','--managed','--runtime',str(provider),'-o',str(target)],capture_output=True,env=env,cwd=root,timeout=30)
            assert translated.returncode==0,translated
            captured=list(root.rglob('bin/monk-runtime'))
            assert len(captured)==1,captured
            captured_runtime=captured[0]
            generation=captured_runtime.parent.parent
            generation_entry=generation/'entry.fish'
            assert generation_entry.is_file()
            modules=[path for path in generation.glob('*.fish') if path!=generation_entry]
            assert modules, list(generation.iterdir())
            provider.unlink()
            execution_env=dict(env)
            execution_env.pop('MONK_TEST_RUNTIME',None)
            execution_env['PATH']=os.pathsep.join(path for path in env['PATH'].split(os.pathsep) if not (Path(path)/'monk-runtime').exists())
            assert shutil.which('monk-runtime',path=execution_env['PATH']) is None
            for mask in range(8):
                def close():
                    for fd in range(3):
                        if not mask&(1<<fd):os.close(fd)
                reference=subprocess.run(['bash',str(source),'first','second'],capture_output=True,env=execution_env,cwd=root,preexec_fn=close,timeout=15)
                expected=(reference.returncode,reference.stdout,reference.stderr)
                for artifact in (target,generation_entry):
                    p=subprocess.run([str(captured_runtime),'--abi','2','launch',str(artifact),'first','second'],capture_output=True,env=execution_env,cwd=root,preexec_fn=close,timeout=15)
                    actual=(p.returncode,p.stdout,p.stderr)
                    assert expected==actual,(mask,artifact,expected,actual)
    print('managed loader and generation entry use captured runtime and relative source modules after provider removal under all stdio masks')
