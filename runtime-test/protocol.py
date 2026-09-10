#!/usr/bin/env python3
"""Independent byte-protocol and Bash semantic comparisons for monk-runtime."""
import os, subprocess, sys
runtime=sys.argv[1]
def call(op, frames):
 return subprocess.run([runtime,'--abi','1',op],input=b''.join(x+b'\0' for x in frames),capture_output=True)
def check(op,frames,out,status=0):
 r=call(op,frames)
 assert (r.returncode,r.stdout)==(status,out),(op,frames,r.returncode,r.stdout,r.stderr,out)
check('integer',[b'add',b'9223372036854775807',b'1'],b'ok\n-9223372036854775808\n-\n')
for op,a,b,expr in [('div','-7','3','-7/3'),('rem','-7','3','-7%3'),('pow','3','41','3**41'),('shr','-7','65','-7>>65'),('add','64#_','1','64#_+1')]:
 expected=subprocess.check_output(['bash','-c','printf "%s" "$(( '+expr+' ))"'])
 check('integer',[op.encode(),a.encode(),b.encode()],b'ok\n'+expected+b'\n-\n')
check('integer',[b'div',b'1',b'0'],b'error\n-\ndivision-by-zero\n')
check('split',[b' :',b' :a:: b: '],b'\0a\0\0b\0')
check('argv',[b'p',b's',b'0',b'',b'z'],b'p\0zs\0')
check('argv',[b'p',b's',b'1'],b'ps\0')
check('echo',[b'-e',br'a\0b\xFF\cignored'],b'a\0b\xff')
check('echo',[b''],b'\n')
check('directory-initial-oldpwd',[b''],b'',1)
check('pattern',[b'match',b'ab\xff',b'1',b'a?',b'0',b'\xff'],b'')
check('pattern',[b'match',b'a',b'0',b'*'],b'',1)
check('glob',[b'0',b'/definitely-absent-monk/*'],b'/definitely-absent-monk/*\0')
for op,raw in [('integer',b'add\0' ),('split',b'a'),('pattern',b'a\0x\0b\0'),('argv',b'a\0b\0bad\0')]:
 r=subprocess.run([runtime,'--abi','1',op],input=raw,capture_output=True)
 assert r.returncode==125,(op,r.returncode,r.stdout)
r=subprocess.run([runtime,'--abi','2','echo'],capture_output=True)
assert r.returncode==125
print('runtime protocol and Bash differential checks passed')
# Bash supplies the oracle, with inputs passed as byte argv and a fixed script.
env=dict(os.environ,LC_ALL='C')
for ifs in [b'',b' ',b':',b' :\t\n',b'\xff']:
 for value in [b'',b' ',b':',b'::',b' a : b: ',b'\xffa\xff\xffz',b'\na\tb  c\n']:
  oracle=subprocess.check_output(['bash','-c','IFS=$1; value=$2; set -- $value; if (( $# )); then printf "%s\\0" "$@"; fi','bash',ifs,value],env=env)
  check('split',[ifs,value],oracle)
for args in [[b''],[b'-n',b'x'],[b'-e',br'\u1234 \U0001F600'],[b'-e',br'\x4a\0777\0\cxxx'],[b'-e',br'\\c'],[b'-eE',br'\n'],[b'-e',br'\uD800 \U80000000'],[b'\xff']]:
 oracle=subprocess.check_output(['bash','-c','echo "$@"','bash',*args],env=env)
 check('echo',args,oracle)
import tempfile
with tempfile.TemporaryDirectory() as directory:
 os.makedirs(os.path.join(directory,'dir','inner'))
 for name in ['a','b','star*','.hidden','\u00e9']:
  open(os.path.join(directory,name),'w').close()
 pattern=os.fsencode(directory)+b'/*'
 oracle=subprocess.check_output(['bash','-c','printf "%s\\0" "$1"/*','bash',directory],env=env)
 check('glob',[b'1',pattern],oracle)
 for spelling in [directory+'//*', '/'+directory+'/*', directory+'//*/', directory+'/*//', directory+'/*///', directory+'/*//inner//', directory+'///none*']:
  oracle=subprocess.check_output(['bash','-c','printf "%s\\0" '+spelling],env=env)
  check('glob',[b'1',os.fsencode(spelling)],oracle)
print('IFS, echo and pathname Bash byte differentials passed')
check('glob',[],b'\0')
check('glob',[b'0',b''],b'\0')
for op,subject,pattern,expr in [('trim-prefix-short',b'abcabc',b'a*','${x#a*}'),('trim-prefix-long',b'abcabc',b'a*','${x##a*}'),('trim-suffix-short',b'abcabc',b'*c','${x%*c}'),('trim-suffix-long',b'abcabc',b'*c','${x%%*c}')]:
 oracle=subprocess.check_output(['bash','-c','x=$1; printf "%s" "'+expr+'"','bash',subject],env=env)
 check('pattern',[op.encode(),subject,pattern],oracle+b'\0')
check('pattern',[b'replace-all',b'ababa',b'aba',b'&'],b'&ba\0')
check('pattern',[b'replace-first',b'abc',b'',b'x'],b'abc\0')
for mask in range(8):
 def close():
  for fd in range(3):
   if not (mask & (1<<fd)): os.close(fd)
 r=subprocess.run([runtime,'--abi','1','descriptor-state'],capture_output=True,preexec_fn=close)
 assert r.returncode==mask,('descriptor-state',mask,r.returncode)
