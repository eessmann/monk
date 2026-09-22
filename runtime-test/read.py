#!/usr/bin/env python3
"""Byte-level read differential against Bash, through the private session RPC."""
import os, subprocess, sys, tempfile
runtime = os.path.abspath(sys.argv[1])

def literal(value):
    return "'" + value.replace('\\', '\\\\').replace("'", "\\'") + "'"

def compare(data, raw=True, delimiter='\n', count=-1, ifs=' \t\n', mode='scalar', names=2):
    flags = (['-r'] if raw else []) + ['-d', delimiter] + (['-n', str(count)] if count >= 0 else [])
    if mode == 'array':
        flags += ['-a', 'values']
        script = 'IFS=$1; shift; read "$@"; code=$?; printf "%s\\0" "$code"; if (( ${#values[@]} )); then printf "%s\\0" "${values[@]}"; fi'
    elif mode == 'reply':
        script = 'IFS=$1; shift; read "$@"; code=$?; printf "%s\\0%s\\0" "$code" "$REPLY"'
    else:
        flags += ['a'+str(n) for n in range(names)]
        script = 'IFS=$1; shift; read "$@"; code=$?; printf "%s\\0" "$code" ' + ' '.join('"$a'+str(n)+'"' for n in range(names))
    expected = subprocess.run(['bash', '-c', script, 'oracle.sh', ifs, *flags], input=data, capture_output=True, env=dict(os.environ, LC_ALL='C'))
    request = ['read','7','oracle.sh','1','0',str(int(raw)),delimiter,str(count),ifs,mode,str(names)]
    fish = '''function rpc
 begin
  printf '%s\\0' $argv | command "$MONK_RUNTIME" --abi 2 session-client --reply
 end 3<&0 4>&1 5>&2
 set -g response (string split0 < "$MONK_SESSION_REPLY")
end
rpc ''' + ' '.join(map(literal, request)) + '''
printf '%s\\0' "$response[2]"
if test (count $response) -gt 3
 printf '%s\\0' $response[4..-1]
end
'''
    with tempfile.TemporaryDirectory() as directory:
        path = os.path.join(directory, 'read.fish')
        with open(path, 'w') as stream: stream.write(fish)
        actual = subprocess.run([runtime,'--abi','2','session-run',path], input=data, capture_output=True, env=dict(os.environ, MONK_RUNTIME=runtime), timeout=10)
    assert (actual.returncode, actual.stdout, actual.stderr) == (0, expected.stdout, expected.stderr), (data,raw,delimiter,count,ifs,mode,names,expected,actual)

for data in [b'a b c\n', b' a  b  c  \n', b'a::b:\n', b'\n', b'', b'partial', b'\xff \xfe\n', b'a\0b c\n']:
    for mode in ['scalar','array','reply']:
        compare(data, mode=mode)
for data in [b'a:',b'a::',b':a:',b'::',b'a : b: ',b' a b\\ c  ',b'a\\:b:c']:
    for raw in [True,False]:
        for mode in ['scalar','array','reply']:
            compare(data+b'\n',raw=raw,ifs=' :',mode=mode)
for data in [b'abc\ndef',b'a\\\nbc\n',b'\\ a b\n',b'ab\0cd\n',b'\0tail']:
    for delimiter in ['\n','',':']:
        for count in [0,1,3,-1]:
            compare(data,raw=False,delimiter=delimiter,count=count,mode='reply')
print('read raw bytes, EOF, NUL/delimiter, counts, IFS remainder, escapes and arrays match Bash')
