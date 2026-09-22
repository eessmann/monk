#!/usr/bin/env python3
"""Freeze and independently compare the historic95 and separate effect cohorts.

Runs trusted repository fixtures only. Every run has an immutable output directory.
Raw bytes are retained; rejected translation, timeout and mismatch are distinct.
The historical denominator is never enlarged by the additional cohorts.
"""
import argparse
import base64
from collections import Counter
from datetime import datetime, timezone
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import shutil
import stat
import subprocess


def load_module(name, filename):
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(filename))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


COMPARE = load_module('byte_comparison', 'compare-bakeoff-bash.py')
PROFILE = load_module('reference_profile', 'reference-runtime-profile.py')


def sha(data):
    return hashlib.sha256(data).hexdigest()


def write_json(path, value):
    path.write_text(json.dumps(value, indent=2) + '\n')


def snapshot(directory):
    """Record names, types, modes, symlink targets and complete regular bytes."""
    rows = {}
    for path in sorted(directory.rglob('*')):
        info = path.lstat()
        row = {'mode': stat.S_IMODE(info.st_mode)}
        if stat.S_ISREG(info.st_mode):
            data = path.read_bytes()
            row.update(type='file', sha256=sha(data), bytes_base64=base64.b64encode(data).decode())
        elif stat.S_ISLNK(info.st_mode):
            row.update(type='symlink', target=os.readlink(path))
        elif stat.S_ISDIR(info.st_mode):
            row['type'] = 'directory'
        elif stat.S_ISFIFO(info.st_mode):
            row['type'] = 'fifo'
        else:
            row['type'] = 'special'
        rows[str(path.relative_to(directory))] = row
    return rows


def effect_comparison(reference, observed, reference_fs=None, observed_fs=None):
    compared = COMPARE.compare(reference, observed)
    if compared['status'] == 'unavailable':
        return compared
    if reference_fs is not None and reference_fs != observed_fs:
        compared['differences'].append('filesystem')
    compared['status'] = 'mismatch' if compared['differences'] else 'match'
    return compared


def strengthened_cases():
    """Non-vacuous controls kept outside the immutable historic95 denominator."""
    return [
        ('read-array-live-ifs', "IFS=' :'; read -r -a values; observed=$?; printf 'status:%s\\n' \"$observed\"; printf 'field:<%s>\\n' \"${values[@]}\"\n", b' alpha:beta:: gamma \n'),
        ('read-eof-without-newline', "IFS= read -r value; observed=$?; printf 'status:%s\\nvalue:<%s>\\n' \"$observed\" \"$value\"\n", b'partial value'),
        ('read-nul-invalid-bytes', "IFS= read -r value; observed=$?; printf 'status:%s\\nvalue:<%s>\\n' \"$observed\" \"$value\"\n", b'a\0\xffb\n'),
        ('dense-array-append-observed', "values=('first item' '' third); values+=('last item' '*'); printf '<%s>\\n' \"${values[@]}\"\n", b''),
        ('visible-time-output', "TIMEFORMAT=measured; time { printf timed; }\n", b''),
    ]


def freeze(repo, output):
    output.mkdir(parents=True)
    inputs = output / 'inputs'
    inputs.mkdir()
    for folder in ('test/fixtures', 'benchmark/fixtures'):
        shutil.copytree(repo / folder, inputs / folder)
    historical_bytes = (repo / 'docs/evidence/bakeoff-2026-09-09.json').read_bytes()
    previous = json.loads(historical_bytes)
    assert len(previous['fixtures']) == 95
    rows = []
    for fixture in previous['fixtures']:
        path = inputs / fixture['fixture']
        if sha(path.read_bytes()) != fixture['input_sha256']:
            raise ValueError('Frozen95 input changed: ' + fixture['fixture'])
        stdin = path.with_suffix('.stdin')
        rows.append({'fixture': fixture['fixture'], 'cohort': 'historic95',
                     'input_sha256': fixture['input_sha256'], 'metadata': fixture['metadata'],
                     'stdin_base64': base64.b64encode(stdin.read_bytes() if stdin.exists() else b'').decode()})
    extra = inputs / 'effects'
    extra.mkdir()
    cases = [
        ('filesystem', 'bytes-append-mode', "printf '\\000\\377A' > output; printf 'B\\n' >> output; chmod 640 output; cat output\n"),
        ('caller', 'function-caller-state', 'f() { local value=inside; echo "$value"; }; echo sourced\n'),
        ('process', 'background-handshake', 'python3 "$1" producer &\npid=$!\ncat ready.fifo\nprintf "go\\n" > go.fifo\nwait "$pid"\ncat events\n'),
        ('process', 'large-process-substitution', 'cat <(python3 "$1" stream)\n'),
    ]
    for cohort, name, source in cases:
        path = extra / (name + '.bash')
        path.write_text(source)
        rows.append({'fixture': str(path.relative_to(inputs)), 'cohort': cohort,
                     'input_sha256': sha(path.read_bytes()), 'stdin_base64': '',
                     'metadata': {'fixtureMetaArgs': [], 'fixtureMetaRecursive': False}})
    (extra / 'caller.json').write_text('{"version":1,"exportedFunctions":["f"],"ambientEffects":"none"}\n')
    (extra / 'worker.py').write_text('''import sys
if sys.argv[1] == 'stream':
    sys.stdout.buffer.write(bytes(range(256)) * 4096)
else:
    with open('events', 'wb') as f: f.write(b'ready\\n')
    with open('ready.fifo', 'wb') as f: f.write(b'ready\\n')
    with open('go.fifo', 'rb') as f: assert f.readline() == b'go\\n'
    with open('events', 'ab') as f: f.write(b'released\\n')
''')
    strengthened = inputs / 'strengthened'
    strengthened.mkdir()
    for name, source, stdin in strengthened_cases():
        path = strengthened / (name + '.bash')
        path.write_text(source)
        path.with_suffix('.stdin').write_bytes(stdin)
        rows.append({'fixture': str(path.relative_to(inputs)), 'cohort': 'strengthened',
                     'input_sha256': sha(path.read_bytes()),
                     'stdin_base64': base64.b64encode(stdin).decode(),
                     'metadata': {'fixtureMetaArgs': [], 'fixtureMetaRecursive': False,
                                  'fixtureMetaHasStdin': bool(stdin)}})
    manifest = {'schema': 1, 'directory_contract': {'lane':args.directory_contract, 'applies_to':['current','candidate']},
              'candidate_entry': {'standalone':'runtime --abi 2 launch SCRIPT ARGS', 'sourceable':'fish --no-config caller observer'},
              'historic_denominator': 95, 'historical_report_sha256': sha(historical_bytes),
                'fixtures': rows, 'input_tree': snapshot(inputs),
                'common16': [r['fixture'] for r in previous['fixtures'] if all(r['tools'][t]['status'] == 'match' for t in ('monk','babelfish'))]}
    write_json(output / 'manifest.json', manifest)
    print('Frozen 95 historic inputs; 4 separate effect cases; 5 separate strengthened cases')


def provenance(path):
    resolved = Path(path).resolve(strict=True)
    return {'path': str(resolved), 'sha256': sha(resolved.read_bytes())}


def translate(tool, binary, runtime, fixture, inputs, generated, env, timeout, directory_contract="default"):
    source = inputs / fixture['fixture']
    if tool == 'babelfish':
        command = [str(binary)]
        data = source.read_bytes()
    else:
        command = [str(binary), str(source)]
        data = b''
        if fixture['metadata'].get('fixtureMetaRecursive'):
            command += ['--recursive', '--sources', 'inline']
        if tool != 'original':
            command += ['--runtime', str(runtime)]
            if directory_contract == 'stable':
                command += ['--directory-contract', 'stable']
            if fixture['cohort'] == 'caller':
                command += ['--entry', 'sourceable', '--caller-contract', str(inputs / 'effects/caller.json')]
    result = COMPARE.run(command, data, inputs, env, timeout)
    generated.write_bytes(result['stdout'])
    return result, command


def execute(shell, fixture, source, inputs, cwd, env, timeout, fish=False, launcher=None):
    # Use the same absolute working directory for both shells and reset it before
    # every execution, so cwd-dependent behavior cannot fabricate a mismatch.
    if cwd.exists():
        shutil.rmtree(cwd)
    shutil.copytree(inputs, cwd)
    argv = fixture['metadata'].get('fixtureMetaArgs', [])
    if fixture['cohort'] == 'process':
        argv = [str(inputs / 'effects/worker.py')]
        if 'background-handshake' in fixture['fixture']:
            os.mkfifo(cwd / 'ready.fifo')
            os.mkfifo(cwd / 'go.fifo')
    if fixture['cohort'] == 'caller':
        if fish:
            body = 'set --global value caller; set --global sentinel unchanged; source "$argv[1]"; set result $status; f; printf "caller:%s:%s:%s\\n" "$value" "$sentinel" "$result"'
        else:
            body = 'value=caller; sentinel=unchanged; source "$1"; result=$?; f; printf "caller:%s:%s:%s\\n" "$value" "$sentinel" "$result"'
        command = [str(shell), *( ['--no-config'] if fish else ['--noprofile','--norc']), '-c', body]
        if not fish:
            command += ['caller-observer']
        command += [str(source)]
    elif fish and launcher is not None:
        command = [str(launcher), '--abi', '2', 'launch', str(source), *argv]
    else:
        command = [str(shell), *( ['--no-config'] if fish else ['--noprofile','--norc']), str(source), *argv]
    observed = COMPARE.run(command, base64.b64decode(fixture['stdin_base64']), cwd, env, timeout)
    filesystem = snapshot(cwd) if fixture['cohort'] in ('filesystem','process') and observed['status'] == 'completed' else None
    return observed, command, filesystem


def measure(args):
    build_receipt_sha256 = None
    if args.stage == 'final':
        if not args.build_receipt:
            raise ValueError('Final comparison requires a successful stable build receipt')
        receipt_bytes = args.build_receipt.read_bytes()
        receipt = json.loads(receipt_bytes)
        if receipt.get('schema') != 2 or not receipt.get('successful_stable_command') or receipt['production_after']['sha256'] != args.source_fingerprint:
            raise ValueError('Build receipt is unsuccessful, unstable, or has a different source fingerprint')
        built = {str(Path(r['path']).resolve()):r['sha256'] for r in receipt['binaries']}
        for path in (args.candidate,args.runtime):
            if built.get(str(path.resolve())) != sha(path.read_bytes()):
                raise ValueError('Build receipt does not identify supplied candidate/runtime')
        build_receipt_sha256 = sha(receipt_bytes)
    args.bash = args.bash.resolve(strict=True)
    args.fish = args.fish.resolve(strict=True)
    frozen = args.frozen.resolve()
    inputs = frozen / 'inputs'
    manifest = json.loads((frozen / 'manifest.json').read_bytes())
    if snapshot(inputs) != manifest['input_tree']:
        raise ValueError('Frozen input tree changed')
    output = args.output.resolve()
    output.mkdir(parents=True)
    cwd = output / 'execution-cwd'
    env = dict(os.environ, LC_ALL='C', LANG='C', XDG_CONFIG_HOME=str(output / 'config'))
    env['PATH'] = str(args.bash.parent) + os.pathsep + str(args.fish.parent) + os.pathsep + env.get('PATH','')
    for key in ('BASH_ENV','ENV','SHELLOPTS','BASHOPTS','CDPATH'):
        env.pop(key, None)
    providers = output / 'providers'
    providers.mkdir()
    supplied = {name: Path(getattr(args, name)).resolve(strict=True) for name in ('original','current','candidate','babelfish')}
    supplied.update(current_runtime=args.current_runtime.resolve(strict=True), runtime=args.runtime.resolve(strict=True))
    for name,path in supplied.items():
        shutil.copy2(path, providers / name)
    if args.stage == 'final':
        for name in ('candidate','runtime'):
            if sha((providers / name).read_bytes()) != built[str(supplied[name])]:
                raise ValueError('Binary changed while capturing final providers: ' + name)
    tools = {name: providers / name for name in ('original','current','candidate','babelfish')}
    args.current_runtime = providers / 'current_runtime'
    args.runtime = providers / 'runtime'
    current_probe = COMPARE.run([str(args.current_runtime),'--describe'],b'',inputs,env,args.timeout)
    report = {'schema': 1, 'stage': args.stage, 'timestamp': datetime.now(timezone.utc).isoformat(),
              'platform': platform.platform(), 'source_fingerprint': args.source_fingerprint,
              'source_fingerprint_kind': ('verified final build inputs' if args.stage == 'final' else 'working-tree inputs at capture; binary can precede in-progress source edits'),
              'build_receipt_sha256': build_receipt_sha256,
              'verification_input_fingerprint': receipt['after']['sha256'] if args.stage == 'final' else None,
              'directory_contract': {'lane':args.directory_contract, 'applies_to':['current','candidate']},
              'candidate_entry': {'standalone':'runtime --abi 2 launch SCRIPT ARGS', 'sourceable':'fish --no-config caller observer'},
              'historic_denominator': 95, 'frozen_manifest_sha256': sha((frozen / 'manifest.json').read_bytes()),
              'reference': PROFILE.profile(str(args.bash), str(args.fish)),
              'tools': {name: dict(provenance(path),supplied_path=str(supplied[name])) for name,path in tools.items()},
              'current_runtime_probe': COMPARE.record(current_probe, output, 'current-runtime-describe', [str(args.current_runtime),'--describe']),
              'runtimes': {name: provenance(path) for name,path in [('current',args.current_runtime),('candidate',args.runtime)]},
              'source_commits': {'original':'2bc0e72bcfaa6d90615946667a573093aed2262e','current':'c2bd371b10f041f921acda7aeab85363df393661'},
              'performance': {'status':'unverified', 'reason':'Historical native baseline does not support Darwin; Linux execution deferred by user. No historical measurements reused as fresh results.'},
              'scope': ['raw stdout','raw stderr','exit status','separate filesystem bytes and modes','separate caller observations','separate handshake process events'],
              'fixtures': [], 'totals': {}}
    for index,fixture in enumerate(manifest['fixtures']):
        directory = output / f'{index:03d}'
        directory.mkdir()
        row = {'fixture': fixture['fixture'], 'cohort': fixture['cohort'], 'input_sha256': fixture['input_sha256'], 'tools':{}}
        reference, command, reference_fs = execute(args.bash, fixture, inputs / fixture['fixture'], inputs, cwd, env, args.timeout)
        row['bash'] = COMPARE.record(reference, directory, 'bash', command)
        if reference_fs is not None:
            write_json(directory / 'bash.filesystem.json', reference_fs)
        for tool,binary in tools.items():
            generated = directory / (tool+'.fish')
            runtime = args.current_runtime if tool == 'current' else args.runtime
            translated, command = translate(tool,binary,runtime,fixture,inputs,generated,env,args.timeout,directory_contract=args.directory_contract)
            observation = {'translation': COMPARE.record(translated,directory,tool+'.translation',command), 'generated_sha256':sha(generated.read_bytes())}
            if translated['status'] != 'completed':
                observation['status'] = 'translation-timeout'
            elif translated['exit'] != 0:
                observation['status'] = 'translation-rejected'
            else:
                result, command, fs = execute(args.fish,fixture,generated,inputs,cwd,env,args.timeout,fish=True,launcher=args.runtime if tool == 'candidate' else None)
                observation.update(effect_comparison(reference,result,reference_fs,fs))
                if (tool == 'current' and b'requires 64-bit Linux' in current_probe['stderr']
                        and b'missing or incompatible native runtime' in result['stderr']):
                    observation['status'] = 'unsupported-native-platform'
                    observation['reason'] = 'The unchanged historical runtime requires 64-bit Linux; native Darwin execution is unavailable.'
                observation['execution'] = COMPARE.record(result,directory,tool,command)
                if fs is not None:
                    write_json(directory / (tool+'.filesystem.json'),fs)
            row['tools'][tool] = observation
        report['fixtures'].append(row)
        write_json(output / 'partial.json',report)
        print(index,fixture['fixture'],{k:v['status'] for k,v in row['tools'].items()},flush=True)
    for cohort in ('historic95','filesystem','caller','process','strengthened'):
        report['totals'][cohort] = {tool:dict(Counter(r['tools'][tool]['status'] for r in report['fixtures'] if r['cohort']==cohort)) for tool in tools}
    write_json(output / 'report.json',report)
    shutil.rmtree(cwd)
    print(json.dumps(report['totals'],indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest='action',required=True)
    f = sub.add_parser('freeze')
    f.add_argument('--repo',type=Path,default=Path.cwd())
    f.add_argument('--output',type=Path,required=True)
    m = sub.add_parser('measure')
    for option in ('frozen','output','original','current','candidate','babelfish','current-runtime','runtime','bash','fish'):
        m.add_argument('--'+option,type=Path,required=True)
    m.add_argument('--source-fingerprint',required=True)
    m.add_argument('--build-receipt',type=Path)
    m.add_argument('--stage',choices=['preliminary','final'],required=True)
    m.add_argument('--timeout',type=float,default=10)
    m.add_argument('--directory-contract',choices=['default','stable'],default='default',
                   help='Separate directory-contract lane for current/candidate; denominators remain unchanged')
    args = parser.parse_args()
    if args.action == 'freeze':
        freeze(args.repo.resolve(),args.output.resolve())
    else:
        measure(args)


if __name__ == '__main__':
    main()
