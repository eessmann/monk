#!/usr/bin/env python3
"""Freeze and measure local immutable providers; unavailable cohorts never pass.

Trusted fixtures only. Timing excludes filesystem reset, includes shell execution.
Every sample is independently checked against exact Bash. No historic timings are
reused. Process-launch evidence is separate and remains unverified by this tool.
"""
import argparse
import base64
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import shutil
import statistics
import time

from importlib.util import spec_from_file_location, module_from_spec


def load(name, filename):
    spec = spec_from_file_location(name, Path(__file__).with_name(filename))
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


C = load('portable_comparison', 'portable-comparison.py')


def aggregate(rows):
    missing = [r['fixture'] for r in rows if r.get('status') != 'measured']
    if missing or not rows:
        return {'status': 'unverified', 'unavailable': missing, 'expected': len(rows),
                'measured': len(rows) - len(missing)}
    totals = {name: [sum(r['samples_ns'][name][i] for r in rows) for i in range(20)]
              for name in ('baseline','candidate')}
    medians = {name: statistics.median(values) for name,values in totals.items()}
    ratio = medians['candidate'] / medians['baseline']
    return {'status': 'measured', 'sample_totals_ns': totals, 'median_ns': medians,
            'candidate_over_baseline': ratio, 'at_most_10_percent_regression': ratio <= 1.1,
            'lower_time': ratio < 1.0}


def freeze(args):
    original = json.loads((args.frozen / 'manifest.json').read_bytes())
    args.output.mkdir(parents=True)
    shutil.copytree(args.frozen / 'inputs', args.output / 'inputs')
    by_name = {r['fixture']: r for r in original['fixtures']}
    fixtures = [dict(by_name[name], performance_cohort='common16') for name in original['common16']]
    historical = json.loads(Path('docs/evidence/bakeoff-native-2026-09-10.json').read_bytes())
    for old in historical['fixtures']:
        if old['historic']:
            continue
        path = Path(old['fixture'])
        row = {'fixture': str(path), 'performance_cohort': 'arithmetic3', 'cohort': 'arithmetic',
               'metadata': old['metadata'], 'stdin_base64': old['stdin_base64'],
               'input_sha256': old['input_sha256']}
        if path.is_file() and C.sha(path.read_bytes()) == row['input_sha256']:
            dest = args.output / 'inputs' / path
            dest.parent.mkdir(parents=True,exist_ok=True)
            shutil.copy2(path,dest)
        else:
            row['unavailable_reason'] = 'Original frozen arithmetic input is absent locally; hash retained, no substitute generated.'
        fixtures.append(row)
    # New targeted cohort is separately frozen, never added to historic95/common16.
    targeted = {
        'field-splitting': 'x="one two three"\nprintf "<%s>\\n" $x\n',
        'escaped-echo': 'echo -e "a\\tb\\nend"\n',
        'composed-words': 'x="one two"\nprintf "<%s>\\n" pre${x}post\n',
        'checked-printf': 'printf "%08d:%s\\n" 42 value\n',
    }
    for name,source in targeted.items():
        path = Path('performance') / (name + '.bash')
        dest = args.output / 'inputs' / path
        dest.parent.mkdir(exist_ok=True)
        dest.write_text(source)
        fixtures.append({'fixture': str(path), 'performance_cohort': 'targeted-native',
                         'cohort': 'targeted-native', 'metadata': {'fixtureMetaArgs': [], 'fixtureMetaRecursive': False},
                         'stdin_base64': '', 'input_sha256': C.sha(dest.read_bytes())})
    C.write_json(args.output / 'manifest.json', {'schema':1, 'fixtures':fixtures,
                 'comparison_manifest_sha256': C.sha((args.frozen / 'manifest.json').read_bytes()),
                 'historical_denominator':95, 'input_tree':C.snapshot(args.output / 'inputs')})


def measure(args):
    frozen = args.frozen.resolve()
    manifest = json.loads((frozen / 'manifest.json').read_bytes())
    inputs = frozen / 'inputs'
    if C.snapshot(inputs) != manifest['input_tree']:
        raise ValueError('Frozen inputs changed')
    receipt_bytes = args.build_receipt.read_bytes()
    receipt = json.loads(receipt_bytes)
    if receipt.get('schema') != 2 or not receipt.get('successful_stable_command'):
        raise ValueError('A successful command with stable build inputs is required')
    binaries = {str(Path(r['path']).resolve()): r['sha256'] for r in receipt['binaries']}
    for path in (args.candidate,args.runtime):
        if binaries.get(str(path.resolve())) != C.sha(path.read_bytes()):
            raise ValueError('Candidate/runtime not verified by build receipt: ' + str(path))
    output = args.output.resolve()
    output.mkdir(parents=True)
    providers = output / 'providers'
    providers.mkdir()
    supplied = {name: getattr(args,name).resolve(strict=True) for name in ('baseline','candidate','baseline_runtime','runtime','bash','fish')}
    for name,path in supplied.items():
        shutil.copy2(path, providers / name)
    for name in ('candidate','runtime'):
        if C.sha((providers / name).read_bytes()) != binaries[str(supplied[name])]:
            raise ValueError('Binary changed while capturing provider: ' + name)
    env = dict(os.environ,LC_ALL='C',LANG='C',XDG_CONFIG_HOME=str(output / 'config'))
    env['PATH'] = str(supplied['bash'].parent) + os.pathsep + str(supplied['fish'].parent) + os.pathsep + env.get('PATH','')
    for key in ('BASH_ENV','ENV','SHELLOPTS','BASHOPTS','CDPATH'):
        env.pop(key,None)
    cwd = output / 'execution-cwd'
    report = {'schema':1,'timestamp':datetime.now(timezone.utc).isoformat(),
              'build_receipt_sha256':C.sha(receipt_bytes),'source_fingerprint':receipt['production_after']['sha256'],
              'verification_input_fingerprint':receipt['after']['sha256'],
              'manifest_sha256':C.sha((frozen / 'manifest.json').read_bytes()),
              'providers':{name:C.provenance(providers / name) for name in supplied},
              'candidate_entry':'runtime --abi 2 launch SCRIPT ARGS',
              'protocol':{'warmups':3,'samples':20,'alternating_order':True,'serial':True},
              'process_launches':{'status':'unverified','reason':'No process tracing collected; static call counts are not launches.'},
              'fixtures':[]}
    baseline_probe = C.COMPARE.run([str(providers / 'baseline_runtime'),'--describe'],b'',inputs,env,30)
    report['baseline_probe'] = C.COMPARE.record(baseline_probe,output,'baseline-probe',[str(providers / 'baseline_runtime'),'--describe'])
    for index,fixture in enumerate(manifest['fixtures']):
        row = {'fixture':fixture['fixture'],'cohort':fixture['performance_cohort'],'input_sha256':fixture['input_sha256']}
        report['fixtures'].append(row)
        if fixture.get('unavailable_reason'):
            row.update(status='missing-frozen-input',reason=fixture['unavailable_reason'])
            continue
        if C.sha((inputs / fixture['fixture']).read_bytes()) != fixture['input_sha256']:
            raise ValueError('Frozen fixture hash differs: ' + fixture['fixture'])
        directory = output / f'{index:03d}'
        directory.mkdir()
        reference,command,_ = C.execute(providers / 'bash',fixture,inputs / fixture['fixture'],inputs,cwd,env,30)
        row['reference'] = C.COMPARE.record(reference,directory,'bash',command)
        commands = {}
        row['first_run'] = {}
        for name in ('baseline','candidate'):
            generated = directory / (name + '.fish')
            runtime = providers / ('baseline_runtime' if name == 'baseline' else 'runtime')
            trans,command = C.translate('current' if name == 'baseline' else 'candidate',providers / name,runtime,fixture,inputs,generated,env,30)
            row[name + '_translation'] = C.COMPARE.record(trans,directory,name + '-translation',command)
            if trans['status'] != 'completed' or trans['exit'] != 0:
                row.update(status='translation-unavailable',provider=name)
                break
            observed,command,_ = C.execute(providers / 'fish',fixture,generated,inputs,cwd,env,30,fish=True,launcher=runtime if name == 'candidate' else None)
            row['first_run'][name] = C.COMPARE.record(observed,directory,name + '-first',command)
            if name == 'baseline' and b'requires 64-bit Linux' in baseline_probe['stderr'] and b'missing or incompatible native runtime' in observed['stderr']:
                row.update(status='unsupported-native-platform',reason='Unchanged baseline requires Linux; user deferred Linux execution.')
                break
            comparison = C.COMPARE.compare(reference,observed)
            if comparison['status'] != 'match':
                row.update(status='execution-' + comparison['status'],provider=name,differences=comparison['differences'])
                break
            commands[name] = command
        else:
            samples = {'baseline':[],'candidate':[]}
            row['observations'] = []
            valid = True
            for iteration in range(23):
                order = ('baseline','candidate') if iteration < 3 or (iteration-3) % 2 == 0 else ('candidate','baseline')
                for name in order:
                    # Every sample uses the same fresh cwd, outside the timed interval.
                    shutil.rmtree(cwd)
                    shutil.copytree(inputs,cwd)
                    started = time.perf_counter_ns()
                    observed = C.COMPARE.run(commands[name],base64.b64decode(fixture['stdin_base64']),cwd,env,30)
                    elapsed = time.perf_counter_ns() - started
                    saved = C.COMPARE.record(observed,directory,f'{name}-{iteration:02d}',commands[name])
                    saved.update(provider=name,warmup=iteration < 3,elapsed_ns=elapsed)
                    row['observations'].append(saved)
                    if C.COMPARE.compare(reference,observed)['status'] != 'match':
                        valid = False
                        break
                    if iteration >= 3:
                        samples[name].append(elapsed)
                if not valid:
                    break
            row.update(status='measured' if valid else 'sample-mismatch',samples_ns=samples)
        C.write_json(output / 'partial.json',report)
    report['cohorts'] = {name:aggregate([r for r in report['fixtures'] if r['cohort'] == name]) for name in ('common16','arithmetic3','targeted-native')}
    report['acceptance'] = {'status':'unverified','reason':'Timing cohorts must all be complete and targeted process-launch reduction must be measured.'}
    C.write_json(output / 'report.json',report)
    print(json.dumps(report['cohorts'],indent=2))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action',choices=('freeze','measure'))
    parser.add_argument('--frozen',required=True,type=Path)
    parser.add_argument('--output',required=True,type=Path)
    for name in ('baseline','candidate','baseline-runtime','runtime','bash','fish','build-receipt'):
        parser.add_argument('--'+name,type=Path)
    args = parser.parse_args()
    if args.action == 'freeze':
        freeze(args)
    else:
        if not all(getattr(args,n) for n in ('baseline','candidate','baseline_runtime','runtime','bash','fish','build_receipt')):
            parser.error('measure requires all provider paths and --build-receipt')
        measure(args)


if __name__ == '__main__':
    main()
