#!/usr/bin/env python3
"""Frozen-cohort native runtime evidence. Run freeze before candidate edits;
run measure only after a verified candidate tree is frozen. Trusted fixtures only.
Python orchestrates measurements; generated Fish never depends on this program.
"""
import argparse
import base64
import hashlib
import json
import os
from pathlib import Path
import platform
import statistics
import signal
import subprocess
import time


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def execute(command, cwd, stdin, env):
    start = time.perf_counter_ns()
    with subprocess.Popen(command, cwd=cwd, stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                          env=env, start_new_session=True) as process:
        try:
            stdout, stderr = process.communicate(stdin, timeout=60)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.communicate()
            raise RuntimeError(f"Measurement timed out: {command!r}")
        return {'elapsed_ns': time.perf_counter_ns() - start, 'exit': process.returncode,
                'stdout': base64.b64encode(stdout).decode(),
                'stderr': base64.b64encode(stderr).decode()}



def same(a, b):
    return all(a[k] == b[k] for k in ('exit', 'stdout', 'stderr'))


def environment(out):
    env = dict(os.environ, LC_ALL='C', LANG='C', XDG_CONFIG_HOME=str(out / 'config'))
    for key in ('BASH_ENV', 'ENV', 'SHELLOPTS', 'BASHOPTS', 'CDPATH'):
        env.pop(key, None)
    return env


def translation(binary, fixture, output, cwd, env, extra=()):
    command = [str(binary), str(cwd / fixture['fixture'])]
    if fixture['metadata']['fixtureMetaRecursive']:
        command += ['--recursive', '--sources', 'inline']
    command += list(extra)
    result = subprocess.run(command, cwd=cwd, env=env, capture_output=True, timeout=60)
    output.write_bytes(result.stdout)
    output.with_suffix('.diagnostics').write_bytes(result.stderr)
    return {'exit': result.returncode, 'bytes': len(result.stdout), 'sha256': sha(output),
            'command': command}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action', choices=['freeze', 'coverage', 'measure'])
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--baseline', required=True, type=Path)
    parser.add_argument('--candidate', type=Path)
    parser.add_argument('--runtime', type=Path)
    parser.add_argument('--bash', required=True, type=Path)
    parser.add_argument('--fish', required=True, type=Path)
    parser.add_argument('--source-fingerprint', help='Verified candidate tree fingerprint (required for measure)')
    args = parser.parse_args()
    cwd = Path.cwd().resolve()
    out = args.out.resolve()
    env = environment(out)
    previous = json.loads((cwd / 'docs/evidence/bakeoff-2026-09-09.json').read_text())
    if args.action == 'freeze':
        frozen = out / 'baseline'
        frozen.mkdir()  # Never overwrite the frozen cohort.
        rows = []
        arithmetic = json.loads((out / 'frozen-arithmetic.json').read_text())['cases']
        fixtures = previous['fixtures'] + [
            {'fixture': case['input'], 'metadata': {'fixtureMetaArgs': case['argv'],
             'fixtureMetaRecursive': False, 'fixtureMetaMode': 'ShellRunExec'},
             'input_sha256': case['input_sha256'], 'extra_arithmetic': True}
            for case in arithmetic]
        for index, fixture in enumerate(fixtures):
            assert sha(cwd / fixture['fixture']) == fixture['input_sha256']
            path = cwd / fixture['fixture']
            stdin = path.with_suffix('.stdin')
            row = dict(fixture, index=index, stdin_base64=base64.b64encode(
                stdin.read_bytes() if stdin.exists() else b'').decode())
            row['baseline_translation'] = translation(args.baseline.resolve(), row,
                                                       frozen / f'{index}.fish', cwd, env)
            rows.append(row)
        manifest = {'cwd': str(cwd), 'baseline_binary_sha256': sha(args.baseline),
                    'bash_sha256': sha(args.bash), 'fish_sha256': sha(args.fish),
                    'historic_count': 95, 'common16': [r['fixture'] for r in previous['fixtures']
                    if all(r['tools'][t]['status'] == 'match' for t in ('monk', 'babelfish'))],
                    'fixtures': rows}
        (out / 'baseline-cohort.json').write_text(json.dumps(manifest, indent=2) + '\n')
        print('Frozen', len(rows), 'fixtures; historic denominator 95')
        return
    if not all((args.candidate, args.runtime, args.source_fingerprint)):
        parser.error('measure requires candidate, runtime, and verified source-fingerprint')
    frozen = json.loads((out / 'baseline-cohort.json').read_text())
    assert sha(args.baseline) == frozen['baseline_binary_sha256']
    assert sha(args.bash) == frozen['bash_sha256'] and sha(args.fish) == frozen['fish_sha256']
    measured = out / ('coverage' if args.action == 'coverage' else 'measurement')
    measured.mkdir()
    rows = []
    for row in frozen['fixtures']:
        index = row['index']
        assert sha(cwd / row['fixture']) == row['input_sha256']
        baseline_path = out / 'baseline' / f'{index}.fish'
        assert sha(baseline_path) == row['baseline_translation']['sha256']
        stdin = base64.b64decode(row['stdin_base64'])
        argv = row['metadata']['fixtureMetaArgs']
        result = {'fixture': row['fixture'], 'historic': index < 95, 'modes': {}}
        for mode in ('default', 'stable'):
            generated = measured / f'{index}-{mode}.fish'
            extra = ['--runtime', str(args.runtime.resolve())]
            if mode == 'stable':
                extra += ['--directory-contract', 'stable']
            trans = translation(args.candidate.resolve(), row, generated, cwd, env, extra)
            current = {'translation': trans}
            result['modes'][mode] = current
        admitted = [mode for mode, data in result['modes'].items() if data['translation']['exit'] == 0]
        bash = None
        if admitted:
            bash = execute([str(args.bash), '--noprofile', '--norc', str(cwd / row['fixture']), *argv], cwd, stdin, env)
            result['bash'] = bash
        for mode in admitted:
            current = result['modes'][mode]
            generated = measured / f'{index}-{mode}.fish'
            current['execution'] = execute([str(args.fish), '--no-config', str(generated), *argv], cwd, stdin, env)
            current['matched'] = same(bash, current['execution'])
        timed = row['fixture'] in frozen['common16'] or row.get('extra_arithmetic', False)
        if timed and args.action == 'measure':
            assert row['baseline_translation']['exit'] == 0
            assert result['modes']['default']['translation']['exit'] == 0
            commands = {'baseline': [str(args.fish), '--no-config', str(baseline_path), *argv],
                        'candidate': [str(args.fish), '--no-config', str(measured / f'{index}-default.fish'), *argv]}
            # First observed executions above are kept separately from warm samples.
            first = {'baseline': execute(commands['baseline'], cwd, stdin, env),
                     'candidate': result['modes']['default']['execution']}
            result['first_run'] = first
            assert all(same(bash, value) for value in first.values()), row['fixture']
            for _ in range(3):
                for command in commands.values():
                    assert same(bash, execute(command, cwd, stdin, env))
            samples = {'baseline': [], 'candidate': []}
            for sample in range(20):
                order = ('baseline', 'candidate') if sample % 2 == 0 else ('candidate', 'baseline')
                for name in order:
                    observation = execute(commands[name], cwd, stdin, env)
                    assert same(bash, observation), (row['fixture'], name, sample)
                    samples[name].append(observation['elapsed_ns'])
            result['samples_ns'] = samples
            result['median_ns'] = {name: statistics.median(values) for name, values in samples.items()}
        rows.append(result)
        (measured / 'partial.json').write_text(json.dumps(rows, indent=2) + '\n')
    if args.action == 'coverage':
        totals = {mode: {'admitted': sum(r['modes'][mode]['translation']['exit'] == 0 for r in rows if r['historic']),
                        'matched': sum(r['modes'][mode].get('matched', False) for r in rows if r['historic']),
                        'admitted_mismatches': sum(r['modes'][mode].get('matched') is False for r in rows if r['historic'])}
                  for mode in ('default', 'stable')}
        report = {'source_fingerprint': args.source_fingerprint, 'candidate_sha256': sha(args.candidate),
                  'runtime_sha256': sha(args.runtime), 'measurement_script_sha256': sha(__file__),
                  'note': 'Preliminary coverage executions under concurrent build load; elapsed values are not acceptance or first-run performance measurements.',
                  'totals': totals, 'fixtures': rows}
        (measured / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
        print(json.dumps(totals, indent=2))
        return
    groups = {'common16': [r for r in rows if r['fixture'] in frozen['common16']],
              'arithmetic': [r for r in rows if not r['historic']]}
    aggregates = {}
    for name, group in groups.items():
        totals = {variant: [sum(r['samples_ns'][variant][i] for r in group) for i in range(20)]
                  for variant in ('baseline', 'candidate')}
        medians = {variant: statistics.median(values) for variant, values in totals.items()}
        aggregates[name] = {'sample_totals_ns': totals, 'median_ns': medians,
                            'baseline_over_candidate': medians['baseline'] / medians['candidate']}
    totals = {mode: {'admitted': sum(r['modes'][mode]['translation']['exit'] == 0 for r in rows if r['historic']),
                    'matched': sum(r['modes'][mode].get('matched', False) for r in rows if r['historic']),
                    'admitted_mismatches': sum(r['modes'][mode].get('matched') is False for r in rows if r['historic'])}
              for mode in ('default', 'stable')}
    large = next(r for r in rows if r['fixture'] == 'benchmark/fixtures/large-exact.bash')
    old_large = next(r for r in frozen['fixtures'] if r['fixture'] == large['fixture'])
    byte_ratio = large['modes']['default']['translation']['bytes'] / old_large['baseline_translation']['bytes']
    report = {'source_fingerprint': args.source_fingerprint, 'cwd': str(cwd), 'platform': platform.platform(),
              'baseline_sha256': sha(args.baseline), 'candidate_sha256': sha(args.candidate),
              'runtime': {'sha256': sha(args.runtime), 'bytes': args.runtime.stat().st_size,
                          'abi': 1, 'describe': execute([str(args.runtime.resolve()), '--describe'], cwd, b'', env),
                          'link_dependencies': subprocess.run(['ldd', str(args.runtime)], capture_output=True, text=True).stdout},
              'protocol': {'warmups': 3, 'samples': 20, 'serial': True, 'alternating_order': True,
                           'first_run': 'initial idle-load performance observation before three warmups, after preliminary coverage; not OS cold-cache measurement'},
              'process_launches': {'status': 'unmeasured', 'reason': 'ptrace previously denied; static calls are not process launches'},
              'totals': totals, 'aggregates': aggregates, 'large_exact_fish_ratio': byte_ratio,
              'bundle_bytes': {'native_executable': args.runtime.stat().st_size,
                               'large_exact_fish': large['modes']['default']['translation']['bytes'],
                               'large_exact_plus_native': large['modes']['default']['translation']['bytes'] + args.runtime.stat().st_size},
              'gates': {'default_at_least_45': totals['default']['matched'] >= 45,
                        'stable_at_least_48': totals['stable']['matched'] >= 48,
                        'zero_admitted_mismatches': all(t['admitted_mismatches'] == 0 for t in totals.values()),
                        'retains_original_exact': all(
                            next(r for r in rows if r['fixture'] == old['fixture'])['modes'][mode].get('matched', False)
                            for old in frozen['fixtures'][:95] if old['tools']['monk']['status'] == 'match'
                            for mode in ('default', 'stable')),
                        'large_exact_at_most_25_percent': byte_ratio <= .25,
                        'arithmetic_at_least_2x': aggregates['arithmetic']['baseline_over_candidate'] >= 2,
                        'common16_at_most_10_percent_regression': aggregates['common16']['baseline_over_candidate'] >= 1 / 1.1},
              'fixtures': rows}
    (measured / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps({k: report[k] for k in ('totals', 'gates')}, indent=2))


if __name__ == '__main__':
    main()
