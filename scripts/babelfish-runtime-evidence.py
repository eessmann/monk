#!/usr/bin/env python3
"""Fresh Babelfish translations and independent Bash comparisons on frozen95."""
import argparse
import base64
import importlib.util
import json
from pathlib import Path
import statistics
import subprocess

SPEC = importlib.util.spec_from_file_location('evidence', Path(__file__).with_name('native-runtime-evidence.py'))
E = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(E)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--babelfish', required=True, type=Path)
    parser.add_argument('--bash', required=True, type=Path)
    parser.add_argument('--fish', required=True, type=Path)
    parser.add_argument('--time-common16', action='store_true')
    parser.add_argument('--run-name', default='babelfish-fresh')
    args = parser.parse_args()
    out = args.out.resolve()
    cohort = json.loads((out / 'baseline-cohort.json').read_text())
    cwd = Path(cohort['cwd'])
    env = E.environment(out)
    measured = out / args.run_name
    measured.mkdir()
    binary = args.babelfish.resolve()
    version = subprocess.run([str(binary), '--version'], capture_output=True)
    receipt = binary.parent.parent / 'INSTALL_RECEIPT.json'
    provenance = {'path': str(binary), 'sha256': E.sha(binary),
                  'version_probe': {'exit': version.returncode, 'stdout': version.stdout.decode(), 'stderr': version.stderr.decode()},
                  'homebrew_receipt': json.loads(receipt.read_text()) if receipt.exists() else None,
                  'homebrew_receipt_sha256': E.sha(receipt) if receipt.exists() else None,
                  'bash_sha256': E.sha(args.bash), 'fish_sha256': E.sha(args.fish)}
    rows = []
    for row in cohort['fixtures'][:95]:
        path = cwd / row['fixture']
        assert E.sha(path) == row['input_sha256']
        current = {'fixture': row['fixture'], 'input_sha256': row['input_sha256'], 'modes': {}}
        for mode in ('default', 'stable'):
            generated = measured / f"{row['index']}-{mode}.fish"
            translated = subprocess.run([str(binary)], input=path.read_bytes(), cwd=cwd, env=env, capture_output=True, timeout=60)
            generated.write_bytes(translated.stdout)
            generated.with_suffix('.diagnostics').write_bytes(translated.stderr)
            observation = {'translation_exit': translated.returncode, 'generated_bytes': len(translated.stdout),
                           'generated_sha256': E.sha(generated)}
            current['modes'][mode] = observation
            if translated.returncode != 0:
                continue
            stdin = base64.b64decode(row['stdin_base64'])
            argv = row['metadata']['fixtureMetaArgs']
            bash = E.execute([str(args.bash.resolve()), '--noprofile', '--norc', str(path), *argv], cwd, stdin, env)
            fish_command = [str(args.fish.resolve()), '--no-config', str(generated), *argv]
            fish = E.execute(fish_command, cwd, stdin, env)
            observation.update(bash=bash, fish=fish, matched=E.same(bash, fish))
            if args.time_common16 and mode == 'default' and row['fixture'] in cohort['common16']:
                assert observation['matched'], row['fixture']
                for _ in range(3):
                    assert E.same(bash, E.execute(fish_command, cwd, stdin, env))
                samples = []
                for _ in range(20):
                    result = E.execute(fish_command, cwd, stdin, env)
                    assert E.same(bash, result)
                    samples.append(result['elapsed_ns'])
                observation['samples_ns'] = samples
        rows.append(current)
        (measured / 'partial.json').write_text(json.dumps(rows, indent=2) + '\n')
    totals = {mode: {'admitted': sum(r['modes'][mode]['translation_exit'] == 0 for r in rows),
                    'matched': sum(r['modes'][mode].get('matched', False) for r in rows),
                    'admitted_mismatches': sum(r['modes'][mode].get('matched') is False for r in rows)}
              for mode in ('default', 'stable')}
    report = {'provenance': provenance, 'cwd': str(cwd), 'historic_denominator': 95,
              'contract_note': 'Babelfish has no directory contract selector; both labels are fresh independent reruns under the corresponding comparison scope, with empty CDPATH.',
              'totals': totals, 'fixtures': rows}
    if args.time_common16:
        timed = [r['modes']['default']['samples_ns'] for r in rows if r['fixture'] in cohort['common16']]
        aggregate = [sum(samples[i] for samples in timed) for i in range(20)]
        report['common16_timing'] = {'warmups': 3, 'samples': 20, 'sample_totals_ns': aggregate,
                                    'median_ns': statistics.median(aggregate),
                                    'protocol': 'serial Babelfish-only follow-up; separate from alternating baseline/candidate acceptance measurements'}
    (measured / 'report.json').write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(totals, indent=2))


if __name__ == '__main__':
    main()
