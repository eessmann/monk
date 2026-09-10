#!/usr/bin/env python3
"""Trace actual process creation/exec events separately from runtime timings."""
import argparse
import base64
import importlib.util
import json
from pathlib import Path
import re

SPEC = importlib.util.spec_from_file_location('evidence', Path(__file__).with_name('native-runtime-evidence.py'))
E = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(E)


def parse_trace(text):
    pending = {}
    events = []
    creations = []
    for line in text.splitlines():
        match = re.match(r'\s*(\d+)\s+(.*)', line)
        if not match:
            continue
        pid, call = match.groups()
        if '<unfinished ...>' in call:
            pending[pid] = call.replace('<unfinished ...>', '')
            continue
        resumed = re.match(r'<\.\.\. (\w+) resumed>(.*)', call)
        if resumed:
            call = pending.pop(pid, '') + resumed[2]
        executed = re.match(r'(execve|execveat)\((.*)\)\s+= 0$', call)
        if executed:
            path = re.search(r'"((?:[^"\\]|\\.)*)"', executed[2])
            events.append({'pid': int(pid), 'syscall': executed[1],
                           'executable': json.loads('"' + path[1] + '"') if path else None})
        created = re.match(r'(clone3?|fork|vfork)\(.*\)\s+= (\d+)$', call)
        if created and int(created[2]) > 0 and 'CLONE_THREAD' not in call:
            creations.append({'parent_pid': int(pid), 'child_pid': int(created[2]), 'syscall': created[1]})
    return {'successful_execs': events, 'process_creations': creations,
            'successful_exec_count_including_entry_shell': len(events),
            'child_process_creation_count_excluding_threads': len(creations)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--fish', required=True, type=Path)
    parser.add_argument('--variant', required=True, choices=['baseline', 'candidate'])
    parser.add_argument('--candidate-dir', type=Path)
    args = parser.parse_args()
    out = args.out.resolve()
    cohort = json.loads((out / 'baseline-cohort.json').read_text())
    cwd = Path(cohort['cwd'])
    env = E.environment(out)
    trace_dir = out / ('process-traces-' + args.variant)
    trace_dir.mkdir()
    rows = []
    for row in cohort['fixtures']:
        if row['fixture'] not in cohort['common16'] and not row.get('extra_arithmetic'):
            continue
        index = row['index']
        if args.variant == 'baseline':
            generated = out / 'baseline' / f'{index}.fish'
        else:
            generated = args.candidate_dir.resolve() / f'{index}-default.fish'
        log = trace_dir / f'{index}.strace'
        command = ['strace', '-f', '-qq', '-s', '4096', '-e', 'trace=process', '-o', str(log),
                   str(args.fish.resolve()), '--no-config', str(generated), *row['metadata']['fixtureMetaArgs']]
        result = E.execute(command, cwd, base64.b64decode(row['stdin_base64']), env)
        events = parse_trace(log.read_text())
        rows.append({'fixture': row['fixture'], 'generated_sha256': E.sha(generated),
                     'command': command, 'execution': result, **events})
        (trace_dir / 'report.json').write_text(json.dumps(rows, indent=2) + '\n')
    print(json.dumps({'variant': args.variant, 'fixtures': len(rows),
                      'successful_execs': sum(r['successful_exec_count_including_entry_shell'] for r in rows),
                      'child_process_creations': sum(r['child_process_creation_count_excluding_threads'] for r in rows)}, indent=2))


if __name__ == '__main__':
    main()
