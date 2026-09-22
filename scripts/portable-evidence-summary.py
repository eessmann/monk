#!/usr/bin/env python3
"""Publish a compact receipt; full byte observations remain in the local report."""
import argparse
import hashlib
import json
from pathlib import Path


def compact(value):
    if isinstance(value, dict):
        return {key: compact(item) for key,item in value.items() if key not in ('base64','observations')}
    if isinstance(value, list):
        return [compact(item) for item in value]
    return value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('input',type=Path)
    parser.add_argument('output',type=Path)
    args = parser.parse_args()
    raw = args.input.read_bytes()
    report = compact(json.loads(raw))
    report['raw_report'] = {'path':str(args.input.resolve()),'sha256':hashlib.sha256(raw).hexdigest(),
                            'scope':'Complete raw streams and per-sample observations are retained locally; this summary preserves hashes, outcomes and sample timings.'}
    with args.output.open('x') as output:
        output.write(json.dumps(report,indent=2)+'\n')


if __name__ == '__main__':
    main()
