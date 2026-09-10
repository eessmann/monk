#!/usr/bin/env python3
"""Check reviewed admissions, rejection diagnostics, and rendered Fish syntax.

This is an admission/syntax manifest. Runtime differential results are separate;
acceptance here never turns a syntax check into evidence of equivalence.
"""
import csv
import hashlib
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def main():
    if len(sys.argv) != 3:
        raise SystemExit("usage: generate-parity-manifest.py MONK_BIN OUTPUT")
    monk = str(Path(sys.argv[1]).resolve())
    destination = Path(sys.argv[2])
    destination.parent.mkdir(parents=True, exist_ok=True)
    with Path("test/fixtures/admission.tsv").open() as source:
        policies = list(csv.DictReader(source, delimiter="\t"))
    actual_paths = sorted(str(path) for path in Path("test/fixtures").rglob("*.bash"))
    if sorted(row["fixture"] for row in policies) != actual_paths:
        raise SystemExit("fixture policy inventory is missing, duplicated, or obsolete")
    failures = []
    with tempfile.TemporaryDirectory(prefix="parity-", dir=destination.parent) as scratch:
        with destination.open("w") as output:
            writer = csv.writer(output, delimiter="\t", lineterminator="\n")
            writer.writerow(["fixture", "expected", "actual", "fish_syntax", "rendered_sha256", "fish_bytes", "diagnostic_codes", "requirements", "verified", "rationale"])
            for index, policy in enumerate(policies):
                fixture = Path(policy["fixture"])
                target = Path(scratch) / f"fixture-{index}.fish"
                command = [monk, str(fixture), "--strict", "--output", str(target)]
                if fixture.with_suffix(".recursive").exists():
                    command += ["--recursive", "--sources", "inline"]
                result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30)
                diagnostics = result.stderr.decode("utf-8", errors="replace")
                codes = sorted(set(re.findall(r"(?:error|warning|note)\[([^]]+)\]", diagnostics)))
                requirements = sorted(set(re.findall(r"runtime requirement: (.+)", diagnostics)))
                data = target.read_bytes() if target.exists() else b""
                accepted = result.returncode == 0 and target.is_file()
                syntax = accepted and subprocess.run(["fish", "--no-config", "--no-execute", str(target)], stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30).returncode == 0
                if policy["expected"] == "exact":
                    verified = accepted and syntax
                elif policy["expected"] == "reject":
                    verified = result.returncode != 0 and not target.exists() and any(code.startswith(policy["diagnostic_prefix"]) for code in codes)
                else:
                    raise SystemExit(f"unknown policy: {policy}")
                if not verified:
                    failures.append((str(fixture), diagnostics))
                writer.writerow([str(fixture), policy["expected"], "accepted" if accepted else "rejected", str(bool(syntax)).lower(), hashlib.sha256(data).hexdigest() if accepted else "", len(data), ",".join(codes), ";".join(requirements), str(verified).lower(), policy["rationale"]])
    for path, details in failures:
        print(f"unaccounted fixture result: {path}\n{details}", file=sys.stderr)
    print(f"{len(policies) - len(failures)}/{len(policies)} reviewed fixture admissions and syntax verified")
    return bool(failures)


if __name__ == "__main__":
    sys.exit(main())
