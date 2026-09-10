#!/usr/bin/env python3
"""Compare each successful bake-off translation directly with Bash.

Run after monk-bakeoff. This companion preserves raw bytes and uses the
standalone entry contract; it never treats translation rejection as a match.
Only stdout, stderr and exit status are compared. Filesystem and caller-state
equivalence remain outside this report. Run only trusted repository fixtures.
"""
import argparse
import base64
from collections import Counter
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess


def digest(data):
    return hashlib.sha256(data).hexdigest()


def run(command, stdin, cwd, env, timeout):
    with subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                          stderr=subprocess.PIPE, cwd=cwd, env=env,
                          start_new_session=True) as process:
        try:
            out, err = process.communicate(stdin, timeout=timeout)
            status = "completed"
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            out, err = process.communicate()
            status = "timeout"
    return {"status": status, "exit": process.returncode, "stdout": out, "stderr": err}


def compare(baseline, candidate):
    if baseline["status"] != "completed" or candidate["status"] != "completed":
        return {"status": "unavailable", "differences": []}
    differences = [key for key in ("stdout", "stderr", "exit") if baseline[key] != candidate[key]]
    return {"status": "mismatch" if differences else "match", "differences": differences}


def record(result, directory, shell, command):
    saved = {"command": command, "status": result["status"], "exit": result["exit"]}
    for stream in ("stdout", "stderr"):
        data = result[stream]
        (directory / (shell + "." + stream)).write_bytes(data)
        saved[stream] = {"bytes": len(data), "sha256": digest(data),
                         "base64": base64.b64encode(data).decode("ascii")}
    return saved


def executable(raw):
    path = shutil.which(raw)
    if path is None:
        raise SystemExit(f"Executable not found: {raw}")
    return str(Path(path).resolve())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run_dir", type=Path)
    parser.add_argument("--bash", default="bash")
    parser.add_argument("--fish", default="fish")
    parser.add_argument("--timeout", type=float, default=30)
    args = parser.parse_args()
    run_dir = args.run_dir.resolve()
    raw_report = (run_dir / "report.json").read_bytes()
    fixtures = json.loads(raw_report)
    meta = json.loads((run_dir / "meta.json").read_bytes())
    cwd = Path(meta["metaCwd"])
    for fixture in fixtures:
        if fixture["fixtureReportMetadata"]["fixtureMetaMode"] != "ShellRunExec":
            raise SystemExit("Rerun monk-bakeoff with the standalone selection fix first")
    out_dir = run_dir / "bash-comparison"
    out_dir.mkdir()  # Preserve earlier evidence; never silently replace a run.
    bash, fish = executable(args.bash), executable(args.fish)
    env = dict(os.environ, LC_ALL="C", LANG="C")
    for key in ("BASH_ENV", "ENV", "SHELLOPTS", "BASHOPTS", "CDPATH"):
        env.pop(key, None)
    # Avoid importing user Fish universal variables or writing user config.
    env["XDG_CONFIG_HOME"] = str(out_dir / "config")
    rows = []
    totals = {name: Counter() for name in ("monk", "babelfish")}
    for index, fixture in enumerate(fixtures):
        path = Path(fixture["fixtureReportPath"])
        row = {"fixture": fixture["fixtureReportRelativePath"],
               "group": fixture["fixtureReportGroup"], "input_sha256": digest(path.read_bytes()),
               "metadata": fixture["fixtureReportMetadata"], "tools": {}}
        translations = {name.lower(): fixture[f"fixtureReport{name}Translation"]
                        for name in ("Monk", "Babelfish")}
        admitted = {name: translation for name, translation in translations.items()
                    if translation and translation["translationStatus"] == "CommandSucceeded"}
        for name, translation in translations.items():
            if name not in admitted:
                status = translation["translationStatus"] if translation else "MissingReport"
                row["tools"][name] = {"status": status}
                totals[name][status] += 1
        if admitted:
            directory = out_dir / f"{index:03d}"
            directory.mkdir()
            stdin_path = path.with_suffix(".stdin")
            stdin = stdin_path.read_bytes() if stdin_path.exists() else b""
            argv = row["metadata"]["fixtureMetaArgs"]
            row["stdin_base64"] = base64.b64encode(stdin).decode("ascii")
            command = [bash, "--noprofile", "--norc", str(path), *argv]
            baseline = run(command, stdin, cwd, env, args.timeout)
            row["bash"] = record(baseline, directory, "bash", command)
            for name, translation in admitted.items():
                generated = Path(translation["translationOutputPath"])
                command = [fish, "--no-config", str(generated), *argv]
                result = run(command, stdin, cwd, env, args.timeout)
                compared = compare(baseline, result)
                counts = {key: translation[key] for key in
                          ("translationErrorCount", "translationWarningCount", "translationNotesCount")}
                row["tools"][name] = {**compared, "diagnostics": counts,
                                       "generated_sha256": digest(generated.read_bytes()),
                                       "execution": record(result, directory, name, command)}
                totals[name][compared["status"]] += 1
                if compared["status"] == "mismatch":
                    if not any(counts.values()):
                        totals[name]["zero_diagnostic_mismatches"] += 1
                    if counts["translationErrorCount"] == counts["translationWarningCount"] == 0:
                        totals[name]["zero_warning_error_mismatches"] += 1
        rows.append(row)
    report = {"schema": 1, "timestamp": datetime.now(timezone.utc).isoformat(),
              "scope": ["stdout bytes", "stderr bytes", "exit status"],
              "entry_mode": "standalone", "cwd": str(cwd), "timeout_seconds": args.timeout,
              "environment": {key: env.get(key) for key in ("LC_ALL", "LANG", "PATH", "XDG_CONFIG_HOME")},
              "bakeoff_report_sha256": digest(raw_report),
              "tools": {name: {"path": path, "sha256": digest(Path(path).read_bytes()),
                               "version": subprocess.check_output([path, "--version"], env=env).decode().splitlines()[0]}
                        for name, path in (("bash", bash), ("fish", fish))},
              "totals": totals, "fixtures": rows}
    (out_dir / "report.json").write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(totals, indent=2))
    # Findings are a successful measurement, not a failing test command.


if __name__ == "__main__":
    main()
