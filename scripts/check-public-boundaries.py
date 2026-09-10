#!/usr/bin/env python3
"""Run under `cabal exec -- python3 scripts/check-public-boundaries.py`.

A positive consumer must compile first; each negative case must fail for its
specific abstraction boundary, not a missing package, compiler or dependency.
Compiler outputs and the final result can be archived with --report FILE.
"""
import argparse
import json
from pathlib import Path
import re
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ghc", default="ghc")
    parser.add_argument("--report", type=Path)
    parser.add_argument("--plan", type=Path, default=Path("dist-newstyle/cache/plan.json"))
    args = parser.parse_args()
    source = Path(__file__).resolve().parent.parent / "test" / "compile-fail"
    cases = {
        "ForgeResult": r"(?:not in scope:.*MkTranslationResult|MkTranslationResult.*not in scope)",
        "UpdateResult": r"(?:translationScript.{0,30}is not a record selector|not in scope: record field.{0,10}translationScript)",
        "ForgeGraph": r"(?:not in scope:.*MkSourceGraph|MkSourceGraph.*not in scope)",
        "UpdateGraph": r"(?:sourceRoot.{0,30}is not a record selector|not in scope: record field.{0,10}sourceRoot)",
        "ForgeBundle": r"(?:not in scope:.*MkOutputBundle|MkOutputBundle.*not in scope)",
        "UpdateBundle": r"(?:bundleRuntimeArtifacts.{0,30}is not a record selector|not in scope: record field.{0,10}bundleRuntimeArtifacts)",
        "UpdateRuntimeImage": r"(?:nativeImageBytes.{0,30}is not a record selector|not in scope: record field.{0,10}nativeImageBytes)",
        "ForgeRuntimeImage": r"(?:not in scope:.*MkNativeRuntimeImage|MkNativeRuntimeImage.*not in scope)",
        "ForgeRuntimeArtifact": r"(?:not in scope:.*MkNativeRuntimeArtifact|MkNativeRuntimeArtifact.*not in scope)",
        "PrivatePlan": r"Could not load module.{0,10}Language.Bash.Plan.{0,50}hidden module",
        "PrivatePublisher": r"Could not load module.{0,10}Monk.Output.Publication.{0,50}hidden module",
    }
    plan = json.loads(args.plan.read_text())
    main_units = [unit["id"] for unit in plan["install-plan"]
                  if unit["pkg-name"] == "monk" and unit.get("component-name") == "lib"]
    if len(main_units) != 1:
        raise SystemExit("Expected exactly one built main Monk library in Cabal plan")
    package_id = main_units[0]
    results = []
    with tempfile.TemporaryDirectory(prefix="monk-boundaries-") as scratch:
        command = [args.ghc, "-v0", "-fno-code", "-fforce-recomp", "-package-id", package_id, "-outputdir", scratch]
        for name, expected in [("Positive", None), *cases.items()]:
            run = subprocess.run([*command, str(source / (name + ".hs"))], text=True, capture_output=True, timeout=60)
            diagnostics = run.stdout + run.stderr
            passed = run.returncode == 0 if expected is None else run.returncode != 0 and re.search(expected, diagnostics, re.S | re.I) is not None
            results.append({"case": name, "exit": run.returncode, "passed": passed, "diagnostics": diagnostics})
            print(("PASS " if passed else "FAIL ") + name)
            if expected is None and not passed:
                break
    report = {"compiler": subprocess.check_output([args.ghc, "--numeric-version"], text=True).strip(), "package_id": package_id, "results": results}
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(json.dumps(report, indent=2) + "\n")
    if len(results) != len(cases) + 1 or not all(item["passed"] for item in results):
        for item in results:
            if not item["passed"]:
                print(item["diagnostics"])
        raise SystemExit(1)


if __name__ == "__main__":
    main()
