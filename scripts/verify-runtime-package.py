#!/usr/bin/env python3
"""Fail closed on runtime release architecture or dynamic dependency leakage."""
import argparse
import hashlib
import json
import platform
import re
import subprocess
from pathlib import Path

TARGETS = {
    "x86_64-linux": ("ELF", "Advanced Micro Devices X86-64"),
    "aarch64-linux": ("ELF", "AArch64"),
    "aarch64-darwin": ("Mach-O", "arm64"),
}


def run(*args):
    return subprocess.run(args, check=True, text=True, capture_output=True).stdout


def inspect(binary, target):
    kind, architecture = TARGETS[target]
    if kind == "ELF":
        header = run("readelf", "-h", str(binary))
        machine = re.search(r"^\s*Machine:\s*(.+)$", header, re.MULTILINE)
        if machine is None or machine.group(1).strip() != architecture:
            raise ValueError(f"unexpected ELF architecture: {machine}")
        program_headers = run("readelf", "-l", str(binary))
        dynamic = run("readelf", "-d", str(binary))
        if re.search(r"^\s*INTERP\s", program_headers, re.MULTILINE):
            raise ValueError("Linux release contains a dynamic interpreter")
        if "(NEEDED)" in dynamic:
            raise ValueError("Linux release contains dynamic dependencies")
        dependencies = []
        requirements = {"os": "Linux", "minimum_kernel": "5.4", "basis": "Declared conservative release floor for the pinned musl/GHC toolchain and POSIX spawn, poll, Unix sockets, descriptor passing, and /dev/fd; not inferred from ELF headers.", "minimum_execution_verified": False, "descriptor_paths": "Process substitution additionally requires usable /dev/fd paths; probed before effects."}
    else:
        architecture_output = run("lipo", "-archs", str(binary)).strip()
        if architecture_output != architecture:
            raise ValueError(f"unexpected Mach-O architecture: {architecture_output}")
        dependencies = []
        for line in run("otool", "-L", str(binary)).splitlines()[1:]:
            dependency = line.strip().split(" (", 1)[0]
            if dependency not in {"/usr/lib/libSystem.B.dylib", "/usr/lib/libffi.dylib", "/usr/lib/libiconv.2.dylib"}:
                raise ValueError(f"non-Apple dynamic dependency: {dependency}")
            dependencies.append(dependency)
        load_commands = run("otool", "-l", str(binary))
        build_version = re.search(r"cmd LC_BUILD_VERSION\b(?:(?!Load command).)*?\bplatform\s+(?:1|macos)\s+(?:(?!Load command).)*?\bminos\s+(\d+(?:\.\d+)+)", load_commands, re.DOTALL | re.IGNORECASE)
        legacy_version = re.search(r"cmd LC_VERSION_MIN_MACOSX\b(?:(?!Load command).)*?\bversion\s+(\d+(?:\.\d+)+)", load_commands, re.DOTALL)
        minimum = build_version or legacy_version
        if minimum is None:
            raise ValueError("Mach-O minimum macOS deployment target is unavailable")
        requirements = {"os": "macOS", "minimum_version": minimum.group(1), "basis": "Mach-O deployment-target load command", "minimum_execution_verified": False, "descriptor_paths": "Process substitution additionally requires usable /dev/fd paths; probed before effects."}
    data = binary.read_bytes()
    return {"target": target, "format": kind, "architecture": architecture,
            "dynamic_dependencies": dependencies, "linkage_verified": True,
            "execution_verified": False, "sha256": hashlib.sha256(data).hexdigest(),
            "bytes": len(data), "abi": 2, "profile": "bash53-i64",
            "runtime_description_verified": False, "platform_requirements": requirements}


def verify_description(report, description):
    lines = description.splitlines()
    expected = f"monk-runtime {report['abi']} {report['profile']}"
    if len(lines) != 3 or lines[0] != expected or lines[2] != 'target ' + report['target']:
        raise ValueError('executed runtime description does not match package ABI/profile/target')
    report['runtime_description_verified'] = True
    report['runtime_capabilities'] = lines[1].split()
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=Path)
    parser.add_argument("--target", choices=TARGETS, required=True)
    parser.add_argument("--report", type=Path)
    parser.add_argument("--description-file", type=Path, help="Previously executed --describe output; does not claim full execution verification")
    args = parser.parse_args()
    report = inspect(args.binary, args.target)
    if args.description_file:
        verify_description(report, args.description_file.read_text())
    report["inspection_host"] = platform.platform()
    encoded = json.dumps(report, indent=2) + "\n"
    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(encoded)
    print(encoded, end="")


if __name__ == "__main__":
    main()
