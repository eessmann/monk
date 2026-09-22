#!/usr/bin/env python3
"""Compiled callbacks preserve execution-site diagnostics, status and argv."""
import os
import subprocess
import sys
import tempfile
from pathlib import Path

runtime, monk = map(os.path.abspath, sys.argv[1:3])
environment = dict(os.environ, LC_ALL="C", LANG="C")
for name in ("MONK_LAUNCH_ORIGINAL", "MONK_LAUNCH_WRAPPER"):
    environment.pop(name, None)
cases = [
    ("trap 'printf x' ERR\nfalse\n", ""),
    ("trap 'true\nprintf x' ERR\nfalse\n", ""),
    ("set -- first 'two words'\ntrap 'printf \"<%s>\" \"$@\"' ERR\nfalse\nfalse\n", ""),
    ("trap 'printf x' ERR\n. ./module.bash\n", "false\n"),
    (". ./module.bash\ntrue\n", "trap 'printf x' EXIT\n"),
    (". ./module.bash\n", "trap 'printf x' EXIT\nexit 2\n"),
    ("trap 'printf x' EXIT\n. ./module.bash\nf\n", "f() { exit 2; }\n"),
    ("trap 'printf x' EXIT\n(trap 'printf child' EXIT; true)\n", ""),
    ("true\ntrue\ntrap 'printf x' EXIT\ntrue\n", ""),
    ("trap 'printf err' ERR\ntrap 'printf exit' EXIT\nfalse\n", ""),
    ("trap 'false; printf x' ERR\nfalse\n", ""),
]
with tempfile.TemporaryDirectory() as temporary:
    root = Path(temporary).resolve()
    source = root / "root.bash"
    module = root / "module.bash"
    generated = root / "generated.fish"
    for index, (body, dependency) in enumerate(cases):
        source.write_text(body)
        module.write_text(dependency)
        translated = subprocess.run(
            [monk, str(source), "--strict", "--recursive", "--sources", "inline", "--runtime", runtime],
            cwd=root, env=environment, capture_output=True, timeout=30,
        )
        assert translated.returncode == 0, (index, translated.stderr)
        generated.write_bytes(translated.stdout)
        for mask in (7, 5, 1):
            def close():
                for fd in range(3):
                    if not mask & (1 << fd):
                        os.close(fd)
            observed = []
            for command in (["bash", "--noprofile", "--norc", str(source)],
                            [runtime, "--abi", "2", "launch", str(generated)]):
                result = subprocess.run(command, cwd=root, env=environment,
                                        capture_output=True, preexec_fn=close, timeout=15)
                observed.append((result.returncode, result.stdout, result.stderr))
            assert observed[0] == observed[1], (index, mask, observed)
print(f"{len(cases) * 3} compiled callback comparisons preserve source site, line offsets, status and argv")
