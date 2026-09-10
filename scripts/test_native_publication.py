#!/usr/bin/env python3
"""Black-box native entry/publication regressions; binaries are explicit inputs."""

import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


class NativePublication(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="monk-native-publication-")
        self.root = Path(self.temp.name)
        self.monk = os.environ["MONK_NATIVE_TEST_BINARY"]
        self.runtime = os.environ["MONK_NATIVE_RUNTIME"]
        self.fish = shutil.which("fish")
        self.env = {**os.environ, "LC_ALL": "C", "LANG": "C"}

    def tearDown(self):
        self.temp.cleanup()

    def translate(self, source, name="entry", *options):
        source_path = self.root / (name + ".bash")
        target = self.root / (name + ".fish")
        source_path.write_text(source)
        result = subprocess.run(
            [self.monk, str(source_path), "--strict", "--runtime", self.runtime,
             "-o", str(target), *options],
            capture_output=True, env=self.env, timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        return target

    def execute(self, target, *, env=None):
        return subprocess.run([self.fish, "--no-config", str(target)],
                              capture_output=True, env=env or self.env, timeout=20)

    def test_literal_output_has_no_runtime(self):
        target = self.translate("echo hello\n")
        output = target.read_bytes()
        self.assertNotIn(b"python", output)
        self.assertNotIn(b"monk-runtime", output)
        result = self.execute(target)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"hello\n", b""))

    def test_managed_runtime_executes_without_python_or_installed_provider(self):
        target = self.translate('x=21; echo "$x"\n', "entry", "--managed")
        binaries = list(self.root.glob(".entry.fish.monk/generations/*/bin/monk-runtime"))
        self.assertEqual(len(binaries), 1)
        self.assertEqual(binaries[0].stat().st_mode & 0o7777, 0o700)
        isolated = self.root / "path"
        isolated.mkdir()
        (isolated / "fish").symlink_to(self.fish)
        result = self.execute(target, env={**self.env, "PATH": str(isolated)})
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"21\n", b""))

    def test_missing_installed_runtime_prevents_body_effects(self):
        self.runtime = str(self.root / "missing-runtime")
        target = self.translate('x=value; printf side-effect; echo "$x"\n')
        result = self.execute(target)
        self.assertEqual(result.returncode, 125)
        self.assertEqual(result.stdout, b"")
        self.assertIn(b"monk.runtime:", result.stderr)

    def test_native_operations_ignore_ambient_ghc_runtime_flags(self):
        result = subprocess.run([self.runtime, "--abi", "1", "echo"],
                                input=b"hello\0", capture_output=True,
                                env={**self.env, "GHCRTS": "-s"}, timeout=20)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"hello\n", b""))

    def test_runtime_provider_path_preserves_terminal_newline(self):
        provider = self.root / "provider\n"
        shutil.copy2(self.runtime, provider)
        self.runtime = str(provider)
        target = self.translate('x=value; echo "$x"\n')
        result = self.execute(target)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"value\n", b""))

    def test_incompatible_installed_runtime_prevents_body_effects(self):
        provider = self.root / "incompatible-runtime"
        provider.write_text("#!/bin/sh\nprintf 'monk-runtime 2 bash53-i64-linux64\\necho\\n'\n")
        provider.chmod(0o700)
        self.runtime = str(provider)
        target = self.translate('printf side-effect; x=value; echo "$x"\n')
        result = self.execute(target)
        self.assertEqual((result.returncode, result.stdout), (125, b""))
        self.assertIn(b"monk.runtime:", result.stderr)

    def test_reuse_rejects_changed_bytes_without_executing_them(self):
        target = self.translate('x=value; echo "$x"\n', "entry", "--managed")
        old_loader = target.read_bytes()
        binary = next(self.root.glob(".entry.fish.monk/generations/*/bin/monk-runtime"))
        marker = self.root / "unexpected-execution"
        binary.write_text(f"#!/bin/sh\nprintf reached > '{marker}'\n"
                          "printf 'monk-runtime 1 bash53-i64-linux64\\necho\\n'\n")
        result = subprocess.run(
            [self.monk, str(self.root / "entry.bash"), "--strict", "--managed",
             "--runtime", self.runtime, "-o", str(target)], capture_output=True,
            env=self.env, timeout=20,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(target.read_bytes(), old_loader)
        self.assertFalse(marker.exists(), "publisher executed corrupted generation member")

    def test_reuse_rejects_executable_mode_corruption(self):
        source = 'x=21; echo "$x"\n'
        target = self.translate(source, "entry", "--managed")
        old_loader = target.read_bytes()
        binary = next(self.root.glob(".entry.fish.monk/generations/*/bin/monk-runtime"))
        binary.chmod(0o600)
        result = subprocess.run(
            [self.monk, str(self.root / "entry.bash"), "--strict", "--managed",
             "--runtime", self.runtime, "-o", str(target)], capture_output=True,
            env=self.env, timeout=20,
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn(b"mode differs", result.stderr)
        self.assertEqual(target.read_bytes(), old_loader)

    def test_exported_function_keeps_its_runtime_generation(self):
        contract = self.root / "caller.json"
        contract.write_text('{"version":1,"exportedFunctions":["f"],"ambientEffects":"none"}')
        options = ("--managed", "--entry", "sourceable", "--caller-contract", str(contract))
        first = self.translate('f() { local n=21; echo "$n"; }\n', "entry", *options)
        saved = self.root / "saved.fish"
        saved.write_bytes(first.read_bytes())
        second = self.translate('f() { local n=33; echo "$n"; }\n', "entry", *options)
        driver = self.root / "driver.fish"
        # This harness is Fish, so it may manipulate functions and the caller.
        driver.write_text(f"source '{saved}'\nfunctions --copy f old_f\nsource '{second}'\n"
                          "builtin cd /\nbuiltin set --global PATH\nold_f\nf\n")
        result = self.execute(driver)
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, b"21\n33\n", b""))


if __name__ == "__main__":
    unittest.main()
