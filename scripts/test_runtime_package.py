#!/usr/bin/env python3
"""Regression checks for fail-closed release artifact inspection."""
import importlib.util
from pathlib import Path
import unittest
from unittest.mock import patch

spec = importlib.util.spec_from_file_location(
    "runtime_package", Path(__file__).with_name("verify-runtime-package.py")
)
package = importlib.util.module_from_spec(spec)
spec.loader.exec_module(package)


class RuntimePackageTests(unittest.TestCase):
    def setUp(self):
        mock = patch.object(Path, "read_bytes", return_value=b"signed-final-artifact")
        mock.start()
        self.addCleanup(mock.stop)

    def inspect_elf(self, headers="", dynamic="", architecture="AArch64"):
        with patch.object(package, "run", side_effect=[
            f"  Machine: {architecture}\n", headers, dynamic
        ]):
            return package.inspect(Path("runtime"), "aarch64-linux")

    def test_static_elf_does_not_claim_native_execution(self):
        report = self.inspect_elf()
        self.assertTrue(report["linkage_verified"])
        self.assertFalse(report["execution_verified"])

    def test_linux_interpreter_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "interpreter"):
            self.inspect_elf(headers="  INTERP 0x0000\n")

    def test_linux_needed_library_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "dynamic dependencies"):
            self.inspect_elf(dynamic="0x00001 (NEEDED) Shared library: [libc.so.6]")

    def test_wrong_architecture_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "architecture"):
            self.inspect_elf(architecture="Advanced Micro Devices X86-64")

    def test_darwin_only_accepts_apple_libraries(self):
        with patch.object(package, "run", side_effect=[
            "arm64\n", "runtime:\n\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0)\n",
            "Load command 8\n cmd LC_BUILD_VERSION\n platform 1\n minos 13.0\n sdk 26.0\n"
        ]):
            report = package.inspect(Path("runtime"), "aarch64-darwin")
            self.assertEqual(report["dynamic_dependencies"], ["/usr/lib/libSystem.B.dylib"])
            self.assertEqual(report["platform_requirements"]["minimum_version"], "13.0")

    def test_artifact_identity_and_linux_policy_are_explicit(self):
        import hashlib
        report = self.inspect_elf()
        self.assertEqual(report["sha256"], hashlib.sha256(b"signed-final-artifact").hexdigest())
        self.assertEqual(report["bytes"], len(b"signed-final-artifact"))
        self.assertEqual((report["abi"], report["profile"]), (2, "bash53-i64"))
        self.assertFalse(report["platform_requirements"]["minimum_execution_verified"])

    def test_darwin_requires_deployment_target(self):
        with patch.object(package, "run", side_effect=["arm64", "runtime:\n", ""]):
            with self.assertRaisesRegex(ValueError, "deployment target"):
                package.inspect(Path("runtime"), "aarch64-darwin")

    def test_darwin_system_directory_is_not_blanket_allowlist(self):
        with patch.object(package, "run", side_effect=["arm64", "runtime:\n\t/usr/lib/not-apple.dylib (version 1)\n"]):
            with self.assertRaisesRegex(ValueError, "non-Apple"):
                package.inspect(Path("runtime"), "aarch64-darwin")

    def test_executed_description_must_match_declared_contract(self):
        report = self.inspect_elf()
        package.verify_description(report, 'monk-runtime 2 bash53-i64\necho integer\ntarget aarch64-linux\n')
        self.assertTrue(report['runtime_description_verified'])
        self.assertFalse(report['execution_verified'])
        with self.assertRaisesRegex(ValueError, 'ABI/profile/target'):
            package.verify_description(report, 'monk-runtime 2 bash53-i64\necho\ntarget aarch64-darwin\n')

    def test_darwin_nix_dependency_is_rejected(self):
        with patch.object(package, "run", side_effect=[
            "arm64\n", "runtime:\n\t/nix/store/example/lib/libgmp.dylib (compatibility version 1.0.0)\n"
        ]):
            with self.assertRaisesRegex(ValueError, "non-Apple"):
                package.inspect(Path("runtime"), "aarch64-darwin")


if __name__ == "__main__":
    unittest.main()
