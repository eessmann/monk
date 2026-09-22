"""An otherwise successful command cannot hide changes to verification inputs."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

COLLECTOR = Path(__file__).with_name('verification-evidence.py').resolve()


class ReceiptTests(unittest.TestCase):
    def collect(self, root, body):
        result = subprocess.run([sys.executable, str(COLLECTOR), '--output', str(root / 'receipt'),
                                 '--', sys.executable, '-c', body], cwd=root, capture_output=True)
        receipt = json.loads((root / 'receipt/receipt.json').read_text())
        return result, receipt

    def test_changed_test_native_test_benchmark_or_collector_invalidates_receipt(self):
        for name in ('test/Spec.hs', 'runtime-test/probe.py', 'benchmark/Main.hs', 'scripts/collector.py'):
            with self.subTest(path=name), tempfile.TemporaryDirectory() as directory:
                root = Path(directory)
                path = root / name
                path.parent.mkdir(parents=True)
                path.write_text('before')
                result, receipt = self.collect(root, f'from pathlib import Path; Path({name!r}).write_text("after")')
                self.assertEqual(result.returncode, 3)
                self.assertEqual(receipt['exit'], 0)
                self.assertFalse(receipt['successful_stable_command'])
                self.assertFalse(receipt['build_inputs_unchanged'])
                self.assertTrue(receipt['production_inputs_unchanged'])

    def test_production_mutation_also_changes_product_fingerprint(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'src').mkdir()
            (root / 'src/Core.hs').write_text('before')
            result, receipt = self.collect(root, 'from pathlib import Path; Path("src/Core.hs").write_text("after")')
            self.assertEqual(result.returncode, 3)
            self.assertFalse(receipt['production_inputs_unchanged'])

    def test_added_and_removed_inputs_are_detected(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'test').mkdir()
            (root / 'test/old.stdin').write_bytes(b'old')
            result, receipt = self.collect(root, 'from pathlib import Path; Path("test/old.stdin").unlink(); Path("test/new.stdin").write_bytes(b"new")')
            self.assertEqual(result.returncode, 3)
            self.assertFalse(receipt['successful_stable_command'])

    def test_generated_caches_are_not_source_changes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            result, receipt = self.collect(root, 'from pathlib import Path; p=Path("scripts/__pycache__/module.pyc"); p.parent.mkdir(parents=True); p.write_bytes(b"cache"); p=Path("test/dist-local/Core.hi"); p.parent.mkdir(parents=True); p.write_bytes(b"interface")')
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertTrue(receipt['successful_stable_command'])

    def test_failed_build_still_records_missing_binary(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            result = subprocess.run([sys.executable, str(COLLECTOR), '--output', str(root/'receipt'),
                                     '--binary', str(root/'missing'), '--', sys.executable, '-c', 'raise SystemExit(7)'],
                                    cwd=root, capture_output=True)
            receipt = json.loads((root/'receipt/receipt.json').read_text())
            self.assertEqual(result.returncode, 7)
            self.assertEqual(receipt['exit'], 7)
            self.assertFalse(receipt['successful_stable_command'])
            self.assertFalse(receipt['requested_binaries_present'])
            self.assertTrue(receipt['binaries'][0]['missing'])

    def test_document_mutation_changes_command_inputs_but_not_product(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / 'docs').mkdir()
            (root / 'docs/input.md').write_text('before')
            result, receipt = self.collect(root, 'from pathlib import Path; Path("docs/input.md").write_text("after")')
            self.assertEqual(result.returncode, 3)
            self.assertFalse(receipt['build_inputs_unchanged'])
            self.assertTrue(receipt['production_inputs_unchanged'])


if __name__ == '__main__':
    unittest.main()
