"""Raw transport controls for the performance evidence collector."""
import base64
import importlib.util
import os
from pathlib import Path
import sys
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    'native_runtime_evidence', Path(__file__).with_name('native-runtime-evidence.py'))
EVIDENCE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(EVIDENCE)


class EvidenceTests(unittest.TestCase):
    def test_execution_preserves_cwd_argv_stdin_raw_streams_and_status(self):
        with tempfile.TemporaryDirectory() as directory:
            program = (
                'import os,sys; '
                'sys.stdout.buffer.write(os.fsencode(os.getcwd()) + b"\\0" + '
                'os.fsencode(sys.argv[1]) + b"\\0" + sys.stdin.buffer.read()); '
                'sys.stderr.buffer.write(b"\\xff\\0\\n"); sys.exit(7)')
            result = EVIDENCE.execute([sys.executable, '-c', program, ' spaced arg '],
                                      directory, b'input\0\xff\n\n', os.environ.copy())
            self.assertEqual(base64.b64decode(result['stdout']),
                             os.fsencode(directory) + b'\0 spaced arg \0input\0\xff\n\n')
            self.assertEqual(base64.b64decode(result['stderr']), b'\xff\0\n')
            self.assertEqual(result['exit'], 7)
            self.assertGreater(result['elapsed_ns'], 0)

    def test_elapsed_time_does_not_hide_stream_or_status_difference(self):
        baseline = {'stdout': 'YQ==', 'stderr': '', 'exit': 0, 'elapsed_ns': 1}
        self.assertTrue(EVIDENCE.same(baseline, dict(baseline, elapsed_ns=100)))
        for field, value in [('stdout', 'Yg=='), ('stderr', 'YQ=='), ('exit', 7)]:
            self.assertFalse(EVIDENCE.same(baseline, dict(baseline, **{field: value})))


if __name__ == '__main__':
    unittest.main()
