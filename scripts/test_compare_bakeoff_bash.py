"""Behavioral checks for the byte-preserving Bash comparison companion."""
import importlib.util
import os
from pathlib import Path
import tempfile
import unittest


class ComparisonTests(unittest.TestCase):
    def setUp(self):
        path = Path(__file__).with_name("compare-bakeoff-bash.py")
        self.assertTrue(path.exists(), "The bake-off needs a direct Bash comparator")
        spec = importlib.util.spec_from_file_location("compare_bakeoff_bash", path)
        self.comparison = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.comparison)

    def test_matching_translators_can_both_disagree_with_bash(self):
        baseline = {"status": "completed", "exit": 0, "stdout": b"right\n", "stderr": b""}
        wrong = {**baseline, "stdout": b"wrong\n"}
        for _tool in ("monk", "babelfish"):
            self.assertEqual(self.comparison.compare(baseline, wrong),
                             {"status": "mismatch", "differences": ["stdout"]})

    def test_nonzero_matching_exit_is_a_match_and_timeout_is_unavailable(self):
        baseline = {"status": "completed", "exit": 7, "stdout": b"", "stderr": b"error\n"}
        self.assertEqual(self.comparison.compare(baseline, baseline)["status"], "match")
        self.assertEqual(self.comparison.compare(baseline, {"status": "timeout"})["status"], "unavailable")

    def test_raw_bytes_empty_arguments_and_stdin_survive(self):
        with tempfile.TemporaryDirectory() as scratch:
            result = self.comparison.run(
                ["bash", "--noprofile", "--norc", "-c",
                 "printf '<%s>' \"$@\"; cat; printf '\\377\\000' >&2; exit 7", "fixture", "", "a b"],
                b"\x00\xff", Path(scratch), dict(os.environ), 2)
        self.assertEqual(result["stdout"], b"<><a b>\x00\xff")
        self.assertEqual(result["stderr"], b"\xff\x00")
        self.assertEqual(result["exit"], 7)

    def test_timeout_is_recorded_and_child_process_group_is_stopped(self):
        with tempfile.TemporaryDirectory() as scratch:
            result = self.comparison.run(["bash", "-c", "sleep 30 & wait"], b"",
                                         Path(scratch), dict(os.environ), 0.05)
        self.assertEqual(result["status"], "timeout")


if __name__ == "__main__":
    unittest.main()
