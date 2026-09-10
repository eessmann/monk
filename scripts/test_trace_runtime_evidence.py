"""Controls for interpreting observed strace process events, not source text."""
import importlib.util
from pathlib import Path
import unittest

SPEC = importlib.util.spec_from_file_location('trace_evidence', Path(__file__).with_name('trace-runtime-evidence.py'))
TRACE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TRACE)


class TraceTests(unittest.TestCase):
    def test_failed_execs_threads_and_resumed_process_creation(self):
        result = TRACE.parse_trace('''10 execve("/fish", ["/fish"], 0x0) = 0
10 clone(child_stack=NULL, flags=CLONE_VM|CLONE_VFORK|SIGCHLD <unfinished ...>
11 execve("/missing", [], 0x0) = -1 ENOENT (No such file or directory)
10 <... clone resumed>) = 11
11 execve("/runtime", ["/runtime"], 0x0 <unfinished ...>
11 <... execve resumed>) = 0
11 clone3({flags=CLONE_VM|CLONE_THREAD}, 88) = 12
''')
        self.assertEqual(result['successful_exec_count_including_entry_shell'], 2)
        self.assertEqual(result['child_process_creation_count_excluding_threads'], 1)
        self.assertEqual([row['executable'] for row in result['successful_execs']], ['/fish', '/runtime'])


if __name__ == '__main__':
    unittest.main()
